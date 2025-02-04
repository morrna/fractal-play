module Command.EncodingSpec exposing ( suite )

import Test as T
import Fuzz as F
import Expect as E
import Bytes.Encode as BE
import Bytes.Decode as BD

import Command.Encoding as CE
import Space.IterFrame as IterFrame
import Space
import Space.Frame as Frame
import Space.Content as Content
import Start
import Geometry as G
import Space.Shape as Shape
import Space.TreeID as ID

import Util

suite : T.Test
suite = T.describe "module Command.Encoding" [
        iterModeEncodeDecode
      , stateByteStringEncodeDecode
      , pointEncodeDecode
      , shapeEncodeDecode
      , contentListEncodeDecode
      , contentListOutsideLimitsEncodeDecode
    ]

iterModeEncodeDecode: T.Test
iterModeEncodeDecode = T.fuzz fuzzIterMode "Encode then decode iterMode"
    <| \iterMode -> checkDecode E.equal iterMode
        (BD.decode CE.decoderIterMode
            (BE.encode <| CE.encoderIterMode iterMode)
        )

fuzzIterMode : F.Fuzzer IterFrame.Mode
fuzzIterMode
    = let
        initMode = IterFrame.initMode
        makeIM depth showIterFrames onlyShowLastLayer
            = { initMode |
                depth = depth
              , showIterFrames = showIterFrames
              , onlyShowLastLayer = onlyShowLastLayer
              }
    in F.map3 makeIM
        (F.intRange 0 63)
        F.bool
        F.bool

{-| Check that a decoded value matches an expected value.
    If the decode fails, the test will fail with a message indicating that.
 -}
checkDecode
    : (a -> a -> E.Expectation)
   -> a -> Maybe a -> E.Expectation
checkDecode check expected decodedActual
    = Maybe.withDefault (E.fail "decode failed")
        <| Maybe.map (check expected) decodedActual

stateByteStringEncodeDecode : T.Test
stateByteStringEncodeDecode = T.fuzz fuzzState "Encode then decode stateByteString"
    <| \state -> expectStatesAgree state
        (CE.decodeStateByteString state <| CE.encodeStateByteString state)

fuzzState : F.Fuzzer Space.Model
fuzzState
    = F.map2
        (\iterMode whichStart -> Space.liftIterMode (always iterMode) (Start.get whichStart))
        fuzzIterMode
        fuzzWhichStart

fuzzWhichStart : F.Fuzzer Start.Which
fuzzWhichStart = F.oneOfValues [
        Start.Sierpinski
      , Start.Dragon
      , Start.SierpinskiCarpet
    ]

expectStatesAgree : Space.Model -> (Maybe String, Space.Model) -> E.Expectation
expectStatesAgree s1 (maybeError, s2)
    = E.all
        [
            E.equal s1.iterMode << .iterMode
          , expectContentsAgree s1.baseContents.present << .present << .baseContents
          , always <| E.equal maybeError Nothing
        ] s2

pointEncodeDecode : T.Test
pointEncodeDecode = T.fuzz fuzzPoint "Encode then decode point"
    <| \point -> checkDecode expectPointsClose point
        (BD.decode CE.decoderPoint <| BE.encode <| CE.encoderPoint point)

{-| Making a fuzzer specific to the module to match how the encoding works. -}
fuzzPoint : F.Fuzzer G.Point
fuzzPoint
    = let
        -- Don't test right up to the edge because of maxint rollover.
        maxCoord = 1.9 * max
            (Frame.width Space.outerFrame) (Frame.height Space.outerFrame)
    in F.map2 G.point
        (F.floatRange -maxCoord maxCoord)
        (F.floatRange -maxCoord maxCoord)

{-| Check that two points are within a tenth of a pixel of each other. -}
expectPointsClose : G.Point -> G.Point -> E.Expectation
expectPointsClose
    = (Util.expectPointWithin <| E.Absolute 0.1)

shapeEncodeDecode : T.Test
shapeEncodeDecode = T.fuzz fuzzPolygon "Encode then decode shape"
    <| \shape -> checkDecode expectPolygonsClose shape
        (BD.decode CE.decoderBasicShape <| BE.encode <| CE.encoderBasicShape shape)


{-| A fuzzer for a polygon.
    The number of points is between 3 and 16.
    The color is always the default blue.
 -}
fuzzPolygon : F.Fuzzer Shape.Def
fuzzPolygon = F.map
    (\ps -> { geoDef = G.Polygon ps, fill = Start.blue })
    <| F.andThen
        (\n -> F.sequence <| List.repeat n fuzzPoint)
        (F.intRange 3 16)

{-| Expect two shapes to have the same fill, and check that the polygon points
    are within a tenth of a pixel of each other.
    Will trivially pass for non-polygon shapes.
 -}
expectPolygonsClose : Shape.Def -> Shape.Def -> E.Expectation
expectPolygonsClose s1 s2
    = E.all
        [
            always <| E.equal s1.fill s2.fill
          , always <| Util.compareGeoDefs expectPointsClose s1.geoDef s2.geoDef
        ] ()

contentListEncodeDecode : T.Test
contentListEncodeDecode = T.fuzz fuzzContentListWithinLimits "Encode then decode content list"
    <| \contentList -> checkDecode expectContentsAgree contentList
        (BD.decode CE.decoderContentList <| BE.encode <| CE.encoderContentList contentList)

{-| Check that two lists of content have the same shapes and iter frames. -}
expectContentsAgree : List Content.Content -> List Content.Content -> E.Expectation
expectContentsAgree c1 c2
    = let
        (shapes1, iterFrames1) = Content.partitionContentDefs c1
        (shapes2, iterFrames2) = Content.partitionContentDefs c2
    in
        Util.compareLists expectPolygonsClose shapes1 shapes2

{-| Fuzz a list of content, keeping the number of shapes and iter frames
    within the limits of the encoding.
 -}
fuzzContentListWithinLimits : F.Fuzzer (List Content.Content)
fuzzContentListWithinLimits
    = let
        fuzzShapes = (F.map << List.map) (Content.makeShape <| ID.Trunk "s")
            <| F.andThen
                (\n -> F.sequence <| List.repeat n fuzzPolygon)
                (F.intRange 0 7)
    in fuzzShapes -- add iterFrames later

contentListOutsideLimitsEncodeDecode : T.Test
contentListOutsideLimitsEncodeDecode
    = T.fuzz fuzzContentListOutsideLimits "Encode then decode content list truncating if outside limits"
        <| \contentList -> checkDecode expectContentsAgree (truncateContentList contentList)
            (BD.decode CE.decoderContentList <| BE.encode <| CE.encoderContentList contentList)

{-| Truncate a list of content to the maximum length that can be encoded.
 -}
truncateContentList : List Content.Content -> List Content.Content
truncateContentList contents
    = let
        (shapes, iterFrames) = Content.partitionContentDefs contents
        shapeContents = List.map (Content.makeShape <| ID.Trunk "s") shapes
        iterFrameContents = List.map (Content.makeIterFrame (ID.Trunk "i") Space.outerFrame) iterFrames
    in (List.take 7 shapeContents) ++ (List.take 15 iterFrameContents)

{-| Fuzz a list of content, with 7 or more shapes and 15 or more iter frames.
 -}
fuzzContentListOutsideLimits : F.Fuzzer (List Content.Content)
fuzzContentListOutsideLimits
    = let
        fuzzShapes = (F.map << List.map) (Content.makeShape <| ID.Trunk "s")
            <| F.andThen
                (\n -> F.sequence <| List.repeat n fuzzPolygon)
                (F.intRange 7 16)
    in fuzzShapes -- add iterFrames later
