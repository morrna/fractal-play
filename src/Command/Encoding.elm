module Command.Encoding exposing (
        encodeStateByteString
      , decodeStateByteString
        -- Exposed for unit testing
      , encoderIterMode
      , decoderIterMode
      , encoderPoint
      , decoderPoint
      , encoderBasicShape
      , decoderBasicShape
      , encoderContentList
      , decoderContentList
      , encoderIterFrame
      , decoderIterFrame
    )

import Bytes as B
import Bytes.Encode as BE
import Bytes.Decode as BD
import Base64.Encode as B64E
import Base64.Decode as B64D
import UrlBase64
import UndoList as U

import Space
import Space.Frame as Frame
import Space.IterFrame as IterFrame
import Geometry as G
import Space.Shape as Shape
import Space.Content as Content
import Space.TreeID as ID
import Start

{-| Get a base 64 encoded string for the current state. -}
encodeStateByteString
    : Space.Model
   -> String
encodeStateByteString model
    = Result.withDefault "Failed to encode"
        <| UrlBase64.encode (Result.Ok << B64E.encode << B64E.bytes << encodeStateBytes) model

encodeStateBytes : Space.Model -> B.Bytes
encodeStateBytes { iterMode, baseContents }
    = BE.encode <| BE.sequence
        [
            encoderIterMode iterMode
          , encoderContentList baseContents.present
        ]

{-| Apply the state from a base 64 encoded string to the model.
    Not everything in the model is encoded, so pass in the current model
    with the first argument and only the decoded pieces will be applied.

    The model is always returned. It will be unchanged if the decoding fails.
    The first return value is an optional error message for decoding failures.
 -}
decodeStateByteString
    : Space.Model
   -> String
   -> (Maybe String, Space.Model)
decodeStateByteString spaceModel bookmark
    = case UrlBase64.decode
            (stringifyB64Error << B64D.decode B64D.bytes)
            bookmark
        of
        Result.Ok decodedBytes
            -> decodeStateBytes spaceModel decodedBytes
        Result.Err err
            -> (Just ("Token decode failure: " ++ err), spaceModel)

stringifyB64Error
    : Result B64D.Error a
   -> Result String a
stringifyB64Error result
    = case result of
        Result.Ok a
            -> Result.Ok a
        Result.Err err
            -> case err of
                B64D.ValidationError -> Result.Err "base64 validation error"
                B64D.InvalidByteSequence -> Result.Err "base64 invalid byte sequence"

decodeStateBytes
    : Space.Model
   -> B.Bytes
   -> (Maybe String, Space.Model)
decodeStateBytes spaceModel bytes
    = Maybe.withDefault
        (Just "Byte decode failure", spaceModel)
        (
            Maybe.map (\sm -> (Nothing, sm))
            <| BD.decode (decoderModel spaceModel) bytes
        )

decoderModel
    : Space.Model
   -> BD.Decoder Space.Model
decoderModel spaceModel
    = BD.map2
        (\iterMode contents -> { spaceModel |
            iterMode = iterMode, baseContents = U.fresh contents
        })
        decoderIterMode
        (decoderContentList spaceModel.referenceFrame)

{-| Encoder for IterFrame.Mode
    Currently encodes depth, showIterFrames, and onlyShowLastLayer.
    Encoding coloring should be revisited when the coloring is changeable
    by the user.
 -}
encoderIterMode : IterFrame.Mode -> BE.Encoder
encoderIterMode mode
    = let
        flagsInt = boolFlagsToInt [mode.showIterFrames, mode.onlyShowLastLayer]
        {- Depth should never be more than 63, so we can combine these to a single unsignedInt8 -}
    in BE.unsignedInt8 (mode.depth * 4 + flagsInt)

{-| Decode IterFrame.Mode from bytes. -}
decoderIterMode : BD.Decoder IterFrame.Mode
decoderIterMode
    = let
        initMode = IterFrame.initMode
        makeIM combinedInt
            = let
                flagsInt = remainderBy 4 combinedInt
                flagList = intToBoolFlags 2 flagsInt
                showIterFrames = Maybe.withDefault False (List.head flagList)
                onlyShowLastLayer = Maybe.withDefault False (Maybe.andThen List.head (List.tail flagList))
            in { initMode |
                depth = combinedInt // 4
              , showIterFrames = showIterFrames
              , onlyShowLastLayer = onlyShowLastLayer
              }
    in BD.map makeIM BD.unsignedInt8

{-| Encode a sequence of boolean flags in an integer, bitmask style. -}
boolFlagsToInt : List Bool -> Int
boolFlagsToInt flags
    = List.foldr (\flag acc -> if flag then acc * 2 + 1 else acc * 2) 0 flags

{-| Decode a sequence of boolean flags from an integer, bitmask style.
    The first argument is the number of flags. If the number of flags is more than
    the number of bits in the integer, the extra flags are False like you'd expect.
 -}
intToBoolFlags : Int -> Int -> List Bool
intToBoolFlags numFlags int
    = if numFlags > 0
        then (remainderBy 2 int == 1) :: (intToBoolFlags (numFlags - 1) (int // 2))
        else []

coordUnit : Float
coordUnit = 4 * max (Frame.width Space.outerFrame) (Frame.height Space.outerFrame) / 65536

toCoordUnit : Float -> Int
toCoordUnit c = round <| c / coordUnit

fromCoordUnit : Int -> Float
fromCoordUnit = (*) coordUnit << toFloat

{-| Encode points

The smallest float encoder offered by the `Bytes` library is 32 bits, or 4 bytes.
This is more detail than we really need. Even if people can see subpixel detail,
that's still only 1 part in about 10^4, which is about 1 in 2^13. Let's keep things
simple and use 16 bits for each coordinate.

The base unit will come from the size of the view box, which specified by Space.outerFrame.
Things can move off the edge of the view box some, so let's use 4 times the largest view box
dimension as maximum allowed coordinate. The granularity will be 1/2^16 of that maximum.
 -}
encoderPoint : G.Point -> BE.Encoder
encoderPoint {x, y}
    = BE.sequence
        [
            BE.signedInt16 B.BE <| toCoordUnit x
          , BE.signedInt16 B.BE <| toCoordUnit y
        ]

decoderPoint : BD.Decoder G.Point
decoderPoint
    = BD.map2 G.point
        (BD.map fromCoordUnit (BD.signedInt16 B.BE))
        (BD.map fromCoordUnit (BD.signedInt16 B.BE))

{-| Encode a polygonal starting shape.

The color is not encoded, because so far it has only been blue,
and we want to make the encoded representation as short as possible.
See the note on the extension bit in the comment for `encoderContentList`
for an idea of how to selectively break this assumption.

The first return value is the number of points in the polygon.
The second is the encoder for the points.
 -}
encoderBasicShape : Shape.Def -> BE.Encoder
encoderBasicShape {geoDef}
    = case geoDef of
        G.Polygon ps -> BE.sequence
            <| BE.unsignedInt8 (List.length ps) :: List.map encoderPoint ps
        _ -> BE.sequence [] -- Not currently implemented

decoderBasicShape : BD.Decoder Shape.Def
decoderBasicShape
    = let
        decoderPointList n
            = if n > 0
                then BD.map2 (::) decoderPoint (decoderPointList (n - 1))
                else BD.succeed []
    in BD.andThen
        ((BD.map (\ps -> { geoDef = G.Polygon ps, fill = Start.blue })) << decoderPointList)
        BD.unsignedInt8


{-| Encode a content list.

Because the list of base shapes is expected to be very short, usually 1,
and the list of iteration frames is not that long, at most 8 in the current
presets, both counts can be encoded in a single byte. The plan is

    0 (extension bit) .. 7 (base shape count) .. 15 (iteration frame count)

The extension bit will always be 0 in the current version. Future versions
may set it and then use a completely different scheme of assumptions.

The content IDs will not be encoded. They can just be generated when decoding.
 -}
encoderContentList : List Content.Content -> BE.Encoder
encoderContentList contents
    = let
        (shapesOrig, iterFramesOrig) = Content.partitionContentDefs contents
        -- Limit the number of shapes to 7, and the number of iter frames to 15
        shapes = List.take 7 shapesOrig
        iterFrames = List.take 15 iterFramesOrig
        controlByte = (List.length shapes) * 16 + (List.length iterFrames)
    in BE.sequence
        [
            BE.unsignedInt8 controlByte
          , BE.sequence (List.map encoderBasicShape shapes)
          , BE.sequence (List.map encoderIterFrame iterFrames)
        ]

decoderContentList : Frame.Def -> BD.Decoder (List Content.Content)
decoderContentList frameDef
    = BD.andThen
        (\(extensionOn, controlByteTail)
            -> if extensionOn
                then BD.fail
                else let
                        shapesCount = controlByteTail // 16
                        iterFramesCount = remainderBy 16 controlByteTail
                    in BD.map2 (++)
                        (decoderBasicShapes shapesCount)
                        (decoderIterFrames frameDef iterFramesCount)
        )
        (BD.map
            (\controlByte -> (controlByte // 128 == 1, remainderBy 128 controlByte))
            BD.unsignedInt8
        )

decoderBasicShapes : Int -> BD.Decoder (List Content.Content)
decoderBasicShapes count
    = if count > 0
        then BD.map2 (::)
            (BD.map (Content.makeShape (ID.Trunk <| "s" ++ String.fromInt count)) decoderBasicShape)
            (decoderBasicShapes (count - 1))
        else BD.succeed []

{-| Encode matrix elements

Matrix elements are essentially ratios of coordinate values, so they need to be
discretized differently than points. If we want to multiply a coordinate on the
order of 1000 pixels and have it wind up within 0.1 pixels of the true result,
the ratio needs to be discretized to about 1 part in 10^4. 1 part in 2^15 is
enough to represent 1 part in 10^4, and that maps nicely onto a signedInt16.

All matrix elements for IFS fractals should be less than 1 in absolute value,
because they always make the input smaller. Because of this, we can just encode
(-1, 1).
 -}
encoderMatrixElement : Float -> BE.Encoder
encoderMatrixElement el
    = BE.signedInt16 B.BE <| truncate <| el * twoToThe15

twoToThe15 : Float
twoToThe15 = 32768

decoderMatrixElement : BD.Decoder Float
decoderMatrixElement
    = BD.map (\i -> toFloat i / twoToThe15) (BD.signedInt16 B.BE)

encoderDisplacement : G.Displacement -> BE.Encoder
encoderDisplacement {x, y}
    = BE.sequence
        [
            encoderMatrixElement x
          , encoderMatrixElement y
        ]

decoderDisplacement : BD.Decoder G.Displacement
decoderDisplacement
    = BD.map2 G.disp
        decoderMatrixElement
        decoderMatrixElement

{-| Encode iteration frames -}
encoderIterFrame : IterFrame.Def -> BE.Encoder
encoderIterFrame {xBasis, yBasis, offset}
    = BE.sequence
        [
            encoderDisplacement xBasis
          , encoderDisplacement yBasis
          , encoderPoint offset
        ]

decoderIterFrame : BD.Decoder IterFrame.Def
decoderIterFrame
    = BD.map3 IterFrame.Def
        decoderDisplacement
        decoderDisplacement
        decoderPoint

decoderIterFrames : Frame.Def -> Int -> BD.Decoder (List Content.Content)
decoderIterFrames frameDef count
    = if count > 0
        then BD.map2 (::)
            (BD.map (Content.makeIterFrame (ID.Trunk <| "f" ++ String.fromInt count) frameDef) decoderIterFrame)
            (decoderIterFrames frameDef (count - 1))
        else BD.succeed []
