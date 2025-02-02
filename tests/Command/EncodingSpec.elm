module Command.EncodingSpec exposing ( suite )

import Test as T
import Fuzz as F
import Expect as E
import Bytes.Encode as BE
import Bytes.Decode as BD

import Command.Encoding as CE
import Space.IterFrame as IterFrame
import Space
import Start


suite : T.Test
suite = T.describe "module Command.Encoding" [
        iterModeEncodeDecode
      , stateByteStringEncodeDecode
    ]

iterModeEncodeDecode: T.Test
iterModeEncodeDecode = T.fuzz fuzzIterMode "Encode then decode iterMode"
    <| \iterMode -> E.equal (Just iterMode)
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

stateByteStringEncodeDecode : T.Test
stateByteStringEncodeDecode = T.fuzz fuzzState "Encode then decode stateByteString"
    <| \state -> E.equal (Nothing, state)
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
