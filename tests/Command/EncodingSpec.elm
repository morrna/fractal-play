module Command.EncodingSpec exposing ( suite )

import Test as T
import Fuzz as F
import Expect as E
import Bytes.Encode as BE
import Bytes.Decode as BD

import Command.Encoding as CE
import Space.IterFrame as IterFrame


suite : T.Test
suite = T.describe "module Command.Encoding" [
        iterModeEncodeDecode
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
