module Command.Encoding exposing (
        getStateByteString
        -- Exposed for unit testing
      , encoderIterMode
      , decoderIterMode
    )

import Bytes as B
import Bytes.Encode as BE
import Bytes.Decode as BD
import Base64.Encode as B64E

import Space
import Space.IterFrame as IterFrame

{-| Get a base 64 encoded string for the current state. -}
getStateByteString
    : Space.Model
   -> String
getStateByteString model
    = B64E.encode <| B64E.bytes <| getStateBytes model

getStateBytes : Space.Model -> B.Bytes
getStateBytes { iterMode }
    = BE.encode <| encoderIterMode iterMode

{-| Encoder for IterFrame.Mode
    Currently encodes depth, showIterFrames, and onlyShowLastLayer.
    Encoding coloring should be revisited when the coloring is changeable
    by the user.
 -}
encoderIterMode : IterFrame.Mode -> BE.Encoder
encoderIterMode mode
    = BE.sequence
        [ BE.unsignedInt8 mode.depth
        , BE.unsignedInt8 (if mode.showIterFrames then 1 else 0)
        , BE.unsignedInt8 (if mode.onlyShowLastLayer then 1 else 0)
        ]

{-| Decode IterFrame.Mode from bytes. -}
decoderIterMode : BD.Decoder IterFrame.Mode
decoderIterMode
    = let
        initMode = IterFrame.initMode
        makeIM depth showIterFrames onlyShowLastLayer
            = { initMode |
                depth = depth
              , showIterFrames = showIterFrames == 1
              , onlyShowLastLayer = onlyShowLastLayer == 1
              }
    in BD.map3 makeIM
        (BD.unsignedInt8)
        (BD.unsignedInt8)
        (BD.unsignedInt8)
