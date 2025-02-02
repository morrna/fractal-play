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
import UrlBase64

import Space
import Space.IterFrame as IterFrame

{-| Get a base 64 encoded string for the current state. -}
getStateByteString
    : Space.Model
   -> String
getStateByteString model
    = Result.withDefault "Failed to encode"
        <| UrlBase64.encode (Result.Ok << B64E.encode << B64E.bytes << getStateBytes) model

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
