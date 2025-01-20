module Command.Keyboard exposing (
        undoRedoSubscriptions
    )

import Browser.Events
import Json.Decode as JD
import UndoList as U

{-| Subscribe to Ctrl+Z for undo and Ctrl+Shift+Z for redo. -}
undoRedoSubscriptions : Sub (U.Msg ())
undoRedoSubscriptions = Browser.Events.onKeyDown undoRedoKeyDecoder

undoRedoKeyDecoder : JD.Decoder (U.Msg ())
undoRedoKeyDecoder = JD.map
    (\isShiftOn -> if isShiftOn then U.Redo else U.Undo)
    <| JD.andThen (always <| JD.field "shiftKey" JD.bool) (ctrlLetterDecoder "z")

letterDecoder : String -> JD.Decoder ()
letterDecoder l = JD.andThen
    (\key ->
        if String.toLower key == l then
            JD.succeed ()
        else
            JD.fail "letter mismatch"
    )
    (JD.field "key" JD.string)

ctrlDecoder : JD.Decoder ()
ctrlDecoder = JD.andThen
    (\ctrl ->
        if ctrl then
            JD.succeed ()
        else
            JD.fail "not ctrl"
    )
    (JD.field "ctrlKey" JD.bool)

ctrlLetterDecoder : String -> JD.Decoder ()
ctrlLetterDecoder l = JD.andThen
    (always <| letterDecoder l)
    ctrlDecoder
