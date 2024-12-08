module Tutorial exposing (
        view
      , Model
      , WrapModel
      , wrapInit
      , Message(..)
      , WrapMessage(..)
      , update
    )

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Html.Styled.Events as HSE
import Css
import Maybe

import Tutorial.Sequence as TS
import Space
import SpaceCommand as SC

{-| State for tutorial. -}
type alias Model
    = { sequence : TS.Sequence }

{-| Starting state for tutorial. -}
init : Model
init = { sequence = TS.init }

{-| Convenient helper to update sequence. -}
modelLiftSequence
    : (TS.Sequence -> TS.Sequence)
   -> (Model -> Model)
modelLiftSequence f model
    = { model | sequence = f model.sequence }

{-| Combined model for tutorial and space. -}
type alias WrapModel
    = {
        space : Space.Model
      , tutorial : Model
    }

{-| Convenient helper to update tutorial state. -}
wrapLiftTutorial
    : (Model -> Model)
   -> (WrapModel -> WrapModel)
wrapLiftTutorial f model
    = { model | tutorial = f model.tutorial }

{-| Combined starting state for tutorial and space. -}
wrapInit : WrapModel
wrapInit = {
        space = SC.init
      , tutorial = init
    }

{-| View for the tutorial element shown in the header. -}
view : Model -> HS.Html Message
view model
    = let
        current = TS.getCurrent model.sequence
        message = Maybe.withDefault
            (HS.text "")
            (Maybe.map TS.getMessage current)
        shouldDisplay = case current of
            Just _ -> True
            Nothing -> False
      in HS.div
        [
            HSA.class "control-background"
          , HSA.css
                [ Css.padding (Css.rem 1)
                , Css.borderRadius (Css.rem 0.5)
                , Css.maxWidth (Css.px 400)
                , (if shouldDisplay
                    then Css.display Css.block
                    else Css.display Css.none)
                ]
          , HSE.onClick Advance
        ]
        -- map for type only
        [ HS.map (always Advance) message ]

{-| Message for tutorial. -}
type Message
    = Advance

{-| Combined message for tutorial and space. -}
type WrapMessage
    = SpaceMessage SC.Message
    | TutorialMessage Message

{-| Update for tutorial messages.
    This gets to see the whole model so that tutorial steps can modify
    the state to guide interactions.
 -}
update : Message -> WrapModel -> WrapModel
update message
    = case message of
        Advance -> wrapLiftTutorial (modelLiftSequence TS.advance)
