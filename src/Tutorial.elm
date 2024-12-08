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

type alias Model
    = { sequence : TS.Sequence }

init : Model
init = { sequence = TS.init }

modelLiftSequence
    : (TS.Sequence -> TS.Sequence)
   -> (Model -> Model)
modelLiftSequence f model
    = { model | sequence = f model.sequence }

type alias WrapModel
    = {
        space : Space.Model
      , tutorial : Model
    }

wrapLiftTutorial
    : (Model -> Model)
   -> (WrapModel -> WrapModel)
wrapLiftTutorial f model
    = { model | tutorial = f model.tutorial }

wrapInit : WrapModel
wrapInit = {
        space = SC.init
      , tutorial = init
    }

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
        [ message ]

type Message
    = Advance

type WrapMessage
    = SpaceMessage SC.Message
    | TutorialMessage Message

update : Message -> WrapModel -> WrapModel
update message
    = case message of
        Advance -> wrapLiftTutorial (modelLiftSequence TS.advance)
