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
import Space.IterFrame as IterFrame
import Space.Content as Content

{-| State for tutorial. -}
type alias Model
    = {
        sequence : TS.Sequence
      , cache : Cache
    }

{-| Starting state for tutorial. -}
init : Model
init = {
        sequence = TS.init
      , cache = {
            iterMode = IterFrame.initMode
          , hiddenContent = []
        }
    }

{-| User state to be restored later. -}
type alias Cache = {
        iterMode : IterFrame.Mode
      , hiddenContent : List Content.Content
    }

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

{-| Convenient helper to update space state. -}
wrapLiftSpace
    : (Space.Model -> Space.Model)
   -> (WrapModel -> WrapModel)
wrapLiftSpace f model
    = { model | space = f model.space }

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
                , Css.maxWidth (Css.rem 80)
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
        Advance -> doStepAction << wrapLiftTutorial (modelLiftSequence TS.advance)

{-| Update the model based on the current step's action. -}
doStepAction : WrapModel -> WrapModel
doStepAction model
    = let
        current = TS.getCurrent model.tutorial.sequence
        action = Maybe.withDefault TS.NoAction (Maybe.map .action current)
      in case action of
        TS.NoAction -> model
        TS.ModifySpace modifier -> cacheAndModifySpace modifier model
        TS.RestoreFromCache -> wrapLiftSpace (restoreFromCache model.tutorial.cache) model

{-| Cache the current state and modify the space. -}
cacheAndModifySpace : (Space.Model -> Space.Model) -> WrapModel -> WrapModel
cacheAndModifySpace modifier model
    = let
        newSpaceModel = modifier model.space
        cache = {
            iterMode = model.space.iterMode
          , hiddenContent = List.filter
              (\content -> not <| List.member content newSpaceModel.baseContents)
              model.space.baseContents
          }
        oldTutorial = model.tutorial
      in { model |
            tutorial = { oldTutorial | cache = cache }
          , space = newSpaceModel
        }

{-| Restore the state from the cache. -}
restoreFromCache : Cache -> Space.Model -> Space.Model
restoreFromCache cache spaceModel
    = { spaceModel |
        iterMode = cache.iterMode
      , baseContents = spaceModel.baseContents ++ cache.hiddenContent
      }
