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
import UndoList as U

import Tutorial.Sequence as TS
import Space
import SpaceCommand as SC
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
      , cache = initCache
    }

{-| User state to be restored later. -}
type alias Cache = {
        hiddenContent : Maybe (List Content.Content)
    }

{-| Initial empty cache state -}
initCache : Cache
initCache = {
        hiddenContent = Nothing
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

{-| Convenient helper to update cache state. -}
wrapLiftCache
    : (Cache -> Cache)
   -> (WrapModel -> WrapModel)
wrapLiftCache f
    = wrapLiftTutorial (\m -> { m | cache = f m.cache })

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

{-| Check if there are no hidden contents cached -}
isEmptyHiddenContent : Cache -> Bool
isEmptyHiddenContent cache =
    cache.hiddenContent == Nothing

{-| Cache the current state if appropriate parts of cache are empty and modify the space. -}
cacheAndModifySpace : (Space.Model -> Space.Model) -> WrapModel -> WrapModel
cacheAndModifySpace modifier model
    = let
        newSpaceModel = modifier model.space
        oldCache = model.tutorial.cache

        -- Only update the parts of cache that are empty
        newCache = {
            hiddenContent = if isEmptyHiddenContent oldCache
                then Just (List.filter
                    (\content -> not <| List.member content newSpaceModel.baseContents.present)
                    model.space.baseContents.present)
                else oldCache.hiddenContent
          }

        oldTutorial = model.tutorial
      in { model |
            tutorial = { oldTutorial | cache = newCache }
          , space = newSpaceModel
        }

{-| Restore the hidden frames from cache. -}
restoreHiddenContent : Cache -> Space.Model -> Space.Model
restoreHiddenContent cache spaceModel =
    { spaceModel |
        baseContents = U.new
            (spaceModel.baseContents.present ++ Maybe.withDefault [] cache.hiddenContent)
            spaceModel.baseContents
    }

{-| Reset the hidden content portion of the cache -}
resetHiddenContent : WrapModel -> WrapModel
resetHiddenContent =
    wrapLiftCache (\m -> { m | hiddenContent = Nothing })

{-| Update the model based on the current step's action. -}
doStepAction : WrapModel -> WrapModel
doStepAction model
    = let
        current = TS.getCurrent model.tutorial.sequence
        action = Maybe.withDefault TS.NoAction (Maybe.map .action current)
      in case action of
        TS.NoAction -> model
        TS.ModifySpace modifier -> cacheAndModifySpace modifier model
        TS.RestoreHiddenContent ->
            (resetHiddenContent
                << wrapLiftSpace (restoreHiddenContent model.tutorial.cache)
            ) model
