module Space exposing (
        Model
      , Message
      , emptyModel
      , addContentToModel
      , addIterFrame
      , addIterFrameNew
      , view
      , update
      , setIterationDepth
      , outerFrame
    )

import Html.Styled as HS
import Svg.Styled as S
import Svg.Styled.Attributes as A
import Svg.Styled.Keyed as K
import List
import Maybe as M
import UndoList as U

import Space.Content as C
import Space.Frame as Frame
import Space.IterFrame as IterFrame
import Space.TreeID as ID
import Space.Pointer as Pointer
import Space.Move as Move

{-| ## View interface -}

type alias Model = {
        referenceFrame : Frame.Def
      , baseContents : U.UndoList (List C.Content)
      , interact : Interact
      , iterMode : IterFrame.Mode
      -- When adding a new IterFrame, use this.
      , defaultIterFrame : IterFrame.Def
    }

type Message
    = PtrOnSpace (Pointer.Update (Maybe C.Selected))
    | PtrSelect C.Selected

{-| Create an empty model with the given reference frame. -}
emptyModel : Frame.Def -> Model
emptyModel frame
    = {
        referenceFrame = frame
      , baseContents = U.fresh []
      , interact = {
          pointer = Pointer.emptyState Nothing
      }
      , iterMode = IterFrame.initMode
      , defaultIterFrame = IterFrame.identityDef
    }

view : Model -> HS.Html Message
view model = S.svg
    (
        getViewBox
        :: (A.preserveAspectRatio "xMidYMid meet")
        :: (A.width "100%")
        :: (A.height "100%")
        :: (Pointer.dragEvents (always Nothing) PtrOnSpace)
    )
    (
        K.lazyNode "g" [] showContent (getContents model)
        :: showReferenceFrame model
    )

update : Message -> Model -> Model
update m = case m of
    PtrOnSpace ptrUpd -> updatePtrOnSpace ptrUpd
    PtrSelect content -> doContentSelect content

{- ## Frame SVG -}

{-| The rectangle that defines the outer frame of the SVG. -}
outerFrame: Frame.Def
outerFrame = Frame.Golden 801
 -- When the reference frame is 800, having the space be 800 leads to a bad looking edge.

getViewBox : S.Attribute m
getViewBox = A.viewBox <| Frame.viewBoxString outerFrame

{- ## Contents -}

{-| Build the content list from the base contents and the iterations specified by iterMode -}
getContents : Model -> List (String, C.Content)
getContents m = C.getAllToShow m.iterMode m.baseContents.present

{-| Add content to the model's base contents.

    Useful for initial setup. Does not create a new undo state.
 -}
addContentToModel
    : ID.TreeID -- ID for content
    -> (Model -> ID.TreeID -> C.Content) -- constructs content from ID
    -> Model
    -> Model
addContentToModel cid cGen model
    = liftBaseContents
        (C.appendNew (cGen model) cid)
        model

{-| Add content to the model's base contents.

    Creates a new undo state.
 -}
addContentToModelNew
    : ID.TreeID -- ID for content
    -> (Model -> ID.TreeID -> C.Content) -- constructs content from ID
    -> Model
    -> Model
addContentToModelNew cid cGen model
    = liftBaseContentsNew
        (C.appendNew (cGen model) cid)
        model

{-| Add an IterFrame to the model's base contents.

    Does not create a new undo state.
 -}
addIterFrame
    : ID.TreeID
    -> (Model -> IterFrame.Def)
    -> Model
    -> Model
addIterFrame id iterDefGen model =
    let
        cGen m id2 = C.makeIterFrame id2 model.referenceFrame (iterDefGen m)
    in
    addContentToModel id cGen model

{-| Add an IterFrame to the model's base contents.

    Creates a new undo state.
 -}
addIterFrameNew
    : ID.TreeID
    -> (Model -> IterFrame.Def)
    -> Model
    -> Model
addIterFrameNew id iterDefGen model =
    let
        cGen m id2 = C.makeIterFrame id2 model.referenceFrame (iterDefGen m)
    in
        addContentToModelNew id cGen model

showContent : C.Content -> S.Svg Message
showContent
    = C.showContent
        (Pointer.select PtrSelect)


type alias Interact
    = {
        pointer: Pointer.State (Maybe C.Selected)
    }

{-| Given an updater for Interact, make an updater for Model. -}
liftInteract
    : (Interact -> Interact)
    -> Model
    -> Model
liftInteract f mdl = { mdl | interact = f mdl.interact }

{-| Set the pointer state in the interact record. -}
updatePointer
    : Pointer.Update (Maybe C.Selected)
    -> Interact
    -> Interact
updatePointer ptrUpdate interact = { interact | pointer = ptrUpdate interact.pointer }

{-| Given an updater for baseContents, make an updater for Model.

    Does not create a new undo state.
 -}
liftBaseContents
    : (List C.Content -> List C.Content)
    -> Model
    -> Model
liftBaseContents f mdl = { mdl | baseContents = U.mapPresent f mdl.baseContents }

{-| Given an updater for baseContents, update the Model and add a new undo state. -}
liftBaseContentsNew
    : (List C.Content -> List C.Content)
    -> Model
    -> Model
liftBaseContentsNew f mdl
    = { mdl | baseContents = U.new (f mdl.baseContents.present) mdl.baseContents }

updatePtrOnSpace
    : Pointer.Update (Maybe C.Selected)
    -> Model
    -> Model
updatePtrOnSpace ptrUpd mdl
    = let
        {- Only do an action if we have a selected. -}
        transformer = M.withDefault identity << M.map C.action << Move.pullMaybe
        (mungeContent, newPointerState)
            = Pointer.doDrag
                transformer
                mdl.interact.pointer
                ptrUpd

        {- Only add to the undo stack when first turning on.
           This saves the state from before the pointer starts moving to the stack.
           While in motion, only the present state is updated. -}
        lifter = if Pointer.isTurningOn mdl.interact.pointer newPointerState
            then liftBaseContentsNew
            else liftBaseContents
    in
        lifter (List.map mungeContent)
            <| liftInteract (updatePointer <| always newPointerState) mdl

{-| Update the model when a content is selected. -}
doContentSelect
    : C.Selected
    -> Model
    -> Model
doContentSelect content
    = liftInteract (updatePointer <| Pointer.updatePayload (always (Just content)))

showReferenceFrame : Model -> List (S.Svg Message)
showReferenceFrame model =
    if model.iterMode.showIterFrames
    then
        [S.rect
            [ A.x (String.fromFloat (-(Frame.width model.referenceFrame) / 2))
            , A.y (String.fromFloat (-(Frame.height model.referenceFrame) / 2))
            , A.width (String.fromFloat (Frame.width model.referenceFrame))
            , A.height (String.fromFloat (Frame.height model.referenceFrame))
            , A.fill "none"
            , A.stroke "black"
            , A.strokeWidth "1"
            ]
            []
        ]
    else
        []

{-| Set the iteration depth on a model. -}
setIterationDepth : Int -> Model -> Model
setIterationDepth depth model = { model | iterMode = IterFrame.setIterationDepth depth model.iterMode }
