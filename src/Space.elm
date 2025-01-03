module Space exposing (
        Model
      , Message
      , emptyModel
      , addContentToModel
      , addIterFrame
      , view
      , update
      , setIterationDepth
      , outerFrame
    )

import Html.Styled as HS
import Css
import Svg.Styled as S
import Svg.Styled.Attributes as A
import Svg.Styled.Keyed as K
import List
import Maybe as M
import Space.Content as C
import Space.Frame as Frame
import Space.IterFrame as IterFrame
import Space.TreeID as ID
import Space.Pointer as Pointer
import Space.Move as Move

{-| ## View interface -}

type alias Model = {
        referenceFrame : Frame.Def
      , baseContents : List C.Content
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
      , baseContents = []
      , interact = {
          pointer = Pointer.emptyState Nothing
      }
      , iterMode = IterFrame.initMode
      , defaultIterFrame = IterFrame.identityDef
    }

view : Model -> HS.Html Message
view model = S.svg
    (
        [
            A.css <| getStyle
          , getViewBox
        ]
        ++ (Pointer.dragEvents (always Nothing) PtrOnSpace)
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

getStyle : List (Css.Style)
getStyle =
    Css.margin (Css.px 10) :: (getWidthHeight outerFrame)

getWidthHeight : Frame.Def -> List (Css.Style)
getWidthHeight outer
    = [ Css.width (Css.px <| Frame.width outer), Css.height (Css.px <| Frame.height outer) ]

getViewBox : S.Attribute m
getViewBox = A.viewBox <| Frame.viewBoxString outerFrame

{- ## Contents -}

{-| Build the content list from the base contents and the iterations specified by iterMode -}
getContents : Model -> List (String, C.Content)
getContents m = C.getAllToShow m.iterMode m.baseContents

addContentToModel
    : ID.TreeID -- ID for content
    -> (Model -> ID.TreeID -> C.Content) -- constructs content from ID
    -> Model
    -> Model
addContentToModel cid cGen model
    = { model | baseContents = C.appendNew (cGen model) cid model.baseContents }

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

{-| Given an updater for baseContents, make an updater for Model. -}
liftBaseContents
    : (List C.Content -> List C.Content)
    -> Model
    -> Model
liftBaseContents f mdl = { mdl | baseContents = f mdl.baseContents }

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
    in
        liftBaseContents (List.map mungeContent)
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
