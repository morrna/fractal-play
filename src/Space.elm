module Space exposing (
        Model
      , Message
      , emptyModel
      , addContentToModel
      , addIterFrame
      , view
      , update
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
        outerFrame : Frame.Def
      , referenceFrame : Maybe Frame.Def
      , baseContents : List C.Content
      , interact : Interact
      , iterMode : IterFrame.Mode
      -- When adding a new IterFrame, use this.
      , defaultIterFrame : IterFrame.Def
    }

type Message
    = PtrOnSpace (Pointer.Update (Maybe C.Selected))
    | PtrSelect C.Selected

emptyModel : Frame.Def -> Model
emptyModel frame
    = {
        outerFrame = frame
      , referenceFrame = Nothing
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
            A.css <| getStyle model
          , getViewBox model
        ]
        ++ (Pointer.dragEvents (always Nothing) PtrOnSpace)
    )
    (
        K.lazyNode "g" [] showContent (getContents model)
        :: maybeShowReferenceFrame model
    )

update : Message -> Model -> Model
update m = case m of
    PtrOnSpace ptrUpd -> updatePtrOnSpace ptrUpd
    PtrSelect content -> doContentSelect content

{- ## Frame SVG -}

getStyle : Model -> List (Css.Style)
getStyle model =
    let
        baseStyles = Css.margin (Css.px 10) :: (getWidthHeight <| model.outerFrame)
    in
    case model.referenceFrame of
        Nothing ->
            Css.border2 (Css.px 1) Css.solid :: baseStyles
        Just _ ->
            baseStyles

getWidthHeight : Frame.Def -> List (Css.Style)
getWidthHeight outer
    = [ Css.width (Css.px <| Frame.width outer), Css.height (Css.px <| Frame.height outer) ]

getViewBox : Model -> S.Attribute m
getViewBox model = A.viewBox <| Frame.viewBoxString model.outerFrame

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
        frameDef = Maybe.withDefault model.outerFrame model.referenceFrame
        cGen m id2 = C.makeIterFrame id2 frameDef (iterDefGen m)
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

maybeShowReferenceFrame : Model -> List (S.Svg Message)
maybeShowReferenceFrame model =
    case model.referenceFrame of
        Just frame ->
            if model.iterMode.showIterFrames then
                [S.rect
                    [ A.x (String.fromFloat (-(Frame.width frame) / 2))
                    , A.y (String.fromFloat (-(Frame.height frame) / 2))
                    , A.width (String.fromFloat (Frame.width frame))
                    , A.height (String.fromFloat (Frame.height frame))
                    , A.fill "none"
                    , A.stroke "black"
                    , A.strokeWidth "1"
                    ]
                    []
                ]
            else
                []
        Nothing ->
            []
