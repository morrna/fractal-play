module Space.Content exposing (
        Content
      , makeShape
      , makeIterFrame
      , showContent
      , getAllToShow
      , appendNew
      , numIterFrames
      , drop
      , getIterFrameIDs
      , Selected
      , action
    )

import Svg.Styled as S
import Set

import Space.TreeID as ID
import Space.Shape as Shape
import Space.Frame as Frame
import Space.IterFrame as IterFrame
import Space.Move as Move

{-| A unit of content for the space, which always has an ID. -}
type alias Content
    = {def: ContentDef, id: ID.TreeID}

{-| Definitions for pieces of content -}
type ContentDef
    = Shape Shape.Def
    {- Iterframes need to refer to the outer frame definition, plus they have their own definition,
        which is a transform. -}
    | IterFrame Frame.Def IterFrame.Def
    | IterShape Shape.Def

{-| Create a piece of content holding a Shape -}
makeShape
    : ID.TreeID
   -> Shape.Def
   -> Content
makeShape id shape
    = {id = id, def = Shape shape}

{-| Create a piece of content holding an IterFrame -}
makeIterFrame
    : ID.TreeID
   -> Frame.Def
   -> IterFrame.Def
   -> Content
makeIterFrame id frame iter
    = {id = id, def = IterFrame frame iter}

{-| Given definition info for a Shape, set the content to be an Iterframe, preserving ID. -}
setShape
    : Shape.Def
   -> Content -> Content
setShape
    = setDef << Shape

{-| Given definition info for an IterFrame, set the content to be an Iterframe, preserving ID. -}
setIterFrame
    : Frame.Def
   -> IterFrame.Def
   -> Content -> Content
setIterFrame frame iter
    = setDef <| IterFrame frame iter

{-| Set a content's def keeping ID the same -}
setDef
    : ContentDef
   -> Content
   -> Content
setDef newDef content
    = { content | def = newDef }

{-| Display the content in the space.

    arg 1: Shape event generator
    arg 2: IterFrame event generator
 -}
showContent
    : (Selected -> List (S.Attribute msg))
   -> Content
   -> S.Svg msg
showContent atts c
    = case c.def of
        Shape s
            -> Shape.show (atts { content = c, action = Translate}) s
        IterFrame outerFrame iter
            -> IterFrame.show
                outerFrame
                (\tool -> atts { content = c, action = IterFrameTool tool })
                iter
        IterShape s -> Shape.show [] s

{- ## Making nodes for keyed lazy show upstairs -}

{-| Get all Content pieces to pass to the show function, including iterated shapes. -}
getAllToShow
    : IterFrame.Mode
   -> List Content
   -> List (String, Content)
getAllToShow mode contents
    = (List.map contentToIDPair) <| (appendIterShapes mode) <| contents

contentToIDPair : Content -> (String, Content)
contentToIDPair c = (ID.toString c.id, c)


{- ## Content IterFrame interface -}

{-| Given a mode, list of base shapes, and IterFrames, return the full list with all of that
    plus iterated shapes. -}
appendIterShapes
    : IterFrame.Mode
   -> List Content
   -> List Content
appendIterShapes mode contents
    = let
        (shapes, shapeDefs) = getShapeDefs contents
        (iterFrames, iterFrameDefs) = getIterFrameDefs contents
        iterShapes
            = List.map makeIterShape
                <| IterFrame.iterateShapes mode iterFrameDefs shapeDefs
        allShapes
            = if Set.member 0 mode.hiddenLayers
                then iterShapes
                else shapes ++ iterShapes
    in
        if mode.showIterFrames
        then allShapes ++ iterFrames
        else allShapes


getIterFrameDefs : List Content -> (List Content, List (ID.TreeID, IterFrame.Def))
getIterFrameDefs = List.unzip << List.filterMap tryIterFrameDef

tryIterFrameDef
    : Content
   -> Maybe (Content, (ID.TreeID, IterFrame.Def))
tryIterFrameDef c = case c.def of
    IterFrame _ d -> Just (c, (c.id, d))
    _ -> Nothing

getShapeDefs : List Content -> (List Content, List (ID.TreeID, Shape.Def))
getShapeDefs = List.unzip << List.filterMap tryShapeDef

tryShapeDef
    : Content
   -> Maybe (Content, (ID.TreeID, Shape.Def))
tryShapeDef c = case c.def of
    Shape s -> Just (c, (c.id, s))
    _ -> Nothing

makeIterShape : (ID.TreeID, Shape.Def) -> Content
makeIterShape (id, sdef) = {id = id, def = IterShape sdef}

{-| Add a new piece of content to a list using the provided constructor. -}
appendNew
    : (ID.TreeID -> Content)
   -> ID.TreeID
   -> List Content
   -> List Content
appendNew newContentGen newID cl
    = cl ++ [(newContentGen newID)]

{-| Count the number of IterFrames in a list of Content. -}
numIterFrames : List Content -> Int
numIterFrames = List.length << List.filterMap tryIterFrameDef

{-| Drop the content with the given ID from the list. -}
drop : ID.TreeID -> List Content -> List Content
drop = ID.drop (.id)

{-| Get all IterFrame IDs from a list of Content. -}
getIterFrameIDs : List Content -> List ID.TreeID
getIterFrameIDs
    = let tryIterFrameID c = case c.def of
            IterFrame _ _ -> Just c.id
            _ -> Nothing
    in List.filterMap tryIterFrameID

{-| A container for selected content, plus any info about what action the selection is for. -}
type alias Selected
    = { content: Content, action: SelectedAction }

{-| Actions that can be taken on selected content. -}
type SelectedAction
    = Translate
    | IterFrameTool IterFrame.Tool

{-| Get the action to take on the selected content. -}
action
    : Move.Info Selected
   -> Content
   -> Content
action mi
    = let
        a = mi.payload.action
        miWithDef = Move.mapPayload (.def << .content) mi
        newContentDef = case a of
            Translate -> translateDef miWithDef
            IterFrameTool t -> actOnIterFrame t miWithDef
        newContent = setDef newContentDef mi.payload.content
    in
        ID.switch (.id) newContent

{-| Translate a content definition. -}
translateDef
    : Move.Info ContentDef
   -> ContentDef
translateDef mi
    = case mi.payload of
        Shape s -> Shape <| Shape.translate <| Move.mapPayload (always s) mi
        _ -> mi.payload -- future: Implement for other types of content

actOnIterFrame
    : IterFrame.Tool
   -> Move.Info ContentDef
   -> ContentDef
actOnIterFrame t mi
    = case mi.payload of
        IterFrame frameDef ifDef
            -> IterFrame frameDef
                <| IterFrame.update frameDef t
                    <| Move.mapPayload
                        (always ifDef)
                        mi
        _ -> mi.payload
