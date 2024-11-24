module Space.IterFrame exposing (
        Def
      , show
      , showKey
      , Mode
      , initMode
      , iterateShapes
      , Tool(..)
      , update
      , identityDef
      , setIterationDepth
      , updateIterationDepth
      , updateOnlyShowLastLayer
    )

import Svg.Styled as S
import Svg.Styled.Attributes as SA
import List
import Set
import Color as C

import Geometry as G
import Space.Frame as Frame
import Space.Shape as Shape
import Space.TreeID as ID
import Space.Move as Move

{-| The definition for an IterFrame is what's needed to make its transform:
    two independent basis vectors and an offset from the origin. -}
type alias Def = {
        xBasis: G.Displacement
      , yBasis: G.Displacement
      , offset: G.Displacement
    }

{-| Create the SVG to display the IterFrame. The first argument is the definition
    of the outer frame, so that the rectangle displayed illustrates what happens
    to everything in the frame under the transformation. -}
show
    : Frame.Def                             {- Geometry of canvas frame -}
    -> (Tool -> List (S.Attribute msg))  {- Function to make attributes to be added to shapes (for events) -}
    -> Def                                  {- State of this IterFrame -}
    -> S.Svg msg
show frame makeAtts def
    = S.g []
        [
            showFrameBox frame def <| makeAtts Translate
          , showPosHandle frame def <| makeAtts Translate
          , showRotateHandle frame def <| makeAtts Rotate
          , showScaleHandle frame def <| makeAtts Scale
          , showSkewHandle frame def <| makeAtts Skew
        ]

showFrameBox
    : Frame.Def -> Def -> List (S.Attribute msg) -> S.Svg msg
showFrameBox frame def atts
    = Shape.showGeoDef
        ((SA.stroke "red")::(SA.strokeWidth "4")::(SA.fill "none")::atts)
        (iterFrameGeoDef frame def)

iterFrameGeoDef
    : Frame.Def -> Def -> G.Definition
iterFrameGeoDef frame def
    = G.applyToDef (getTransform def) (Frame.corners frame)

{-| Gets the canvas location of the corner point indicated by the enum value,
    where the enum value is interpreted relative to the outer frame.
    This means the points will be attached to the corners of the IterFrame
    as it rotates and skews. -}
cornerPoint
    : Frame.Def
   -> Def
   -> G.RelativeLocation
   -> G.Point
cornerPoint frameDef ifDef rl
    = G.apply (getTransform ifDef) <| G.getRelativeLocation rl (Frame.corners frameDef)

showPosHandle
    : Frame.Def -> Def -> List (S.Attribute msg) -> S.Svg msg
showPosHandle frame def atts
    = Shape.showGeoDef
        (atts ++ [SA.fill "grey"])
        (G.Circle (cornerPoint frame def G.TopLeftCorner) 12)

showRotateHandle
    : Frame.Def -> Def -> List (S.Attribute msg) -> S.Svg msg
showRotateHandle frameDef ifDef atts
    = Shape.showGeoDef
        (atts ++ [SA.fill "cyan"])
        (G.Circle (cornerPoint frameDef ifDef G.TopRightCorner) 12)

showScaleHandle
    : Frame.Def -> Def -> List (S.Attribute msg) -> S.Svg msg
showScaleHandle frameDef ifDef atts
    = Shape.showGeoDef
        (atts ++ [SA.fill "gold"])
        (G.Circle (cornerPoint frameDef ifDef G.BottomRightCorner) 12)

showSkewHandle
    : Frame.Def -> Def -> List (S.Attribute msg) -> S.Svg msg
showSkewHandle frameDef ifDef atts
    = Shape.showGeoDef
        (atts ++ [SA.fill "magenta"])
        (G.Circle (cornerPoint frameDef ifDef G.BottomLeftCorner) 12)

{-| Show a simple IterFrame to serve as a key explaining the IterFrame controls. -}
showKey
    : S.Svg msg
showKey
    = S.g []
        [
            show
                (Frame.Rectangle 36 36)
                (\_ -> [])
                {identityDef | offset = G.disp 50 50}
        ]


getTransform : Def -> G.Transform
getTransform d = G.makeAffineTransform d.xBasis d.yBasis d.offset

{-| Settings for the iteration -}
type alias Mode
    = {
        depth: Int
      , hueShift : Float
      , showIterFrames : Bool
        -- Layers to hide
        -- Convention: 1 is the first iteration
        --   0 represents the base shapes
      , onlyShowLastLayer : Bool
    }

{-| Default mode for initializing -}
initMode
    : Mode
initMode
    = {depth = 6, hueShift = (0.618034 * 8/7), showIterFrames = True, onlyShowLastLayer = False}

getHiddenLayers : Mode -> Set.Set Int
getHiddenLayers mode = if mode.onlyShowLastLayer
    then allButLastLayerHidden mode.depth
    else Set.empty


iterateShapes
    : Mode
    -> List (ID.TreeID, Def)
    -> List (ID.TreeID, Shape.Def)
    -> List (ID.TreeID, Shape.Def)
iterateShapes mode ifdefs sdefs
    = iterateShapesWithHiding mode mode.depth 1 (getHiddenLayers mode) ifdefs sdefs

iterateShapesWithHiding
    : Mode
    -> Int -- current depth
    -> Int -- current index
    -> Set.Set Int -- hidden layers
    -> List (ID.TreeID, Def)
    -> List (ID.TreeID, Shape.Def)
    -> List (ID.TreeID, Shape.Def)
iterateShapesWithHiding mode currentDepth currentIndex hiddenLayers ifdefs sdefs
    = if currentDepth <= 0
        then []
        else
            let
                newSdefs = singleIteration mode ifdefs sdefs
                newDepth = currentDepth - 1
                newIndex = currentIndex + 1
            in
                if Set.member currentIndex hiddenLayers then
                    iterateShapesWithHiding mode newDepth newIndex hiddenLayers ifdefs newSdefs
                else
                    newSdefs ++ (iterateShapesWithHiding mode newDepth newIndex hiddenLayers ifdefs newSdefs)

singleIteration
    : Mode
    -> List (ID.TreeID, Def)
    -> List (ID.TreeID, Shape.Def)
    -> List (ID.TreeID, Shape.Def)
singleIteration mode ifdefs
    = List.concatMap (genShapeIterators mode ifdefs)

genShapeIterators
    : Mode
    -> List (ID.TreeID, Def)
    -> (ID.TreeID, Shape.Def)
    -> List (ID.TreeID, Shape.Def)
genShapeIterators mode ifdefs (sid, sdef)
    = List.map
        (\(ifid, ifdef) -> (ID.graft ifid sid, transformShapeDef mode ifdef sdef))
        ifdefs

transformShapeDef
    : Mode
    -> Def
    -> Shape.Def
    -> Shape.Def
transformShapeDef mode ifdef sdef
    = { sdef |
        fill = shiftColor mode sdef.fill
      , geoDef = G.applyToDef (getTransform ifdef) sdef.geoDef
    }

shiftColor : Mode -> C.Color -> C.Color
shiftColor {hueShift} = C.fromHsla <<
    (\crec -> { crec |
            hue =
                let
                    total = crec.hue + hueShift
                in
                    total - toFloat (floor total)
        }
    )
    << C.toHsla


identityDef : Def
identityDef
    = {
        xBasis = G.disp 1 0
      , yBasis = G.disp 0 1
      , offset = G.disp 0 0
    }

{- ## Events and Updates -}

type Tool
    = None
    | Translate
    | Rotate
    | Scale
    | Skew
update
    : Frame.Def
   -> Tool
   -> Move.Info Def
   -> Def
update frameDef tool {anchor, current, payload}
    = case tool of
        None -> payload
        Translate -> doTranslate anchor current payload
        Rotate -> doTransformFromDisplacements
            G.rotationFromDisplacements
            G.TopRightCorner
            anchor current frameDef payload
        Scale -> doTransformFromDisplacements
            G.scaleFromDisplacements
            G.BottomRightCorner
            anchor current frameDef payload
        Skew -> doSkew frameDef anchor current payload
doTranslate
    : G.Point {- anchor -}
   -> G.Point {- current -}
   -> Def     {- def @ anchor -}
   -> Def
doTranslate anchor current def
    = let
        toolDisplacement = G.pointsDisp current anchor
    in
        { def |
            offset = G.shift def.offset toolDisplacement
        }

{-| Get anchor and current displacements from the center of the IterFrame. -}
centeredDisplacements
    : G.RelativeLocation {- location of control -}
   -> G.Point {- anchor mouse position -}
   -> G.Point {- current mouse position -}
   -> Frame.Def {- outer frame def -}
   -> Def     {- def @ anchor -}
   -> (G.Displacement, G.Displacement)
centeredDisplacements controlLoc anchor current frameDef def
    = let
        ifT = getTransform def
        center = G.apply ifT <| G.point 0 0
        corner = cornerPoint frameDef def controlLoc
        a = G.pointsDisp corner center
        c = G.pointsDisp (G.shift corner (G.pointsDisp current anchor)) center
    in
        (a, c)


doTransformFromDisplacements
    : (G.Displacement -> G.Displacement -> G.Transform) {- transform from displacements -}
    -> G.RelativeLocation {- location of control -}
    -> G.Point {- anchor -}
    -> G.Point {- current -}
    -> Frame.Def {- outer frame def -}
    -> Def     {- def @ anchor -}
    -> Def
doTransformFromDisplacements f controlLoc anchor current frameDef def
    = let
        (a, c) = centeredDisplacements controlLoc anchor current frameDef def
        t = f a c
    in
        { def | xBasis = G.apply t def.xBasis, yBasis = G.apply t def.yBasis}

doSkew
    : Frame.Def {- outer frame def -}
    -> G.Point {- anchor -}
    -> G.Point {- current -}
    -> Def
    -> Def
doSkew frameDef anchor current def
    = let
        toolDisplacement = G.pointsDisp current anchor
        yShift
            = G.scaleDisp
                {- This corresponds to the y component because the control point
                   is at the bottom left corner of the outer frame, so I'm hard coding
                   the relative location. -}
                (1/(G.y <| G.getRelativeLocation G.BottomLeftCorner (Frame.corners frameDef)))
                (toolDisplacement)
    in
        { def | yBasis = G.shift def.yBasis yShift }

{-| The layers to hide if only the last layer is shown. -}
allButLastLayerHidden : Int -> Set.Set Int
allButLastLayerHidden iterationDepth
    = Set.fromList (List.range 0 (iterationDepth - 1))

{-| Set the iteration depth on a mode. -}
setIterationDepth : Int -> Mode -> Mode
setIterationDepth depth mode = { mode | depth = depth }

{-| Update the iteration depth. 
    The first argument is the change in depth.
 -}
updateIterationDepth
    : Int -- ^ change in depth
   -> Mode
   -> Mode
updateIterationDepth change mode
    = { mode | depth = mode.depth + change }

{-| Update the hidden layers to show only the last layer. -}
updateOnlyShowLastLayer
    : Bool
    -> Mode
    -> Mode
updateOnlyShowLastLayer newOnlyShowLastLayer mode
    = { mode | onlyShowLastLayer = newOnlyShowLastLayer }
