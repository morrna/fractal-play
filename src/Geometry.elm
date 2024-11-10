module Geometry exposing (
        Point
      , point
      , pointFromPair
      , x
      , y
      , Displacement
      , disp
      , mag
      , pointsDisp
      , shift
      , scaleDisp
      , nGon
      , Definition(..)
      , translateFromAnchor
      , Transform
      , makeBasisTransform
      , makeAffineTransform
      , applyToDef
      , RelativeLocation(..)
      , getRelativeLocation
      , apply
      , unapply
      , rotateOrigin
      , rotationFromDisplacements
      , scaleFromDisplacements
    )

{- Maybe it's overkill, but the alt-linear-algebra package doesn't contain a Matrix3 option,
   so I'm going to embed everything in 3D space. Maybe someday I'll fork this package and implement
   the version I need, but this will work just fine for now. -}
import AltMath.Vector3 as V
import AltMath.Matrix4 as M
import List as L
import Maybe

type alias Point = V.Vec3
type alias Displacement = V.Vec3

point : Float -> Float -> Point
point xVal yVal = V.vec3 xVal yVal 0

pointFromPair : (Float, Float) -> Point
pointFromPair = uncurry2 point

uncurry2 : (a -> b -> c) -> (a, b) -> c
uncurry2 f (v, w) = f v w

x : Point -> Float
x = V.getX

y : Point -> Float
y = V.getY

{-| Create a displacement from X and Y components -}
disp : Float -> Float -> Displacement
disp xVal yVal = V.vec3 xVal yVal 0

mag : Displacement -> Float
mag = V.length

{-| Get the displacement from the second point to the first.
    (Works like vector subtraction.) -}
pointsDisp : Point -> Point -> Displacement
pointsDisp = V.sub

{-| Translate a point by the provided displacement -}
shift : Point -> Displacement -> Point
shift = V.add


{-| Scale a displacement by a scalar -}
scaleDisp : Float -> Displacement -> Displacement
scaleDisp = V.scale

{-| Points defining a regular polygon.

    1. Number of sides
    2. Radius
    3. Rotation angle for first vertex from +x axis
    4. Center point
 -}
nGon : Int -> Float -> Float -> Point -> Definition
nGon n radius theta center = Polygon <| L.map
    (V.add center << rotateOrigin theta)
    (nGonCentered n radius)

nGonCentered : Int -> Float -> List (Point)
nGonCentered n radius = L.map
    (V.scale radius << (liftA2 point cos sin) << turns << (\i -> i /(toFloat n)) << toFloat)
    (L.range 0 (n-1))

liftA2 : (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
liftA2 f g h v = f (g v) (h v)

{-| Geometrical definitions of shapes -}
type Definition
    = Polygon (List (Point))
    | Circle Point Float

{-| Translate definitions, getting the displacement from an anchor position (arg 1) to a target position (arg 2) -}
translateFromAnchor : Point -> Point -> Definition -> Definition
translateFromAnchor anchor target = translate (V.sub target anchor)

translate : Displacement -> Definition -> Definition
translate d def = case def of
    Polygon points -> Polygon <| L.map (V.add d) points
    Circle center radius -> Circle (V.add d center) radius

{- ## Transforms -}

type alias Transform = M.Mat4

{-| Get a transform for a rotation about the origin given an angle in radians -}
rotateT : Float -> Transform
rotateT theta = M.makeRotate theta <| V.Vec3 0 0 1

{-| apply a transform to a point or displacement -}
apply : Transform -> Point -> Point
apply = M.transform

{-| undo a transform to a point or displacement (inverse) -}
unapply : Transform -> Point -> Point
unapply = M.transform << Maybe.withDefault M.identity << M.inverse

{-| Rotate a point about the origin. May also be used to rotate displacements.
    First argument is the angle in radians. -}
rotateOrigin : Float -> Point -> Point
rotateOrigin = apply << rotateT

{-| Make a linear transform from the provided basis vectors.
    The first argument is the x unit vector under the transform,
    and the second is the y unit vector. -}
makeBasisTransform : Displacement -> Displacement -> Transform
makeBasisTransform xunit yunit = M.makeBasis xunit yunit <| V.vec3 0 0 1

{-| Make the transform for translating by the given displacement. -}
makeTranslationTransform : Displacement -> Transform
makeTranslationTransform = M.makeTranslate

{-| Make an affine transform defined by first applying the linear
    map given by the basis vectors (first two arguments) and then
    translating (third argument) -}
makeAffineTransform
    : Displacement -- x basis
    -> Displacement -- x basis
    -> Displacement -- offset
    -> Transform
makeAffineTransform xunit yunit offset
    = M.mulAffine (makeTranslationTransform offset) (makeBasisTransform xunit yunit)

applyToDef : Transform -> Definition -> Definition
applyToDef t d = case d of
    Polygon pts -> Polygon (List.map (apply t) pts)
    Circle c r -> Circle c r {- do nothing is the closest thing to 'undefined' I can find in Elm -}

type RelativeLocation
    = TopLeftCorner
    | TopRightCorner
    | BottomLeftCorner
    | BottomRightCorner

getRelativeLocation
    : RelativeLocation -> Definition -> Point
getRelativeLocation relLoc = relLocFromPoints relLoc << getRelativePoints

getRelativePoints
    : Definition
    {- List of x values, list of y values -}
    -> (List Float, List Float)
getRelativePoints d
    = case d of
        Polygon pts -> (L.map x pts, L.map y pts)
        Circle c r -> ([x c - r, x c, x c + r], [y c - r, y c, y c + r])

relLocFromPoints
    : RelativeLocation
    {- List of x values, list of y values -}
    -> (List Float, List Float)
    -> Point
relLocFromPoints relLoc (xvals, yvals)
    = case relLoc of
        TopLeftCorner -> point (simpMinimum xvals) (simpMinimum yvals)
        TopRightCorner -> point (simpMaximum xvals) (simpMinimum yvals)
        BottomLeftCorner -> point (simpMinimum xvals) (simpMaximum yvals)
        BottomRightCorner -> point (simpMaximum xvals) (simpMaximum yvals)

simpMinimum : List Float -> Float
simpMinimum
    = Maybe.withDefault (1/0) << L.minimum

simpMaximum : List Float -> Float
simpMaximum
    = Maybe.withDefault (-1/0) << L.maximum

rotationFromDisplacements
    : Displacement {- rotation from this vector -}
   -> Displacement {- rotation to this vector -}
   -> Transform    {- transform for rotation -}
rotationFromDisplacements a b
    = let
        aMag = V.length a
        bMag = V.length b
        prodMags = aMag * bMag
        cos = (V.dot a b) / prodMags
        {- This next line assumes these are 2D, in case that's not obvious -}
        sin = (V.cross a b).z / prodMags
    in
        makeBasisTransform (disp cos sin) (disp (-sin) cos)

{-| Scale evenly, using the projection of the second displacement on the first
    to get the scale factor. -}
scaleFromDisplacements
    : Displacement
    -> Displacement
    -> Transform
scaleFromDisplacements a b
    = let
        scale = V.dot a b / (V.length a)^2
    in
        M.makeScale3 scale scale 1
