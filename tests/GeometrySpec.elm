module GeometrySpec exposing ( suite )

import Test as T
import Expect as E
import Fuzz as F

import AltMath.Vector3 as V
import AltMath.Matrix4 as M

import Geometry exposing (..)

import Util as U

suite : T.Test
suite = T.describe "module Geometry" [
        dPoint
      , transforms
    ]

dPoint : T.Test
dPoint = T.describe "area Points" [
        T.fuzz2 F.float F.float "`point` always sets hidden z value to zero"
            (\x y -> E.equal 0 <| V.getZ <| point x y)
      , T.fuzz2 U.saneFloat U.saneFloat "`x` gets x value from point" <|
            \xv yv -> floatSame xv <| x <| point xv yv
      , T.fuzz2 U.saneFloat U.saneFloat "`y` gets y value from point" <|
            \xv yv -> floatSame yv <| y <| point xv yv
    ]

floatSame : Float -> Float -> E.Expectation
floatSame = E.within <| E.AbsoluteOrRelative 0.00000001 0.00000001

transforms : T.Test
transforms = T.describe "area Transforms" [
        tMakeAffineTransform
      , tApplyToDefTri
      , tRotationFromDisplacements
      , tScaleFromDisplacements
    ]

tMakeAffineTransform : T.Test
tMakeAffineTransform
    = T.fuzz3 fDisp fDisp fDisp "`makeAffineTransform` sets basis vectors and offset to columns"
        <| \xunit yunit offset
            -> U.compareAllGetters floatSame
                [
                    .m11, .m12, .m13, .m14
                  , .m21, .m22, .m23, .m24
                  , .m31, .m32, .m33, .m34
                  , .m41, .m42, .m43, .m44
                ]
                (manualAffine xunit yunit offset)
                (makeAffineTransform xunit yunit offset)

manualAffine : Displacement -> Displacement -> Displacement -> M.Mat4
manualAffine xunit yunit offset
    = {
        m11=(x xunit), m12=(x yunit), m13=0, m14=(x offset)
      , m21=(y xunit), m22=(y yunit), m23=0, m24=(y offset)
      , m31=        0, m32=        0, m33=1, m34=         0
      , m41=        0, m42=        0, m43=0, m44=         1
    }

fDisp : F.Fuzzer Displacement
fDisp = F.map2 disp U.saneFloat U.saneFloat

tApplyToDefTri : T.Test
tApplyToDefTri
    = T.describe "cases for `applyToDef`" [
        T.fuzz2 fTri fTransform
        "`applyToDef` maps points as expected when applied to a triangle, fully fuzzed"
        <| \pts tr
            -> U.compareGeoDefs pointSame
                (Polygon (List.map (M.transform tr) pts))
                (applyToDef tr (Polygon pts))
        {- While the above test logically covers everything, I think humans would find it hard
           to grok. More readable cases follow. -}
      , T.fuzz fTri "triangle stays the same under identity transform"
        <| \pts
            -> U.compareGeoDefs pointSame
            (Polygon pts)
            (applyToDef M.identity (Polygon pts))
    ]

pointSame : Point -> Point -> E.Expectation
pointSame = U.expectPointWithin <| E.AbsoluteOrRelative 0.00000001 0.00000001

fPoint : F.Fuzzer Point
fPoint = F.map2 point U.saneFloat U.saneFloat

fTri : F.Fuzzer (List (Point))
fTri = F.map3 (\p1 p2 p3 -> [p1, p2, p3]) fPoint fPoint fPoint

fTransform : F.Fuzzer Transform
fTransform = F.map3 manualAffine fDisp fDisp fDisp

fNonZeroDisplacement : F.Fuzzer Displacement
fNonZeroDisplacement
    = F.map2
        (\mag theta
            -> M.transform (M.makeRotate theta (V.k)) (disp mag 0)
        )
        (F.floatRange 1e-4 1e4) (F.floatRange -10 10)

tRotationFromDisplacements : T.Test
tRotationFromDisplacements
    = T.describe "cases for rotationFromDisplacements" [
        T.test "static example from codepal" <| always (
                let
                    a = disp 1 0
                    b = disp 0 1
                    t = rotationFromDisplacements a b
                    p = point 2 3
                    rotatedPoint = apply t p
                in
                    E.all [
                            (\pt -> E.equal (x pt) (-3))
                          , (\pt -> E.equal (y pt) 2)
                        ]
                        rotatedPoint

            ) {- Codepal gets an A+ for correct logic and an F in understanding elm-test -}
      , T.fuzz2 fNonZeroDisplacement fNonZeroDisplacement
        "Rotating first displacement by transform yields a vector parallel to second"
        <| \a b
            -> let
                t = rotationFromDisplacements a b
                ta = apply t a
            in
                floatSame (ta.x * b.y) (ta.y * b.x)
                {- comparing to zero can work badly near the edges of large ranges,
                   because you can only compare to it absolutely, but that absolute value can still be
                   large. Doing this algebra to the cross definition gets us numbers that are
                   more guaranteed to be comparable in size. -}
      , T.fuzz2 fNonZeroDisplacement fNonZeroDisplacement
        "Rotating second displacement by inverse transform yields a vector parallel to first"
        <| \a b
            -> let
                t = rotationFromDisplacements a b
                tb = unapply t b
            in
                floatSame (a.x * tb.y) (a.y * tb.x)
    ]

tScaleFromDisplacements : T.Test
tScaleFromDisplacements = T.describe "cases for scaleFromDisplacements" [
        T.fuzz2 U.sanePositiveFloat fNonZeroDisplacement
        "Scale factor is the ratio of the components in X when the first displacement is in X"
        <| \x1 d2
            -> E.all [
                    floatSame (x d2 / x1) << (.m11)
                  , floatSame (x d2 / x1) << (.m22)
                ]
                <| scaleFromDisplacements (disp x1 0) d2
      , T.fuzz2 fNonZeroDisplacement fNonZeroDisplacement
        "The scale factor inverts when the displacements are switched"
        <| \a b
            -> let
                s1 = scaleFromDisplacements a b
                s2 = scaleFromDisplacements b a
                cos = V.dot a b / (V.length a * V.length b)
            in
                floatSame (cos / s1.m11) (s2.m11 / cos)
    ]
