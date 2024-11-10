module Space.FrameSpec exposing ( suite )

import Test as T
import Fuzz as F
import Expect as E


import Space.Frame as Frame
import Geometry as G

import Util as U

suite : T.Test
suite = T.describe "module Space.Frame" [
        cornerTests
    ]

cornerTests : T.Test
cornerTests = T.describe "corners functionality" [
        rectangleCorners
      , goldenCorners
    ]

rectangleCorners : T.Test
rectangleCorners = T.fuzz2 scalarDimension scalarDimension
    "`corners` gets correct corners from `Rectangle`"
    <| \w h ->
        let
            def = Frame.Rectangle w h
            corners = Frame.corners def
        in
            expectCornersWithin (E.Relative 0.00001)
                (centeredCorners w h)
                (corners)

goldenCorners : T.Test
goldenCorners = T.fuzz scalarDimension
    "`corners` gets correct corners from `Golden`"
    <| \h ->
        let
            def = Frame.Golden h
            corners = Frame.corners def
            w = (1 + sqrt 5)/2 * h
        in
            expectCornersWithin (E.Relative 0.00001)
                (centeredCorners w h)
                (corners)

scalarDimension : F.Fuzzer Float
scalarDimension = F.floatRange 0 50000

{- ## expectation builders -}
expectCornersWithin
    : E.FloatingPointTolerance
    -> G.Definition -- expected
    -> G.Definition -- actual
    -> E.Expectation
expectCornersWithin tol
    = U.compareGeoDefs (U.expectPointWithin tol)

{- ## Common cases -}
centeredCorners : Float -> Float -> G.Definition
centeredCorners w h
    = G.Polygon [
        G.point (w/2) (h/2)
      , G.point (-w/2) (h/2)
      , G.point (-w/2) (-h/2)
      , G.point (w/2) (-h/2)
      ]
