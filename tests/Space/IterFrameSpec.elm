module Space.IterFrameSpec exposing (suite)

import Test as T
import Expect as E
import Set
import Color

import Space.IterFrame as IterFrame
import Space.TreeID as ID
import Space.Shape as Shape
import Geometry as G

suite : T.Test
suite = T.describe "module Space.IterFrame" [
        iterationTests
    ]

iterationTests : T.Test
iterationTests = T.describe "iteration functionality" [
        basicIteration
      , hiddenLayerTests
    ]

basicIteration : T.Test
basicIteration = T.test "iterateShapes produces expected number of shapes with no hidden layers" <|
    \_ ->
        let
            mode = { initMode | depth = 3 }
            startShape = (ID.Trunk "start", dummyShape)
            iterFrame = (ID.Trunk "f1", dummyFrame)
            result = IterFrame.iterateShapes mode [iterFrame] [startShape]
        in
            E.equal (List.length result) 3  -- One shape for each depth

hiddenLayerTests : T.Test
hiddenLayerTests = T.describe "hidden layer functionality" [
        T.test "hiding middle layer removes those shapes" <|
            \_ ->
                let
                    mode = { initMode | 
                        depth = 3,
                        hiddenLayers = Set.singleton 1  -- Hide middle layer
                        }
                    startShape = (ID.Trunk "start", dummyShape)
                    iterFrame = (ID.Trunk "f1", dummyFrame)
                    result = IterFrame.iterateShapes mode [iterFrame] [startShape]
                in
                    E.equal (List.length result) 2  -- First and last layer only
      , T.test "hiding all layers produces empty list" <|
            \_ ->
                let
                    mode = { initMode | 
                        depth = 3,
                        hiddenLayers = Set.fromList [1, 2, 3]
                        }
                    startShape = (ID.Trunk "start", dummyShape)
                    iterFrame = (ID.Trunk "f1", dummyFrame)
                    result = IterFrame.iterateShapes mode [iterFrame] [startShape]
                in
                    E.equal result []
    ]

-- Helper definitions for testing
initMode : IterFrame.Mode
initMode = IterFrame.initMode

dummyShape : Shape.Def
dummyShape = 
    { fill = Color.black
    , geoDef = G.Polygon [G.point 0 0, G.point 1 0, G.point 0 1]
    }

dummyFrame : IterFrame.Def
dummyFrame = IterFrame.identityDef
