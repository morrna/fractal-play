module Space.IterFrameSpec exposing (suite)

import Test as T
import Expect as E
import Fuzz as F
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
      , modeUpdateTests
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
        T.fuzz (F.intRange 0 20) "hiding all but last layer produces just one layer" <|
            \depth ->
                let
                    mode = { initMode | 
                        depth = depth,
                        onlyShowLastLayer = True
                        }
                    startShape = (ID.Trunk "start", dummyShape)
                    iterFrame = (ID.Trunk "f1", dummyFrame)
                    result = IterFrame.iterateShapes mode [iterFrame] [startShape]
                in
                    E.equal (List.length result)
                        -- The base shape is not handled by iterateShapes,
                        -- so we need to account for that.
                        (if depth == 0 then 0 else 1)
    ]

modeUpdateTests : T.Test
modeUpdateTests = T.describe "mode update functionality" [
        T.describe "updateIterationDepth" [
            T.fuzz2 (F.intRange 0 10) (F.intRange -3 3)
                "Result depth updated with change argument" <|
                \oldDepth randChange ->
                    let
                        -- The button prevents the depth from going below 0,
                        -- so let's simulate only changes in the allowed range.
                        change = if randChange + oldDepth < 0
                            then -oldDepth
                            else randChange
                        mode = { initMode | depth = oldDepth }
                        result = IterFrame.updateIterationDepth change mode
                    in
                        E.equal result.depth (oldDepth + change)
          , T.fuzz3 (F.intRange 0 10) (F.intRange -3 3) (F.bool)
                "Hidden layer setting same after layer change if they started empty" <|
                \oldDepth randChange startSetting ->
                    let
                        change = if randChange + oldDepth < 0
                            then -oldDepth
                            else randChange
                        mode = { initMode | depth = oldDepth, onlyShowLastLayer = startSetting }
                        result = IterFrame.updateIterationDepth change mode
                    in
                        E.equal result.onlyShowLastLayer startSetting
        ]
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
