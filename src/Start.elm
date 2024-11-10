module Start exposing (
        Which(..)
      , get
      , sierpinskiTransform
    )

import Color as Clr

import Space
import Space.Frame as Frame
import Space.TreeID as ID
import Space.Content as Content
import Space.Shape as Shape
import Space.IterFrame as IterFrame
import Geometry as G

type Which
    = Sierpinski
    | Dragon

get : Which -> Space.Model
get which =
    case which of
        Sierpinski -> sierpinski
        Dragon -> dragon

setDefaultIterFrame : IterFrame.Def -> Space.Model -> Space.Model
setDefaultIterFrame ifDef model =
    { model | defaultIterFrame = ifDef }

setReferenceFrame : Maybe Frame.Def -> Space.Model -> Space.Model
setReferenceFrame refFrame model =
    { model | referenceFrame = refFrame }

{- ## Sierpinski triangle -}

sierpinski : Space.Model
sierpinski
    = Space.addIterFrame (ID.Trunk "f3")
        (\m -> sierpinskiThird m.outerFrame)
        <| Space.addIterFrame (ID.Trunk "f2")
            (\m -> sierpinskiSecond m.outerFrame)
        <| Space.addIterFrame (ID.Trunk "f1")
            (\m -> sierpinskiFirst m.outerFrame)
        <| Space.addContentToModel (ID.Trunk "start")
            (\m id -> Content.makeShape id <| sierpinskiStartingTriangle m.outerFrame)
        <| setDefaultIterFrame sierpinskiTransform
        <| Space.emptyModel <| Frame.Golden 800

sierpinskiStartingTriangle : Frame.Def -> Shape.Def
sierpinskiStartingTriangle rdef
    = let
        h = Frame.height rdef
    in
        { fill = blue
            , geoDef = G.nGon 3 (2*h/3) (turns (1/12)) <| G.point 0 (h/6)
        }

sierpinskiTransform : IterFrame.Def
sierpinskiTransform = { xBasis = G.disp 0.5 0, yBasis = G.disp 0 0.5, offset = G.disp 0 0 }

sierpinskiFirst : Frame.Def -> IterFrame.Def
sierpinskiFirst frame
    = { sierpinskiTransform |
        offset = G.disp 0 (negate (Frame.height frame) / 4)
    }

sierpinskiSecond : Frame.Def -> IterFrame.Def
sierpinskiSecond frame
    = { sierpinskiTransform |
        offset = G.disp (negate (Frame.height frame)/(2 * tan (pi/3))) (Frame.height frame/4)
    }

sierpinskiThird : Frame.Def -> IterFrame.Def
sierpinskiThird frame
    = { sierpinskiTransform |
        offset = G.disp (Frame.height frame/(2 * tan (pi/3))) (Frame.height frame/4)
    }

blue : Clr.Color
blue = Clr.rgb 0 0 1

{- ## Dragon curve -}

dragon : Space.Model
dragon
    = Space.addIterFrame (ID.Trunk "f2")
        (\m -> dragonSecond m.outerFrame)
        <| Space.addIterFrame (ID.Trunk "f1")
            (\m -> dragonFirst m.outerFrame)
        <| Space.addContentToModel (ID.Trunk "start")
            (\m id -> Content.makeShape id <| dragonStarter m.outerFrame)
        <| setDefaultIterFrame dragonDefaultTransform
        <| setReferenceFrame (Just <| Frame.Golden 400)
        <| Space.emptyModel <| Frame.Golden 800

{-| Get the unit of scale best for the dragon curve. -}
dragonScale : Frame.Def -> Float
dragonScale frame = Frame.height frame / 5

dragonFirst : Frame.Def -> IterFrame.Def
dragonFirst frame
    = let
        unit = dragonScale frame
    in
        {
            xBasis = G.disp 0.5 (-0.5), yBasis = G.disp 0.5 0.5
          , offset = G.disp (-1.5 * unit) (-0.5 * unit)
        }

dragonSecond : Frame.Def -> IterFrame.Def
dragonSecond frame
    = let
        unit = dragonScale frame
    in
        {
            xBasis = G.disp (-0.5) (-0.5), yBasis = G.disp 0.5 (-0.5)
          , offset = G.disp (0.5 * unit) (0.5 * unit)
        }

dragonStarter : Frame.Def -> Shape.Def
dragonStarter frame
    = let
        unit = dragonScale frame
    in
        {
            fill = blue
          , geoDef = G.Polygon
            [
                G.point (2.5 * unit) (unit * 3 / 2)
              , G.point (-2.5 * unit) (unit * 3 / 2)
              , G.point (-2.5 * unit) (unit / 2)
              , G.point (2.5 * unit) (unit / 2)
            ]
        }

dragonDefaultTransform
    : IterFrame.Def
dragonDefaultTransform
    = {
        xBasis = G.rotateOrigin (turns 1/8) <| G.disp 0.5 (-0.5)
      , yBasis = G.rotateOrigin (turns 1/8) <| G.disp 0.5 0.5
      , offset = G.disp 0 0
    }
