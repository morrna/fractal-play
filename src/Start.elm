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
    | SierpinskiCarpet

get : Which -> Space.Model
get which =
    case which of
        Sierpinski -> sierpinski
        Dragon -> dragon
        SierpinskiCarpet -> sierpinskiCarpet

setDefaultIterFrame : IterFrame.Def -> Space.Model -> Space.Model
setDefaultIterFrame ifDef model =
    { model | defaultIterFrame = ifDef }

{- ## Sierpinski triangle -}

sierpinski : Space.Model
sierpinski
    = Space.addIterFrame (ID.Trunk "f3")
        (\m -> sierpinskiThird m.referenceFrame)
        <| Space.addIterFrame (ID.Trunk "f2")
            (\m -> sierpinskiSecond m.referenceFrame)
        <| Space.addIterFrame (ID.Trunk "f1")
            (\m -> sierpinskiFirst m.referenceFrame)
        <| Space.addContentToModel (ID.Trunk "start")
            (\m id -> Content.makeShape id <| sierpinskiStartingTriangle m.referenceFrame)
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
        (\_ -> dragonSecond Space.outerFrame)
        <| Space.addIterFrame (ID.Trunk "f1")
            (\_ -> dragonFirst Space.outerFrame)
        <| Space.addContentToModel (ID.Trunk "start")
            (\_ id -> Content.makeShape id <| dragonStarter Space.outerFrame)
        <| setDefaultIterFrame dragonDefaultTransform
        <| Space.emptyModel <| Frame.Golden 400

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

{- ## Sierpinski Carpet -}

sierpinskiCarpet : Space.Model
sierpinskiCarpet =
    Space.addIterFrame (ID.Trunk "f8")
        (\m -> carpetFrameTopRight m.referenceFrame)
        <| Space.addIterFrame (ID.Trunk "f7")
            (\m -> carpetFrameTopCenter m.referenceFrame)
        <| Space.addIterFrame (ID.Trunk "f6")
            (\m -> carpetFrameTopLeft m.referenceFrame)
        <| Space.addIterFrame (ID.Trunk "f5")
            (\m -> carpetFrameMiddleRight m.referenceFrame)
        <| Space.addIterFrame (ID.Trunk "f4")
            (\m -> carpetFrameMiddleLeft m.referenceFrame)
        <| Space.addIterFrame (ID.Trunk "f3")
            (\m -> carpetFrameBottomRight m.referenceFrame)
        <| Space.addIterFrame (ID.Trunk "f2")
            (\m -> carpetFrameBottomCenter m.referenceFrame)
        <| Space.addIterFrame (ID.Trunk "f1")
            (\m -> carpetFrameBottomLeft m.referenceFrame)
        <| Space.addContentToModel (ID.Trunk "start")
            (\m id -> Content.makeShape id <| sierpinskiCarpetStartingSquare m.referenceFrame)
        <| Space.setIterationDepth 3
        <| setDefaultIterFrame sierpinskiCarpetTransform
        <| Space.emptyModel <| carpetReferenceFrame

{-| Get the length of the side of the starting square for the Sierpinski carpet. -}
carpetSquareLength : Frame.Def -> Float
carpetSquareLength frame = Frame.height frame

{-| Common function to calculate the scale for Sierpinski Carpet -}
carpetScale : Frame.Def -> Float
carpetScale frame =
    carpetSquareLength frame / 3  -- Scaling to 1/3 of the frame's height

carpetReferenceFrame : Frame.Def
carpetReferenceFrame =
    Frame.Rectangle
        (carpetSquareLength Space.outerFrame)
        (carpetSquareLength Space.outerFrame)

{-| Get the starting square for the Sierpinski carpet. -}
sierpinskiCarpetStartingSquare : Frame.Def -> Shape.Def
sierpinskiCarpetStartingSquare rdef =
    let
        size = carpetSquareLength rdef
    in
        { fill = blue
            , geoDef = G.Polygon
                [
                    G.point (-size / 2) (-size / 2)
                  , G.point (size / 2) (-size / 2)
                  , G.point (size / 2) (size / 2)
                  , G.point (-size / 2) (size / 2)
                ]
        }

sierpinskiCarpetTransform : IterFrame.Def
sierpinskiCarpetTransform =
    { xBasis = G.disp (1 / 3) 0    -- Fixed scaling ratio
    , yBasis = G.disp 0 (1 / 3)    -- Fixed scaling ratio
    , offset = G.disp 0 0            -- Base offset
    }

-- Top Left IterFrame
carpetFrameTopLeft : Frame.Def -> IterFrame.Def
carpetFrameTopLeft frame =
    let
        scale = carpetScale frame
    in
        { sierpinskiCarpetTransform
            | offset = G.disp (-scale) scale
        }

-- Top Center IterFrame
carpetFrameTopCenter : Frame.Def -> IterFrame.Def
carpetFrameTopCenter frame =
    let
        scale = carpetScale frame
    in
        { sierpinskiCarpetTransform
            | offset = G.disp 0 scale
        }

-- Top Right IterFrame
carpetFrameTopRight : Frame.Def -> IterFrame.Def
carpetFrameTopRight frame =
    let
        scale = carpetScale frame
    in
        { sierpinskiCarpetTransform
            | offset = G.disp scale scale
        }

-- Middle Left IterFrame
carpetFrameMiddleLeft : Frame.Def -> IterFrame.Def
carpetFrameMiddleLeft frame =
    let
        scale = carpetScale frame
    in
        { sierpinskiCarpetTransform
            | offset = G.disp (-scale) 0
        }

-- Middle Right IterFrame
carpetFrameMiddleRight : Frame.Def -> IterFrame.Def
carpetFrameMiddleRight frame =
    let
        scale = carpetScale frame
    in
        { sierpinskiCarpetTransform
            | offset = G.disp scale 0
        }

-- Bottom Left IterFrame
carpetFrameBottomLeft : Frame.Def -> IterFrame.Def
carpetFrameBottomLeft frame =
    let
        scale = carpetScale frame
    in
        { sierpinskiCarpetTransform
            | offset = G.disp (-scale) (-scale)
        }

-- Bottom Center IterFrame
carpetFrameBottomCenter : Frame.Def -> IterFrame.Def
carpetFrameBottomCenter frame =
    let
        scale = carpetScale frame
    in
        { sierpinskiCarpetTransform
            | offset = G.disp 0 (-scale)
        }

-- Bottom Right IterFrame
carpetFrameBottomRight : Frame.Def -> IterFrame.Def
carpetFrameBottomRight frame =
    let
        scale = carpetScale frame
    in
        { sierpinskiCarpetTransform
            | offset = G.disp scale (-scale)
        }
