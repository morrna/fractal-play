module Space.Frame exposing (
        Def(..)
      , width
      , height
      , viewBoxString
      , corners
    )

import Geometry as G

{-| Ways to define the dimensions of the outer frame (i.e. SVG element)

  - `Rectangle width height` does what it says
  - `Golden height` gives a golden rectangle with the given height  -}
type Def
    = Rectangle Float Float
    | Golden Float

{-| Get the width of a frame -}
width : Def -> Float
width rd = case rd of
    Rectangle w _ -> w
    Golden h -> 1.618034 * h

{-| Get the height of a frame -}
height : Def -> Float
height rd = case rd of
    Rectangle _ h -> h
    Golden h -> h

viewBoxString : Def -> String
viewBoxString d = String.join " " <| List.map String.fromFloat [
        -(width d)/2
      , -(height d)/2
      , width d
      , height d
    ]

{-| Get the corners of the frame's rectangle as a polygon definition. -}
corners : Def -> G.Definition
corners d
    = let
        dx = width d / 2
        dy = height d / 2
    in
        G.Polygon [
                G.point dx dy
              , G.point -dx dy
              , G.point -dx -dy
              , G.point dx -dy
          ]
