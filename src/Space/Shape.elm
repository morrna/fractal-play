module Space.Shape exposing (
        Def
      , show
      , showGeoDef
      , translate
    )

import Svg.Styled as S
import Svg.Styled.Attributes as A
import Color as C

import Geometry as G
import Space.Move as Move

{-| Record used to fully define an SVG shape, including its geometry, styles, and identifiers. -}
type alias Def = {
        fill: C.Color
      , geoDef: G.Definition
    }

{-| Create the SVG for a basic shape. These are shown with fill and no border
    for ease of use with transforms.
    Attributes passed in with the first argument are appended
    after the attributes coming from the definition. -}
show : List (S.Attribute msg) -> Def -> S.Svg msg
show atts s = showGeoDef ((A.fill <| C.toCssString s.fill)::atts) s.geoDef

{-| Create an SVG element just for a `Geometry.Definition` with no other styles.
    This is exposed for other submodules to use. -}
showGeoDef : List (S.Attribute msg) -> G.Definition -> S.Svg msg
showGeoDef atts gd
    = case gd of
        G.Polygon ps -> S.polygon
            ((A.points <| repPoints ps)::atts)
            []

        G.Circle center radius -> S.circle
            (
                (floatify A.cx <| G.x center)
                ::(floatify A.cy <| G.y center)
                ::(floatify A.r radius)
                ::atts
            )
            []

repPoints : List (G.Point) -> String
repPoints ps = case ps of
    [] -> ""
    p::[] -> repPoint p
    p::t -> (repPoint p) ++ " " ++ (repPoints t)

repPoint : G.Point -> String
repPoint p = (String.fromFloat (G.x p)) ++ "," ++ (String.fromFloat (G.y p))

floatify: (String -> a) -> Float -> a
floatify = (>>) String.fromFloat

{-| Translate a shape by the vector from the anchor to the current position. -}
translate
    : Move.Info Def
   -> Def
translate {anchor, current, payload} =
    { payload | geoDef = G.translateFromAnchor anchor current payload.geoDef }
