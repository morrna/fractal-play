module Space.ShapeSpec exposing ( suite )

import Test as T
import Fuzz as F
import Test.Html.Query as Q
import Test.Html.Selector as TS

import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Html as H
import Html.Attributes as HA
import List
import Tuple
import Color as C

import Geometry as G
import Space.Shape as Shape

suite : T.Test
suite = T.describe "module Space.Shape" [
        polygonFill
      , polygonOtherAttributes
    ]

polygonFill : T.Test
polygonFill = T.fuzz fuzzColor "fill is set correctly for polygon" <|
    \cFill ->
        let
            def = {fill=cFill, geoDef = plainTriangle}
            pgSVG = Shape.show [] def
        in
            Q.has [TS.attribute <| HA.attribute "fill" (C.toCssString cFill)]
                <| Q.fromHtml <| S.toUnstyled pgSVG

fuzzColor : F.Fuzzer C.Color
fuzzColor = let bitRange = F.intRange 0 255
    in F.map3 C.rgb255 bitRange bitRange bitRange

plainTriangle : G.Definition
plainTriangle = G.nGon 3 1 0 (G.point 0 0)

polygonOtherAttributes : T.Test
polygonOtherAttributes
    = T.fuzz
        (F.list <| F.pair (F.stringOfLengthBetween 1 32) F.string)
        "other shape attributes passed in to `Shape.Show` all show up on the element"
        <| \pairs ->
            let
                def = {fill=C.black, geoDef = plainTriangle}
                attrs = List.map pair2attr <| uniqueInFirst pairs
            in
                Q.has (List.map TS.attribute attrs)
                    <| Q.fromHtml <| S.toUnstyled
                    <| Shape.show (List.map SA.fromUnstyled attrs) def

pair2attr : (String, String) -> H.Attribute m
pair2attr (s1, s2) = HA.attribute s1 s2

uniqueInFirst : List (String, String) -> List (String, String)
uniqueInFirst = uniqueInFirstIter []

uniqueInFirstIter : List (String, String) -> List (String, String) -> List (String, String)
uniqueInFirstIter already notyet
    = case notyet of
        [] -> already
        h::t -> if List.any ((==) (Tuple.first h) << Tuple.first) already
            then uniqueInFirstIter already t
            else uniqueInFirstIter (h::already) t
