module Util exposing (
        compareAllGetters
      , expectPointWithin
      , compareLists
      , compareGeoDefs
      , saneFloat
      , sanePositiveFloat
    )

import Expect as E
import Fuzz as F

import Geometry as G

expectPointWithin
    : E.FloatingPointTolerance
    -> G.Point -- expected
    -> G.Point -- actual
    -> E.Expectation
expectPointWithin tol
    = compareAllGetters (E.within tol) [G.x, G.y]

compareAllGetters
    : (a -> a -> E.Expectation)
    -> List (b -> a)
    -> b -- expected source type
    -> b -- actual source type
    -> E.Expectation
compareAllGetters comp l expected
    = E.all
        <| List.map
            (\getter actual
                -> comp (getter expected) (getter actual)
            )
            l

compareLists
    : (a -> a -> E.Expectation) -- comparison between points
    -> List a -- expected
    -> List a -- actual
    -> E.Expectation
compareLists comp lExp lAct
    = if List.length lExp == List.length lAct
    {- `all` has a funny form that's super frustrating for zipping list expectations together!
        It requires a list of mappings from a single value, rather than a list of expectation results.
        The workaround here is to make that 'single value' unit and then wrap the result we want in a
        function that ignores its argument.
        There should really be something like compareLists in the standard Expect library.
     -}
    then E.all (List.map2 ((<<) ((<<) always) comp) lExp lAct) ()
    else E.fail "compared lists have unequal length"

compareGeoDefs
    : (G.Point -> G.Point -> E.Expectation) -- comparison between points
    -> G.Definition -- expected
    -> G.Definition -- actual
    -> E.Expectation
compareGeoDefs comp dExp dAct
    = case dExp of
        G.Polygon lExp
            -> case dAct of
                G.Polygon lAct
                    -> compareLists comp lExp lAct
                G.Circle _ _ -> E.fail "different shapes"

        G.Circle cExp rExp
            -> case dAct of
                G.Polygon _ -> E.fail "different shapes"
                G.Circle cAct rAct
                    -> compareLists comp (centerRadiusToCompPts cExp rExp) (centerRadiusToCompPts cAct rAct)

{-| Because comparison is defined as between points,
    the radius needs to be converted into a point to be compared.
    A point on the circumference is fine. -}
centerRadiusToCompPts
    : G.Point -> Float -> List G.Point
centerRadiusToCompPts p f
    = p::[pointForFloatComp p f]

{-| Because comparison is defined as between points,
    plain floats need to be converted into points to be compared. -}
pointForFloatComp
    : G.Point -> Float -> G.Point
pointForFloatComp p f = G.point (G.x p + f) (G.y p)

{-| Because niceFloat isn't nice enough.
    Created after running into NaN results coming from numbers near the max of float
    being added or multiplied. -}
saneFloat : F.Fuzzer Float
saneFloat = F.floatRange (-1e99) 1e99

{-| Resonable strictly positive float. -}
sanePositiveFloat : F.Fuzzer Float
sanePositiveFloat = F.floatRange 1e-99 1e99
