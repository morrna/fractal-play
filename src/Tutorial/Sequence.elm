module Tutorial.Sequence exposing (
        Sequence
      , init
      , getCurrent
      , getMessage
      , advance
    )

import Html.Styled as HS
import Html.Styled.Attributes as HSA

type alias Sequence = List Step

type alias Step
    = { message : HS.Html () }

init : Sequence
init
    = [
        { message = HS.text "Click to start tutorial" }
      , { message = HS.span []
            [ HS.text "These fractals come from "
            , HS.a
                [ HSA.href "https://en.wikipedia.org/wiki/Iterated_function_system" ]
                [ HS.text "iterated function systems" ]
            , HS.text (". Click the link to read about them on Wikipedia,"
              ++ " or click this block to continue."
              )
            ]
        }
      ]

getCurrent : Sequence -> Maybe Step
getCurrent sequence
    = List.head sequence

getMessage : Step -> HS.Html ()
getMessage item
    = item.message

advance : Sequence -> Sequence
advance sequence
    = case List.tail sequence of
        Nothing -> init
        Just [] -> init
        Just rest -> rest
