module Tutorial.Sequence exposing (
        Sequence
      , init
      , getCurrent
      , getMessage
      , advance
    )

import Html.Styled as HS

type alias Sequence = List Step

type alias Step = String

init : Sequence
init
    = [
        "Click to start tutorial"
      , "These fractals come from Iteration Function Systems."
    ]

getCurrent : Sequence -> Maybe Step
getCurrent sequence
    = List.head sequence

getMessage : Step -> HS.Html msg
getMessage item
    = HS.text item

advance : Sequence -> Sequence
advance sequence
    = case List.tail sequence of
        Nothing -> init
        Just [] -> init
        Just rest -> rest
