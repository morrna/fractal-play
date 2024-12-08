module Tutorial.Sequence exposing (
        Sequence
      , init
      , getCurrent
      , getMessage
    )

import Html.Styled as HS

type alias Sequence = List Step

type alias Step = String

init : Sequence
init
    = [
        "Click to start tutorial"
    ]

getCurrent : Sequence -> Maybe Step
getCurrent sequence
    = List.head sequence

getMessage : Step -> HS.Html msg
getMessage item
    = HS.text item
