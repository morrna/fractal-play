module Tutorial exposing (
        view
      , Model
      , init
    )

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Css
import Maybe

import Tutorial.Sequence as TS

type alias Model
    = { sequence : TS.Sequence }

init : Model
init = { sequence = TS.init }

view : Model -> HS.Html msg
view model
    = let
        current = TS.getCurrent model.sequence
        message = Maybe.withDefault
            (HS.text "")
            (Maybe.map TS.getMessage current)
        shouldDisplay = case current of
            Just _ -> True
            Nothing -> False
      in HS.div
        [
            HSA.class "control-background"
            , HSA.css
                [ Css.padding (Css.rem 1)
                , Css.borderRadius (Css.rem 0.5)
                , Css.maxWidth (Css.px 400)
                , (if shouldDisplay
                    then Css.display Css.block
                    else Css.display Css.none)
                ]
        ]
        [ message ]
