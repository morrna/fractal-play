module Main exposing ( main )

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Css
import Browser as B

import SpaceCommand as SC
import Tutorial

main : Program () Tutorial.WrapModel Tutorial.WrapMessage
main = B.sandbox {
        init = Tutorial.wrapInit
      , view = HS.toUnstyled << viewWithHeaderFooter
      , update = update
    }


{-| Main view, including header and tutorial.
    This is defined here because things like the header should be owned by Main.
 -}
viewWithHeaderFooter : Tutorial.WrapModel -> HS.Html Tutorial.WrapMessage
viewWithHeaderFooter model =
    HS.div [ HSA.id "inner-root" ]
        [ HS.header
            [ HSA.class "header" ]
            [ HS.h1
                [ HSA.css [Css.margin2 (Css.rem 1) (Css.rem 0) ] ]
                [ HS.text "Fractal Play" ]
            , HS.map Tutorial.TutorialMessage <| Tutorial.view model.tutorial
            ]
        , HS.map Tutorial.SpaceMessage <| SC.view model.space
        , HS.footer []
            [ HS.p [] [ HS.a [ HSA.href "LICENSE.txt" ] [ HS.text "© 2024 Nathan Morrison" ] ] ]
        ]


{-| Update that redirects messages to the right component. -}
update : Tutorial.WrapMessage -> Tutorial.WrapModel -> Tutorial.WrapModel
update message model
  = case message of
        Tutorial.SpaceMessage msg -> { model | space = SC.update msg model.space }
        Tutorial.TutorialMessage msg -> Tutorial.update msg model
