module Main exposing ( main )

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Css
import Browser as B

import Space
import SpaceCommand as SC
import Tutorial

main : Program () Space.Model SC.Message
main = B.sandbox {
        init = SC.init
      , view = HS.toUnstyled << viewWithHeader
      , update = SC.update
    }

viewWithHeader : Space.Model -> HS.Html SC.Message
viewWithHeader model =
    HS.div []
        [ HS.header 
            [ HSA.css 
                [ Css.displayFlex
                , Css.alignItems Css.center
                , Css.property "gap" "2rem"
                , Css.justifyContent Css.spaceBetween
                , Css.paddingLeft (Css.rem 2)
                , Css.paddingRight (Css.rem 2)
                ]
            ]
            [ HS.h1 
                [ HSA.css [Css.margin2 (Css.rem 1) (Css.rem 0) ] ]
                [ HS.text "Fractal Play" ]
            , Tutorial.view
            ]
        , SC.view model
        ]
