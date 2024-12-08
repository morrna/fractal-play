module Main exposing ( main )

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Css
import Browser as B

import Space
import SpaceCommand as SC

main : Program () Space.Model SC.Message
main = B.sandbox {
        init = SC.init
      , view = HS.toUnstyled << viewWithHeader
      , update = SC.update
    }

viewWithHeader : Space.Model -> HS.Html SC.Message
viewWithHeader model =
    HS.div []
        [ HS.header []
            [ HS.h1 
                [ HSA.css [ Css.marginLeft (Css.rem 2) ] ]
                [ HS.text "Fractal Play" ]
            ]
        , SC.view model
        ]
