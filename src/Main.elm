module Main exposing ( main )

import Html.Styled as HS
import Browser as B

import Space
import SpaceCommand as SC

main : Program () Space.Model SC.Message
main = B.sandbox {
        init = SC.init
      , view = HS.toUnstyled << SC.view
      , update = SC.update
    }
