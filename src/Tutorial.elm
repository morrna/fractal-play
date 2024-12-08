module Tutorial exposing (
        view
    )

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Css


view : HS.Html msg
view
    = HS.div 
        [ 
            HSA.class "control-background"
            , HSA.css
                [ Css.padding (Css.rem 1)
                , Css.borderRadius (Css.rem 0.5)
                , Css.maxWidth (Css.px 400)
                ]
        ]
        [ HS.text (messageDisplay) ]

messageDisplay : String
messageDisplay
    = "Click to start tutorial" 
