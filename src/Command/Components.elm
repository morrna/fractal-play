{- Visual components for controlling configuration -}
module Command.Components exposing (
        commandLabel
      , incrementer
      , IncrementerSettings
      , incrementerDefaults
      , toggle
      , choice
      , textButtonGroup
    )

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Html.Styled.Events as HSE
import List
import String
import Css

{-| Common heading for command controls -}
commandLabel : String -> HS.Html msg
commandLabel text =
    HS.h4 
        [ HSA.class "command-label" ]
        [ HS.text text ]

incrementer
    : IncrementerSettings        {- configuration -}
   -> (Int -> msg)  {- delegate to create messages from +1 / -1 -}
   -> Int           {- current value to display -}
   -> List (HS.Html msg)
incrementer settings sendIncrement current
    = [
        commandLabel settings.label
      , HS.span [] [
            HS.button
                (
                    if valueAtLimit settings.max current
                    then []
                    else [HSE.onClick <| sendIncrement 1]
                )
                [HS.text "+"]
          , numberLabel current
          , HS.button
                (
                    if valueAtLimit settings.min current
                    then []
                    else [HSE.onClick <| sendIncrement (-1)]
                )
                [HS.text "-"]
        ]
    ]

{-| Configuration for an incrementer

    label : String
        The label to display above the incrementer
 -}
type alias IncrementerSettings = {
        label : String
      , min : Maybe Int
      , max : Maybe Int
    }

incrementerDefaults : IncrementerSettings
incrementerDefaults
    = {
        label = ""
      , min = Nothing
      , max = Nothing
    }

valueAtLimit : Maybe Int -> Int -> Bool
valueAtLimit limit value
    = case limit of
        Just limitValue ->
            value == limitValue
        Nothing ->
            False


numberLabel
    : Int
   -> HS.Html msg
numberLabel
    = HS.span [HSA.css [Css.margin (Css.em 0.5)]]
        << List.singleton << HS.text << String.fromInt

toggle
    : String        {- label -}
   -> msg           {- message constructor -}
   -> Bool          {- current value to display -}
   -> List (HS.Html msg)
toggle label sendToggle current
    = let
        pressed
            = HSA.attribute "aria-pressed"
                <| if current then "true" else "false"
        style
            = HSA.css [
                Css.backgroundColor <|
                    if current
                    then (Css.rgb 76 175 80)    -- Lighter green (#4CAF50)
                    else (Css.rgb 244 67 54)    -- Lighter red (#F44336)
            ]
        labelText
            = if current then "on" else "off"
    in
    [
        commandLabel label
      , HS.button [HSE.onClick sendToggle, pressed, style] [HS.text labelText]
    ]

{-| A list of choices as a dropdown -}
choice
    : String             {- component label -}
   -> List (String, msg) {- choices, as a list of ( label text, message ) -}
   -> List (HS.Html msg)
choice label choices
    = [
        commandLabel label
      , HS.select []
            <| List.map
                (\(choiceText, msg) ->
                    HS.option [HSE.onClick msg] [HS.text choiceText]
                )
                choices
    ]

{-| A group of buttons with text labels and a header -}
textButtonGroup
    : String  -- ^ Header text
    -> List ( String, msg )  -- ^ List of (label, message) pairs
    -> List (HS.Html msg)
textButtonGroup header buttons
    = [
        HS.div [ HSA.css [Css.marginTop (Css.px 10)] ]
            (commandLabel header
                :: List.map
                    (\(label, msg) ->
                        HS.button
                            [
                                HSA.css [
                                    Css.marginRight (Css.px 5)
                                  , Css.marginLeft (Css.px 5)
                                ]
                              , HSE.onClick msg
                            ]
                            [ HS.text label ]
                    )
                    buttons
            )
    ]
