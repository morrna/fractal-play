{- Visual components for controlling configuration -}
module Command.Components exposing (
        incrementer
      , IncrementerSettings
      , incrementerDefaults
      , toggle
      , choice
    )

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Html.Styled.Events as HSE
import List
import String
import Css


incrementer
    : IncrementerSettings        {- configuration -}
   -> (Int -> msg)  {- delegate to create messages from +1 / -1 -}
   -> Int           {- current value to display -}
   -> List (HS.Html msg)
incrementer settings sendIncrement current
    = [
        HS.h4 [] [HS.text settings.label]
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
                    then (Css.rgb 0 128 0)
                    else (Css.rgb 128 0 0)
            ]
    in
    [
        HS.h4 [] [HS.text label]
      , HS.button [HSE.onClick sendToggle, pressed, style] [HS.text "on"]
    ]

{-| A list of choices as a dropdown -}
choice
    : String             {- component label -}
   -> List (String, msg) {- choices, as a list of ( label text, message ) -}
   -> List (HS.Html msg)
choice label choices
    = [
        HS.h4 [] [HS.text label]
      , HS.select []
            <| List.map
                (\(choiceText, msg) ->
                    HS.option [HSE.onClick msg] [HS.text choiceText]
                )
                choices
    ]