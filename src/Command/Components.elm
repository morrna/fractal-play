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
import Json.Decode as JD

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
                    HSA.class "control-background"
                    ::
                    if valueAtLimit settings.max current
                    then []
                    else [HSE.onClick <| sendIncrement 1]
                )
                [HS.text "+"]
          , numberLabel current
          , HS.button
                (
                    HSA.class "control-background"
                    ::
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
    = let
        (labels, messages) = List.unzip choices
        (values, onChange) = onChangeDiscrete messages
    in [
        commandLabel label
      , HS.select [HSA.class "control-background", onChange]
            <| List.map2
                (\val lbl ->
                    HS.option [HSA.value val] [HS.text lbl]
                )
                values
                labels
    ]

{-| An onChange event handler for a list of discrete messages.

    It returns `(values_list, onChange_attribute)`. Use the values to set up the
    options available for the element the attribute is applied to.
 -}
onChangeDiscrete
    : List msg
   -> (List String, HS.Attribute msg)
onChangeDiscrete messages
    = let
        valMsg = List.indexedMap (\ix msg -> (String.fromInt ix, msg)) messages
        values = List.map Tuple.first valMsg
        getMsg val = Maybe.map Tuple.second
            <| List.head
            <| List.filter (\(v, _) -> v == val) valMsg
        attribute = HSE.on "change"
            <| JD.andThen
                (\val -> case val of
                    Just m -> JD.succeed m
                    Nothing -> JD.fail "No message for value"
                )
            <| JD.map getMsg HSE.targetValue
    in
        (values, attribute)

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
                                HSA.class "control-background"
                              , HSA.css [
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
