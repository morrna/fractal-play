module Command.ComponentsSpec exposing ( suite )

import Test as T
import Test.Html.Query as Q
import Test.Html.Selector as S
import Test.Html.Event as E

import Html.Styled as HS
import List
import Maybe

import Command.Components exposing (
        toggle
      , incrementer
      , incrementerDefaults
    )

suite : T.Test
suite = T.describe "module Command.Components" [
        toggleSpec
      , incrementerSpec
    ]

toggleSpec : T.Test
toggleSpec = T.describe "toggle" [
        toggleHasLabel
      , toggleSends
    ]

type MockToggleMessage = MockToggleMessage

mockToggle : HS.Html MockToggleMessage
mockToggle = HS.div [] <| toggle "mock toggle" MockToggleMessage False

toggleHasLabel : T.Test
toggleHasLabel = T.test "button has label" <| \_ ->
    Q.has [ S.text "mock toggle" ] <|
        Q.fromHtml <| HS.toUnstyled mockToggle

toggleSends : T.Test
toggleSends = T.test "button sends message" <| \_ ->
    E.expect MockToggleMessage <|
        E.simulate E.click <|
            Q.find [ S.tag "button" ] <|
                Q.fromHtml <| HS.toUnstyled mockToggle


incrementerSpec : T.Test
incrementerSpec = T.describe "incrementer" [
        incrementerHasLabel
      , incrementerPlusSendsPos
      , incrementerMinusSendsNeg
    ]

type MockIncrementMessage = MockIncrementMessage Int

mockIncrementer : HS.Html (MockIncrementMessage)
mockIncrementer = HS.div [] <| incrementer
    { incrementerDefaults | label = "mock incrementer" }
    MockIncrementMessage
    0

incrementerHasLabel : T.Test
incrementerHasLabel = T.test "incrementer has label" <| \_ ->
    Q.has [ S.text "mock incrementer" ] <|
        Q.fromHtml <| HS.toUnstyled mockIncrementer

incrementerPlusSendsPos : T.Test
incrementerPlusSendsPos = T.test "incrementer plus sends positive" <| \_ ->
    E.expect (MockIncrementMessage 1) <|
        E.simulate E.click <|
            Q.find [ S.tag "button", S.containing [S.text "+"] ] <|
                Q.fromHtml <| HS.toUnstyled mockIncrementer

incrementerMinusSendsNeg : T.Test
incrementerMinusSendsNeg = T.test "incrementer minus sends negative" <| \_ ->
    E.expect (MockIncrementMessage -1) <|
        E.simulate E.click <|
            Q.find [ S.tag "button", S.containing [S.text "-"] ] <|
                Q.fromHtml <| HS.toUnstyled mockIncrementer
