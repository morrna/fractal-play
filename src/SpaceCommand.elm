module SpaceCommand exposing (
        Message
      , init
      , view
      , update
    )

import List
import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Css

import Space
import Start
import Command

init
    : Space.Model
init
    = Start.get Start.Sierpinski

type Message
    = SpaceMessage Space.Message
    | CommandMessage Command.Message

view
    : Space.Model
   -> HS.Html Message
view model
    = HS.div [HSA.css [Css.displayFlex]]
        [
            HS.div [HSA.css []]
                [HS.map SpaceMessage <| Space.view model]
          , HS.div [HSA.css [Css.flex (Css.int 1), Css.margin (Css.px 10)]]
                <| List.map (HS.map CommandMessage) <| Command.viewBar model
        ]

update
    : Message
   -> Space.Model
   -> Space.Model
update message
    = case message of
        SpaceMessage msg -> Space.update msg
        CommandMessage msg -> Command.update msg
