{- Control the drawing space from outside -}
module Command exposing (
        init
      , Message
      , view
      , update
      , subscriptions
    )

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Css
import List
import Maybe
import Svg.Styled as S
import UndoList as U

import Space
import Space.Content as Content
import Space.IterFrame as IterFrame
import Space.TreeID as ID
import Start
import Command.Components exposing (
        incrementer
      , incrementerDefaults
      , toggle
      , choice
      , textButtonGroup
      , commandLabel
    )
import Command.Keyboard as Keyboard

{-| Initial combined state. -}
init
    : Space.Model
init
    = Start.get Start.Sierpinski

{-| Combined message for command and space. -}
type Message
    = SpaceMessage Space.Message
    | CommandMessage CommandMessage

{-| Interactions with the command controls outside the drawing space -}
type CommandMessage
    = ChangeIterationDepth Int
    | ToggleShowIterFrames
    | ChangeNumIterFrames Int
    | Reset Start.Which
    | UpdateOnlyShowLastLayer Bool
    | UndoList (U.Msg ())

{-| Combined view for space with the command bar. -}
view
    : Space.Model
   -> HS.Html Message
view model
    = HS.div [HSA.css [Css.displayFlex], HSA.class "space-command-container"]
        [
            HS.div [HSA.class "space-container"]
                [HS.map SpaceMessage <| Space.view model]
          , HS.div [HSA.class "command-bar"]
                <| List.map (HS.map CommandMessage) <| viewBar model
        ]

{-| Display a vertical bar with controls for configuration.
    Currently this includes the maximum iteration depth and whether to show
    the iteration frames.
 -}
viewBar
    : Space.Model
   -> List (HS.Html CommandMessage)
viewBar {iterMode, baseContents}
    = choice "Start From"
        [
            ("Sierpinski triangle", Reset Start.Sierpinski)
          , ("Dragon", Reset Start.Dragon)
          , ("Sierpinski carpet", Reset Start.SierpinskiCarpet)
        ]
        ++ toggle "Show Iteration Frames" ToggleShowIterFrames iterMode.showIterFrames
        ++ incrementer
            { incrementerDefaults | label = "Maximum Iteration Depth" , min = Just 0}
            ChangeIterationDepth
            iterMode.depth
        ++ layerVisibilityControls
        ++ incrementer
            { incrementerDefaults | label = "# Iteration Frames" , min = Just 0}
            ChangeNumIterFrames
            (Content.numIterFrames baseContents.present)
        ++ iterFrameKey
        ++ textButtonGroup "Canvas State"
            [
                ("Undo", UndoList U.Undo)
              , ("Redo", UndoList U.Redo)
            ]

layerVisibilityControls : List (HS.Html CommandMessage)
layerVisibilityControls
    = textButtonGroup "Layer Visibility"
        [
            ("Show All Layers", UpdateOnlyShowLastLayer False)
          , ("Show Last Layer", UpdateOnlyShowLastLayer True)
        ]

{-| Combined update function for space and command.
    Passes each sub message to the appropriate update function.
 -}
update
    : Message
   -> Space.Model
   -> Space.Model
update message
    = case message of
        SpaceMessage msg -> Space.update msg
        CommandMessage msg -> updateCommand msg

{-| Update the model based on events from the command controls. -}
updateCommand
    : CommandMessage
   -> Space.Model
   -> Space.Model
updateCommand msg model =
    case msg of
        ChangeIterationDepth change
            -> { model |
                iterMode = let oldIterMode = model.iterMode
                    in { oldIterMode | depth = oldIterMode.depth + change }
            }
        ToggleShowIterFrames
            -> { model |
                iterMode = let oldIterMode = model.iterMode
                    in { oldIterMode | showIterFrames = not oldIterMode.showIterFrames }
            }
        ChangeNumIterFrames change
            -> if change == -1
                then Maybe.withDefault model <|
                    Maybe.map
                        (\iterFrameIDtoDrop -> { model |
                            baseContents = U.new
                                (Content.drop iterFrameIDtoDrop model.baseContents.present)
                                model.baseContents
                        })
                        (getIterFrameIDtoDrop model)
                else if change == 1
                then Space.addIterFrameNew (getNewIterFrameID model) (.defaultIterFrame) model
                else model
        Reset whichStart
            -> Start.get whichStart
        UpdateOnlyShowLastLayer newOnlyShowLastLayer
            -> { model |
                iterMode = IterFrame.updateOnlyShowLastLayer
                    newOnlyShowLastLayer model.iterMode
            }
        UndoList ulMsg
            -> { model |
                baseContents = U.update (always identity) ulMsg model.baseContents
            }

{-| Get the ID of the iter frame to drop when the number of iter frames is
    reduced.

    Currently this just gets the last IterFrame. In the future, it might be nice to tie this
    to the last IterFrame that was selected.
 -}
getIterFrameIDtoDrop : Space.Model -> Maybe ID.TreeID
getIterFrameIDtoDrop {baseContents}
    = List.head <| List.reverse <| Content.getIterFrameIDs baseContents.present

{-| Get an ID for a new IterFrame to add to the baseContents.

    The ID is generated by incrementing the number of IterFrames in the baseContents and adding
    a prefix of "f" similar to Space.init. At some point it might be worth writing common logic
    for generating IDs.
 -}
getNewIterFrameID : Space.Model -> ID.TreeID
getNewIterFrameID {baseContents}
    = ID.Trunk <| "f" ++ String.fromInt (1 + Content.numIterFrames baseContents.present)

iterFrameKey : List (HS.Html msg)
iterFrameKey
    = [
        commandLabel "Iteration Frame Controls"
      , S.svg
            [
                HSA.css
                    [
                        Css.marginLeft (Css.px 16)
                      , Css.marginRight (Css.px 16)
                      -- Correct for extra whitespace in the SVG
                      , Css.marginBottom (Css.px -20)
                    ],
                HSA.height 110
            ]
            [
                IterFrame.showKey
            ]
    ]

subscriptions : Sub Message
subscriptions = Sub.map (CommandMessage << UndoList) Keyboard.undoRedoSubscriptions
