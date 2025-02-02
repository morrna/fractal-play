{- Control the drawing space from outside -}
module Command exposing (
        Model
      , init
      , liftSpace
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
import Browser.Navigation as Nav

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
import Command.Encoding as Encoding

{-| Combined state for the command bar and the svg space. -}
type alias Model = {
        space : Space.Model
    , command : CommandModel
    }

{-| Initial combined state. -}
init
    : Nav.Key
   -> Model
init key
    = {
        space = Start.get Start.Sierpinski,
        command = { showBookmark = False, navKey = key }
    }

{-| Lift a function over the space model to the combined state. -}
liftSpace : (Space.Model -> Space.Model) -> Model -> Model
liftSpace f model = { model | space = f model.space }

{-| Lift a function over the command model to the combined state. -}
liftCommand : (CommandModel -> CommandModel) -> Model -> Model
liftCommand f model = { model | command = f model.command }

{-| State specific to the command bar. -}
type alias CommandModel = {
        showBookmark : Bool
      , navKey : Nav.Key
    }

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
    | ToggleShowBookmark

{-| Combined view for space with the command bar. -}
view
    : Model
   -> HS.Html Message
view model
    = HS.div [HSA.css [Css.displayFlex], HSA.class "space-command-container"]
        [
            HS.div [HSA.class "space-container"]
                [HS.map SpaceMessage <| Space.view model.space]
          , HS.div [HSA.class "command-bar"]
                <| List.map (HS.map CommandMessage) <| viewBar model.space model.command
        ]

{-| Display a vertical bar with controls for configuration.
    Currently this includes the maximum iteration depth and whether to show
    the iteration frames.
 -}
viewBar
    : Space.Model
   -> CommandModel
   -> List (HS.Html CommandMessage)
viewBar spaceModel {showBookmark}
    = let
        { iterMode, baseContents } = spaceModel
    in choice "Start From"
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
              , (if showBookmark then "Hide Bookmark" else "Show Bookmark", ToggleShowBookmark)
            ]
        ++ viewBookmark showBookmark spaceModel

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
   -> Model
   -> Model
update message
    = case message of
        SpaceMessage msg -> liftSpace <| Space.update msg
        CommandMessage msg -> updateCommand msg

{-| Update the model based on events from the command controls. -}
updateCommand
    : CommandMessage
   -> Model
   -> Model
updateCommand msg =
    case msg of
        ChangeIterationDepth change
            -> liftSpace <| Space.liftIterMode (\iterMode
                -> { iterMode | depth = iterMode.depth + change }
            )
        ToggleShowIterFrames
            -> liftSpace <| Space.liftIterMode (\iterMode
                -> { iterMode | showIterFrames = not iterMode.showIterFrames }
            )
        ChangeNumIterFrames change
            -> liftSpace <| changeNumIterFrames change
        Reset whichStart
            -> liftSpace <| always <| Start.get whichStart
        UpdateOnlyShowLastLayer newOnlyShowLastLayer
            -> liftSpace <| Space.liftIterMode
                <| IterFrame.updateOnlyShowLastLayer newOnlyShowLastLayer
        UndoList ulMsg
            -> liftSpace <| Space.liftUndoList
                <| U.update (always identity) ulMsg
        ToggleShowBookmark
            -> liftCommand <| \command -> { command | showBookmark = not command.showBookmark }

{-| Apply a change in the number of iter frames to Space.Model. -}
changeNumIterFrames : Int -> Space.Model -> Space.Model
changeNumIterFrames change model
    = if change == -1
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

{-| Show the bookmark in an input field for easy copying.
    Only show if showBookmark is True.
 -}
viewBookmark : Bool -> Space.Model -> List (HS.Html CommandMessage)
viewBookmark showBookmark spaceModel
    = if showBookmark
        then [
            HS.input
                [
                    HSA.type_ "text"
                  , HSA.readonly True
                  , HSA.value (Encoding.getStateByteString spaceModel)
                ]
                []
        ]
        else []
