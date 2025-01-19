{- Control the drawing space from outside -}
module Command exposing (
        Message(..)
      , viewBar
      , update
    )

import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Css
import List
import Maybe
import Svg.Styled as S

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

{-| Interactions with the command controls outside the drawing space -}
type Message
    = ChangeIterationDepth Int
    | ToggleShowIterFrames
    | ChangeNumIterFrames Int
    | Reset Start.Which
    | UpdateOnlyShowLastLayer Bool

{-| Display a vertical bar with controls for configuration.
    Currently this includes the maximum iteration depth and whether to show
    the iteration frames.
 -}
viewBar
    : Space.Model
   -> List (HS.Html Message)
viewBar {iterMode, baseContents}
    = choice "Start from"
            [
                ("Sierpinski triangle", Reset Start.Sierpinski)
              , ("Dragon", Reset Start.Dragon)
              , ("Sierpinski carpet", Reset Start.SierpinskiCarpet)
            ]
        ++ toggle "show iteration frames" ToggleShowIterFrames iterMode.showIterFrames
        ++ incrementer
            { incrementerDefaults | label = "maximum iteration depth" , min = Just 0}
            ChangeIterationDepth
            iterMode.depth
        ++ layerVisibilityControls
        ++ incrementer
            { incrementerDefaults | label = "# iteration frames" , min = Just 0}
            ChangeNumIterFrames
            (Content.numIterFrames baseContents)
        ++ iterFrameKey iterMode.showIterFrames

layerVisibilityControls : List (HS.Html Message)
layerVisibilityControls
    = textButtonGroup "layer visibility"
        [
            ("Show All Layers", UpdateOnlyShowLastLayer False)
          , ("Show Last Layer", UpdateOnlyShowLastLayer True)
        ]

{-| Update the model based on events from the command controls. -}
update
    : Message
   -> Space.Model
   -> Space.Model
update msg model =
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
                            baseContents = Content.drop iterFrameIDtoDrop model.baseContents
                        })
                        (getIterFrameIDtoDrop model)
                else if change == 1
                -- Iterframes are last in the list of baseContents, so just append a new one
                then Space.addIterFrame (getNewIterFrameID model) (.defaultIterFrame) model
                else model
        Reset whichStart
            -> Start.get whichStart
        UpdateOnlyShowLastLayer newOnlyShowLastLayer
            -> { model |
                iterMode = IterFrame.updateOnlyShowLastLayer
                    newOnlyShowLastLayer model.iterMode
            }

{-| Get the ID of the iter frame to drop when the number of iter frames is
    reduced.

    Currently this just gets the last IterFrame. In the future, it might be nice to tie this
    to the last IterFrame that was selected.
 -}
getIterFrameIDtoDrop : Space.Model -> Maybe ID.TreeID
getIterFrameIDtoDrop {baseContents}
    = List.head <| List.reverse <| Content.getIterFrameIDs baseContents

{-| Get an ID for a new IterFrame to add to the baseContents.

    The ID is generated by incrementing the number of IterFrames in the baseContents and adding
    a prefix of "f" similar to Space.init. At some point it might be worth writing common logic
    for generating IDs.
 -}
getNewIterFrameID : Space.Model -> ID.TreeID
getNewIterFrameID {baseContents}
    = ID.Trunk <| "f" ++ String.fromInt (1 + Content.numIterFrames baseContents)

iterFrameKey
    : Bool
   -> List (HS.Html msg)
iterFrameKey showIterFrames
    = if showIterFrames
        then
        [
            commandLabel "iteration frame controls"
          , S.svg
                [
                    HSA.css
                        [
                            Css.marginLeft (Css.px 16)
                          , Css.marginRight (Css.px 16)
                        ]
                ]
                [
                    IterFrame.showKey
                ]
        ]
        else []
