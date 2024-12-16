module Tutorial.Sequence exposing (
        Sequence
      , init
      , getCurrent
      , getMessage
      , advance
      , StepAction(..)
    )

import Html.Styled as HS
import Html.Styled.Attributes as HSA

import Space
import Space.Content as Content

{-| A sequence of steps in the tutorial. -}
type alias Sequence = List Step

{-| A single step in the tutorial. -}
type alias Step
    = {
        message : HS.Html ()
      , action : StepAction
    }

{-| Ways the tutorial can modify Space.Model. -}
type StepAction
    = NoAction
    | ModifySpace (Space.Model -> Space.Model)
    | RestoreFromCache


init : Sequence
init
    = [
        { message = HS.text "Click to start tutorial"
        , action = RestoreFromCache
        }
      , { message = HS.span []
            [ HS.text "These fractals come from "
            , HS.a
                [ HSA.href "https://en.wikipedia.org/wiki/Iterated_function_system" ]
                [ HS.text "iterated function systems" ]
            , HS.text (". Click the link to read about them on Wikipedia,"
              ++ " or click this block to continue."
              )
            ]
        , action = NoAction
        }
      , { message = HS.text ("This red frame represents an iteration."
          ++ " Let's just look at one for now."
          ++ " The red frame is the image of the thin black reference frame"
          ++ " under a function."
        )
        , action = ModifySpace (\spaceModel ->
            let
                oldIterMode = spaceModel.iterMode
                -- Drop all but the first IterFrame
                idsToDrop = Maybe.withDefault []
                    (List.tail (Content.getIterFrameIDs spaceModel.baseContents))
                newContent = List.foldr Content.drop spaceModel.baseContents idsToDrop
            in { spaceModel |
                baseContents = newContent
              , iterMode = { oldIterMode |
                  depth = 0
                , showIterFrames = True
              }
            }
          )
        }
      ]

getCurrent : Sequence -> Maybe Step
getCurrent sequence
    = List.head sequence

getMessage : Step -> HS.Html ()
getMessage item
    = item.message

advance : Sequence -> Sequence
advance sequence
    = case List.tail sequence of
        Nothing -> init
        Just [] -> init
        Just rest -> rest
