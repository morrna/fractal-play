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
    | RestoreHiddenContent


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
            , HS.text (". Click the link to read about them on Wikipedia."
              ++ " Every time you're ready to continue with the next tutorial step, click this block."
              )
            ]
        , action = NoAction
        }
      , { message = HS.text ("This red frame represents an iteration."
          ++ " Let's just look at one for now."
          ++ " If you feed the thin black reference frame into the iteration function,"
          ++ " you get the red frame as the output."
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
      , { message = HS.text ("Now let's apply the function to the base shape once."
          ++ " Notice how the new shape fits inside the red frame in the same way the base shape fits"
          ++ " inside the thin black reference frame."
          )
        , action = ModifySpace (Space.setIterationDepth 1)
        }
      , { message = HS.text ("The knobs on the red frame let you modify the function."
          ++ " The grey knob lets you move the center offset."
          ++ " Give it a try!"
          )
        , action = NoAction
        }
      , { message = HS.text ("The cyan knob lets you change the rotation angle."
          ++ " Give it a spin!"
          )
        , action = NoAction
        }
      , { message = HS.text ("The yellow knob lets you change the scale."
          ++ " Try making it bigger and smaller!"
          )
        , action = NoAction
        }
      , { message = HS.text ("The magenta knob lets you change how much the function stretches in one direction"
          ++ " while keeping the other direction the same size. It also lets you change the skew,"
          ++ " making right angles come out of the function at a slant."
          )
        , action = NoAction
        }
      , { message = HS.text ("You probably noticed how the first new shape changes as you change the function."
          ++ " Now let's apply the function again. Play with the knobs and watch the second new shape change."
          ++ " Every change that happens to the first new shape happens to the second new shape doubled."
          )
        , action = ModifySpace (Space.setIterationDepth 2)
        }
      , { message = HS.text ("Now it's time to use multiple iteration functions at once."
          ++ " Each function creates its own new shape at each layer."
          ++ " Then for the next layer, each function takes all the shapes from the previous layer as input."
          ++ " Spend some time playing with the functions, and watch how the shapes in the second layer change."
          )
        , action = RestoreHiddenContent
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
