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
import UndoList as U

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
    | RestoreHiddenContent


init : Sequence
init
    = [
        { message = HS.text "Click to start tutorial"
        , action = NoAction
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
                    (List.tail (Content.getIterFrameIDs spaceModel.baseContents.present))
                newContent = List.foldr Content.drop spaceModel.baseContents.present idsToDrop
            in { spaceModel |
                baseContents = U.new newContent spaceModel.baseContents
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
      , { message = HS.text ("Changing one function changes the shapes created by every frame, because at each layer"
          ++ " each function takes all the shapes from the previous layer as input."
          )
        , action = NoAction
        }
      , { message = HS.text ("Let's add another iteration."
          ++ " Change one of the functions again, and watch the shapes that change."
          )
        , action = ModifySpace (Space.setIterationDepth 3)
      }
      , { message = HS.text ("You might expect the image to just become randomly messy as you add more iterations."
          ++ " However, under the right conditions, the image will converge to a pattern determined by the functions,"
          ++ " not the initial shape."
          )
        , action = NoAction
      }
      , { message = HS.text ("Click the + on the sidebar under \"maximum iteration depth\" to add more iterations."
          ++ " (Stop if you notice the page slow down."
          ++ " This app doesn't currently prevent you from adding more shapes than your computer can handle.)"
          ++ " Once it starts looking like a fractal, play with the frames again!"
          ++ " The way your changes affect the fractal might surprise you."
          )
        , action = NoAction
      }
      , { message = HS.text ("That's all for this tutorial. Thanks for following along! If you want to start it again, just click here.")
        , action = NoAction
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
