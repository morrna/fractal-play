module Space.Move exposing (
        Info
      , mapPayload
      , pullMaybe
    )

{-| Common definitions both for things that move, and the things that need to control movement.
-}

import Geometry as G

{-| Information about a move. -}
type alias Info payload
    = {
        anchor: G.Point
      , current: G.Point
      , payload: payload
    }

{-| Apply a function to the payload, leaving other info in place. -}
mapPayload : (p -> q) -> Info p -> Info q
mapPayload f info =
    {
        anchor = info.anchor
      , current = info.current
      , payload = f info.payload
    }

{-| If the payload is optional, make the move info optional. -}
pullMaybe : Info (Maybe p) -> Maybe (Info p)
pullMaybe info =
    case info.payload of
        Just p
            -> Just { anchor = info.anchor, current = info.current, payload = p }
        Nothing
            -> Nothing
