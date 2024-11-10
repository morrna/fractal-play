module Space.Pointer exposing (
        Update
      , State
      , updatePayload
      , dragEvents
      , doDrag
      , emptyState
      , select
      , getPayload {- not currently in use, but useful for debugging -}
    )

import List
import Svg.Styled as S
import Svg.Styled.Attributes as A
import Html.Events.Extra.Pointer as E

import Geometry as G
import Space.Move as Move

{-| Position of a pointer labeled for stage in action with attached info -}
type alias Update a = State a -> State a

type State anchorPayload
    = Off       (Move.Info anchorPayload)
    | Anchored  {anchor: G.Point, payload: anchorPayload}
    | Moving    (Move.Info anchorPayload)

{-| Make an empty state with a provided payload -}
emptyState
    : a
   -> State a
emptyState anchorPayload
    = Off {
          anchor = G.point 0 0
        , current = G.point 0 0
        , payload = anchorPayload
    }

{-| Get the payload from a pointer state. -}
getPayload
    : State a
   -> a
getPayload s
    = case s of
        Off mi -> mi.payload
        Anchored mi -> mi.payload
        Moving mi -> mi.payload


{-| Update only the payload of a pointer state. -}
updatePayload
    : (a -> b)
    -> State a
    -> State b
updatePayload f s
    = case s of
        Off mi -> Off {payload = f mi.payload, anchor = mi.anchor, current = mi.current}
        Anchored mi -> Anchored {payload = f mi.payload, anchor = mi.anchor}
        Moving mi -> Moving {payload = f mi.payload, anchor = mi.anchor, current = mi.current}

getOffsetPos : E.Event -> G.Point
getOffsetPos {pointer} = G.pointFromPair pointer.offsetPos


{- ## Shape events -}

{-| Set of events appropriate for things like moving things that are clicked and dragged. -}
dragEvents
     : (a -> a)          {- handler for ending the drag: onLast -}
    -> (Update a -> msg) {- message constructor -}
    -> List (S.Attribute msg)
dragEvents onLast makeMsg
    = List.map A.fromUnstyled <| [
        E.onDown  <| makeMsg << captureAnchor
      , E.onMove  <| makeMsg << captureCurrent
      , E.onUp    <| makeMsg << captureLast onLast
      , E.onLeave <| makeMsg << captureLast onLast
    ]

captureAnchor
    : E.Event
    -> Update a
captureAnchor
    = setAnchor << getOffsetPos

setAnchor
    : G.Point
    -> Update a
setAnchor p s
    = case s of
        Off mi -> Anchored {anchor = p, payload = mi.payload}
        Anchored mi -> Anchored {anchor = p, payload = mi.payload}
        Moving mi -> Moving { mi | anchor = p, payload = mi.payload}

setCurrent
    : G.Point
    -> Update a
setCurrent p s
    = case s of
        Off _ -> s
        Anchored {anchor, payload} -> Moving {anchor = anchor, payload = payload, current = p}
        Moving mv -> Moving { mv | current = p}

captureCurrent
    : E.Event
    -> Update a
captureCurrent
    = setCurrent << getOffsetPos

captureLast
    : (a -> a)
    -> E.Event
    -> Update a
captureLast onLast
    = ((<<) (turnOff << updatePayload onLast)) << setCurrent << getOffsetPos

turnOff
    : State a
    -> State a
turnOff s
    = case s of
        Off mi -> Off mi
        Anchored {anchor, payload} -> Off {anchor = anchor, current = anchor, payload = payload}
        Moving mi -> Off mi

doDrag
    : (Move.Info anchorPayload -> target -> target) {- transformer for target state -}
    -> State anchorPayload {- incoming (old) pointer state -}
    -> Update anchorPayload {- update function from event attribute -}
    -> (target -> target, State anchorPayload) {- action for target state, outgoing (new) pointer state -}
doDrag mover state ptrUpd
    = let
        newState = ptrUpd state
    in
        case newState of
            Anchored _ -> (identity, newState)
            Moving mi -> (mover mi, newState)
            Off mi
                -> case state of
                    Off _ -> (identity, newState)
                    Anchored _ -> (identity, newState)
                    Moving _ -> (mover mi, newState)

{-| Attribute generator for selecting a target on down only. -}
select
    : (target -> msg) {- message constructor -}
   -> target          {- target to select -}
   -> List (S.Attribute msg)
select makeMsg target
    = List.map A.fromUnstyled <| [
        E.onDown <| always <| makeMsg target
    ]
