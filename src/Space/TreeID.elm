module Space.TreeID exposing (
        TreeID(..)
      , toString
      , equal
      , graft
      , switch
      , drop
    )

type TreeID
    = Trunk String
    | Branch TreeID String

{-| Produce a string representing the whole tree. Pieces are joined with '|' -}
toString : TreeID -> String
toString t = case t of
    Trunk s -> s
    Branch parent s -> (toString parent) ++ "|" ++ s

equal : TreeID -> TreeID -> Bool
equal = branchAccumulator False (==) (&&)

{-| Apply a given mapping to a tree, returning a default if comparing branch vs trunk -}
branchAccumulator
    : a -- default value
    -> (String -> String -> a) -- mapping on like branches
    -> (a -> a -> a) -- accumulator
    -> TreeID -> TreeID -> a -- produced mapping
branchAccumulator default map acc t1 t2
    = case t1 of
        Trunk s1 ->
            case t2 of
                Trunk s2 -> map s1 s2
                Branch _ _ -> default

        Branch p1 s1 ->
            case t2 of
                Trunk _ -> default
                Branch p2 s2
                    -> acc (branchAccumulator default map acc p1 p2) (map s1 s2)

graft
    : TreeID -- parent
    -> TreeID -- child
    -> TreeID -- new
graft parent child
    = case child of
        Trunk s -> Branch parent s
        Branch subparent s -> Branch (graft parent subparent) s

{-| Switch an old thing for a new thing only if IDs match. -}
switch
     : (c -> TreeID) {-^ getter for each thing's ID -}
    -> c             {-^ potential new thing -}
    -> c             {-^ incoming old thing -}
    -> c             {-^ The new thing, if the IDs match, otherwise the old thing unchanged. -}
switch getID newThing oldThing
    = if equal (getID oldThing) (getID newThing)
        then newThing
        else oldThing

{-| Drop the item from a list that matches the given TreeID. -}
drop
    : (a -> TreeID) {- getter for each item's ID -}
    -> TreeID       {- ID to match -}
    -> List a
    -> List a
drop getID matchID
    = List.filter (not << (equal matchID) << getID)
