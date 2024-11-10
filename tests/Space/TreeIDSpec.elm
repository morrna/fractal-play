module Space.TreeIDSpec exposing ( suite )

import Test as T
import Expect as E
import Fuzz as F

import Space.TreeID as ID

suite : T.Test
suite = T.describe "module Space.TreeID" [
        toStringBasics
      , tEqual
      , tGraft
    ]

toStringBasics : T.Test
toStringBasics = T.describe "toString basic cases" [
        T.fuzz F.string "toString gives contents of trunk"
            <| \s -> E.equal s (ID.toString (ID.Trunk s))
      , T.fuzz2 F.string F.string "toString gives trunk|branch for branch with parent"
            <| \st sb -> E.equal (st ++ "|" ++ sb) (ID.toString (ID.Branch (ID.Trunk st) sb))
    ]

tEqual : T.Test
tEqual = T.describe "cases for `equal`" [
        T.fuzz F.string "trees with equal strings test equal"
            <| \s -> E.equal True <| ID.equal (ID.Trunk s) (ID.Trunk s)
      , T.fuzz2 F.string F.string "branches with equal strings test equal"
            <| \sp sb -> E.equal True
                <| ID.equal
                    (ID.Branch (ID.Trunk sp) sb)
                    (ID.Branch (ID.Trunk sp) sb)
      , T.fuzz2 F.string F.string "branches with unequal strings test unequal"
            <| \sp sb -> E.equal False
                <| ID.equal
                    (ID.Branch (ID.Trunk sp) (sb ++ "q"))
                    (ID.Branch (ID.Trunk sp) sb)
      , T.fuzz2 F.string F.string "branches with unequal parent strings test unequal"
            <| \sp sb -> E.equal False
                <| ID.equal
                    (ID.Branch (ID.Trunk (sp ++ "q")) sb)
                    (ID.Branch (ID.Trunk sp) sb)
      , T.fuzz2 F.string F.string "branch not equal to trunk"
            <| \sp sb -> E.equal False
                <| ID.equal
                    (ID.Branch (ID.Trunk sp) sb)
                    (ID.Trunk sp)
      , T.fuzz2 F.string F.string "trunk not equal to branch"
            <| \sp sb -> E.equal False
                <| ID.equal
                    (ID.Trunk sp)
                    (ID.Branch (ID.Trunk sp) sb)
      , T.fuzz2 fuzzTreeID fuzzTreeID "fully fuzzed `equal` test vs (==) after `toString`"
            <| \t1 t2 -> E.equal (ID.toString t1 == ID.toString t2) (ID.equal t1 t2)
    ]

tidEqual : ID.TreeID -> ID.TreeID -> E.Expectation
tidEqual = (<<) ((<<) (E.equal True)) ID.equal

tGraft : T.Test
tGraft = T.describe "cases for `graft`" [
        T.fuzz2 F.string F.string "two trunks make a branch"
            <| \s1 s2 -> tidEqual (ID.Branch (ID.Trunk s1) s2) (ID.graft (ID.Trunk s1) (ID.Trunk s2))
      , T.fuzz3 F.string F.string F.string "trunk + branch = longer branch"
            <| \s1 s2 s3 -> tidEqual
                (ID.Branch (ID.Branch (ID.Trunk s1) s2) s3)
                (ID.graft (ID.Trunk s1) (ID.Branch (ID.Trunk s2) s3))
      , T.fuzz3 F.string F.string F.string "branch + trunk = longer branch"
            <| \s1 s2 s3 -> tidEqual
                (ID.Branch (ID.Branch (ID.Trunk s1) s2) s3)
                (ID.graft (ID.Branch (ID.Trunk s1) s2) (ID.Trunk s3))
      , T.fuzz2 fuzzTreeID fuzzTreeID "fully fuzzed `graft` test vs `toString` and `++ |`"
            <| \t1 t2 -> E.equal
                    (ID.toString <| ID.graft t1 t2)
                    ((ID.toString t1) ++ "|" ++ (ID.toString t2))
    ]

fuzzTreeID : F.Fuzzer ID.TreeID
fuzzTreeID = F.andThen fuzzTreeIDN <| F.intRange 1 16

fuzzTreeIDN : Int -> F.Fuzzer ID.TreeID
fuzzTreeIDN n = if n <= 1
    then F.map ID.Trunk F.string
    else F.map2 ID.Branch (fuzzTreeIDN (n-1)) F.string
