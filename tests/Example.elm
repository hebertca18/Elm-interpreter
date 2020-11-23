module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main 
import Main exposing (Value(..))
import Main exposing (handlePrimop)


suite : Test
suite =
    descrie "inter"
    [test "two plus two equals four"
        (\_ -> Expect.equal 4 (2 + 2)),
    test "interp num"
        (\_ -> Expect.equal (NumV 5) (interp (NumC 5 []))),
    test "interp string"
        (\_ -> Expect.equal (StringV "\"test\"") (interp (StringC "test" []))),
    test "interp id"
        (\_ -> Expect.equal (IdC "+") (interp (IdC "+" []))),
    test "interp function"
        (\_ -> Expect.equal (CloV [] (NumC 5) []) (interp (LamC [] (NumC 5) []))),
    test "interp application"
        (\_ -> Expect.equal (NumC 7) (interp (AppC "+" (NumC 3) (NumC 4))),
    test "serialize a number"
        (\_ -> Expect.equal "5" (serialize (NumV 5)))
    test "serialize a string"
        (\_ -> Expect.equal "LOL" (serialize (StringV "LOL")))
    test "serialize a cloV"
        (\_ -> Expect.equal "#<procedure>" (serialize (CloV ["lol"] (NumC 5) [(Binding "+" (PrimopV "+"))])))
    test "serialize a PrimopV"
        (\_ -> Expect.equal "#<primop>" (serialize (PrimopV "+")))
    test "serialize a BoolV"
        (\_ -> Expect.equal "true" (serialize (BoolV True)))
    test "serialize a Invalid Input"
        (\_ -> Expect.equal "message" (serialize (InvalidInput "message")))
    test "serialize a NonBooleanTestInput"
        (\_ -> Expect.equal "testing" (serialize (NonBooleanTestInput "testing")))
    test "serialize a NonExistentBinding"
        (\_ -> Expect.equal "test" (serialize (NonExistentBinding "test")))
    test "serialize unrecognized"
        (\_ -> Expect.equal "ERROR: Unrecognized value." (serialize 5))    
    test "interp a number"
        (\_ -> Expect.equal (NumV 5) (interp (NumC 5) [(Binding "+" (PrimopV "+")), 
        (Binding "-" (PrimopV "-")),
        (Binding "/" (PrimopV "/")),
        (Binding "*" (PrimopV "*")),
        (Binding "<=" (PrimopV "<=")),
        (Binding "equal?" (PrimopV "equal?")),
        (Binding "true" (BoolV True)),
        (Binding "false" (BoolV False))]))
    test "intepr a function returning boolean"
        (\_ -> Expect.equal (BoolV True) (interp (NumV 5) [(Binding "+" (PrimopV "+")), 
        (Binding "-" (PrimopV "-")),
        (Binding "/" (PrimopV "/")),
        (Binding "*" (PrimopV "*")),
        (Binding "<=" (PrimopV "<=")),
        (Binding "equal?" (PrimopV "equal?")),
        (Binding "true" (BoolV True)),
        (Binding "false" (BoolV False))]))
    test "invalid interp, no closure or primop"
        (\_ -> Expect.equal (InvalidInput "First argument of AppC must be a closure or Primop") 
        (interp (AppC (BoolV True) [(NumC 5), (NumC 5)]) [(Binding "+" (PrimopV "+")), 
        (Binding "-" (PrimopV "-")),
        (Binding "/" (PrimopV "/")),
        (Binding "*" (PrimopV "*")),
        (Binding "<=" (PrimopV "<=")),
        (Binding "equal?" (PrimopV "equal?")),
        (Binding "true" (BoolV True)),
        (Binding "false" (BoolV False))]))
     test "interp ifC"
        (\_ -> Expect.equal (NumV 4) 
        (interp (IfC (AppC (IdC "equal?") [(IdC "true"), (IdC "true")]) (NumC 4) (NumC 5)) 
        [(Binding "+" (PrimopV "+")), 
        (Binding "-" (PrimopV "-")),
        (Binding "/" (PrimopV "/")),
        (Binding "*" (PrimopV "*")),
        (Binding "<=" (PrimopV "<=")),
        (Binding "equal?" (PrimopV "equal?")),
        (Binding "true" (BoolV True)),
        (Binding "false" (BoolV False))]))
    test "handlePrimop +"
        (\_ -> Expect.equal (NumV 10)
        (interp (handlePrimop "+" [(NumC 5), (NumC 5)]) 
        [(Binding "+" (PrimopV "+")), 
        (Binding "-" (PrimopV "-")),
        (Binding "/" (PrimopV "/")),
        (Binding "*" (PrimopV "*")),
        (Binding "<=" (PrimopV "<=")),
        (Binding "equal?" (PrimopV "equal?")),
        (Binding "true" (BoolV True)),
        (Binding "false" (BoolV False))]))
    test "handlePrimop + error"
        (\_ -> Expect.equal (InvalidInput "true must evaluate to a number for +")
        (interp (handlePrimop "+" [(BoolC True), (NumC 5)]) 
        [(Binding "+" (PrimopV "+")), 
        (Binding "-" (PrimopV "-")),
        (Binding "/" (PrimopV "/")),
        (Binding "*" (PrimopV "*")),
        (Binding "<=" (PrimopV "<=")),
        (Binding "equal?" (PrimopV "equal?")),
        (Binding "true" (BoolV True)),
        (Binding "false" (BoolV False))]))
    test "handlePrimop + error"
        (\_ -> Expect.equal (InvalidInput "true must evaluate to a number for +")
        (interp (handlePrimop "+" [(BoolC True), (NumC 5)]) 
        [(Binding "+" (PrimopV "+")), 
        (Binding "-" (PrimopV "-")),
        (Binding "/" (PrimopV "/")),
        (Binding "*" (PrimopV "*")),
        (Binding "<=" (PrimopV "<=")),
        (Binding "equal?" (PrimopV "equal?")),
        (Binding "true" (BoolV True)),
        (Binding "false" (BoolV False))]))
    test "handlePrimop + error"
        (\_ -> Expect.equal (InvalidInput "true must evaluate to a number for +")
        (interp (handlePrimop "+" [(NumC 5), (BoolC True)]) 
        [(Binding "+" (PrimopV "+")), 
        (Binding "-" (PrimopV "-")),
        (Binding "/" (PrimopV "/")),
        (Binding "*" (PrimopV "*")),
        (Binding "<=" (PrimopV "<=")),
        (Binding "equal?" (PrimopV "equal?")),
        (Binding "true" (BoolV True)),
        (Binding "false" (BoolV False))]))
    test "handlePrimop + error args"
        (\_ -> Expect.equal (InvalidInput "Invalid number of arguments for a primop")
        (interp (handlePrimop "+" [(NumC 5), (BoolC True), (NumC 5)]) 
        [(Binding "+" (PrimopV "+")), 
        (Binding "-" (PrimopV "-")),
        (Binding "/" (PrimopV "/")),
        (Binding "*" (PrimopV "*")),
        (Binding "<=" (PrimopV "<=")),
        (Binding "equal?" (PrimopV "equal?")),
        (Binding "true" (BoolV True)),
        (Binding "false" (BoolV False))]))
    test "lookup -"
        (\_ -> Expect.equal (PrimopV "-")
        (lookup [(Binding "+" (PrimopV "+")), 
        (Binding "-" (PrimopV "-")),
        (Binding "/" (PrimopV "/")),
        (Binding "*" (PrimopV "*")),
        (Binding "<=" (PrimopV "<=")),
        (Binding "equal?" (PrimopV "equal?")),
        (Binding "true" (BoolV True)),
        (Binding "false" (BoolV False))]
        "-"))
    test "lookup error"
        (\_ -> Expect.equal (Main.NonExistentBinding "The binding for: fdsa does not exist.")
        (lookup [(Binding "+" (PrimopV "+")), 
        (Binding "-" (PrimopV "-")),
        (Binding "/" (PrimopV "/")),
        (Binding "*" (PrimopV "*")),
        (Binding "<=" (PrimopV "<=")),
        (Binding "equal?" (PrimopV "equal?")),
        (Binding "true" (BoolV True)),
        (Binding "false" (BoolV False))]
        "fdsa"))
    ]

