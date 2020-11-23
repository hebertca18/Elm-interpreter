module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main 


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
    ]
