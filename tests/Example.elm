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
    test "interp"
        (\_ -> Expect.equal 4 (2 + 2)),
    test "serialize a number"
        (\_ -> Expect.equal "5" (serialize (NumV 5)))
    ]
