module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    test "Interp NumC"
        Expect.equal NumV 1 (interp NumC 1 [])
