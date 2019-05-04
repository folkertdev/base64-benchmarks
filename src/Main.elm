module Main exposing (main, suite)

import Array
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Encode as Encode
import New.Base64 as New
import New.Decode
import New.Encode
import Old.Base64 as Old


main : BenchmarkProgram
main =
    program suite


exampleBase64String =
    "15DXlNec158g16LXldec150="


data =
    -- generate 1 KB of test data
    -- repeat fewer times for faster benchmarks
    String.repeat 64 ("15DXlNec" ++ "158g16LX")


old =
    Old.toBytes data


new =
    New.toBytes data


suite : Benchmark
suite =
    describe "Base64" [ oldVsNew, oldVsNewEncode ]


oldVsNew =
    Benchmark.compare "decode performance"
        "old"
        (\_ -> Maybe.map Old.fromBytes old)
        "new"
        (\_ -> Maybe.map New.fromBytes new)


oldVsNewEncode =
    Benchmark.compare "encode performance"
        "old"
        (\_ -> Old.toBytes data)
        "new"
        (\_ -> New.toBytes data)


justNewEncode =
    benchmark "new" (\_ -> New.toBytes data)


justNew =
    benchmark "new" (\_ -> Maybe.map New.fromBytes new)


encodingInts =
    let
        a =
            34

        b =
            42

        c =
            140

        combined =
            Bitwise.or (Bitwise.shiftLeftBy 8 a) b
    in
    Benchmark.compare "byte encoding performance"
        "with uint16"
        (\_ ->
            Encode.encode <|
                Encode.sequence
                    [ Encode.unsignedInt16 BE combined
                    , Encode.unsignedInt8 c
                    ]
        )
        "only uint8"
        (\_ ->
            Encode.encode <|
                Encode.sequence
                    [ Encode.unsignedInt8 a
                    , Encode.unsignedInt8 b
                    , Encode.unsignedInt8 c
                    ]
        )
