module TestUtil exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Graphql.Util exposing (..)
import Parser exposing (Parser, run, (|.), (|=), succeed, symbol, end)

identifiers : Test
identifiers =
    describe "primitives"
        [ parseOk "lowercase" ident "hello" "hello" 
        , parseOk "CamelCase" ident "CamelCase" "CamelCase"
        , parseOk "snake_case" ident "snake_case" "snake_case"
        , parseFail "comma fail" ident ",foo"
        , parseFail "parens fail" ident "[bazz]"
        ]

whitespace : Test
whitespace =
    describe "whitespace tests"
        [ wspAndMark "is optional" "hi"
        , wspAndMark "single space" " hi"
        , wspAndMark "double spaces" "  hi"
        , wspAndMark "end of line" "\nhi"
        , wspAndMark "multi eol mixed with spaces" " \n\n hi"
        , wspAndMark "empty comment" "#\nhi"
        , wspAndMark "non-empty comment" "#hello here\nhi"
        , wspAndMark "multi-line comment" "#hello\n#here\nhi"
        , wspAndMark "mix spaces eol and comment" " #hello\n\n hi"
        ]


wspAndMark : String -> String -> Test
wspAndMark label src =
    test label <|
        \_ ->
            run wsParser src
                |> Expect.equal (Ok ())

wsParser : Parser ()
wsParser =
    succeed identity
        |. sp
        |= symbol "hi"
        |. end

parseOk : String -> Parser a -> String -> a -> Test
parseOk label parser src expected =
    test label <|
        \_ ->
            run parser src
                |> Expect.equal (Ok expected)

parseFail : String -> Parser a -> String -> Test
parseFail label parser src =
    test label <|
        \_ ->
            run parser src |> Expect.err