module Graphql.Util exposing (alpha, wsp, identChar, sequence, ident, sp, space, liftResult, comment, isBang)

import Parser
    exposing
        ( Parser
        , Count(..)
        , (|.)
        , (|=)
        , symbol
        , repeat
        , ignore
        , succeed
        , oneOrMore
        , zeroOrMore
        , source
        , delayedCommit
        , oneOf
        )
import Parser.LanguageKit exposing 
    ( whitespace
    , LineComment(LineComment)
    , MultiComment(NoMultiComment)
    )
import Char


-- Character classes


alpha : Char -> Bool
alpha c =
    Char.isLower c || Char.isUpper c


wsp : Char -> Bool
wsp c =
    c == ' ' || c == '\n' || c == '\x0D' || c == '\t'


identChar : Char -> Bool
identChar c =
    Char.isDigit c || alpha c || c == '_'



-- Parsers

isBang : Parser Bool
isBang = oneOf [Parser.map (\()->True) (symbol "!"), succeed False]

liftResult : Parser (Result x a) -> Parser a
liftResult =
    Parser.andThen <|
        \v ->
            case v of
                Ok content -> succeed content
                Err err -> Parser.fail <| toString err


sequence : Parser a -> Parser (List a)
sequence parser =
    succeed (::)
        |= parser
        |= repeat zeroOrMore (delayedCommit space parser)


ident : Parser String
ident =
    source <|
        ignore (Exactly 1) alpha
            |. ignore zeroOrMore identChar

comment : Parser ()
comment =
    symbol "#"
        |. ignore zeroOrMore (\c -> c /= '\n')
        |. symbol "\n"


sp : Parser ()
sp =
    whitespace
        { allowTabs = False
        , lineComment = LineComment "#"
        , multiComment = NoMultiComment

        }
    -- ignore zeroOrMore wsp
    --     |. repeat zeroOrMore comment
    --     |. ignore zeroOrMore wsp


space : Parser ()
space =
    ignore (Exactly 1) wsp
        |. sp
