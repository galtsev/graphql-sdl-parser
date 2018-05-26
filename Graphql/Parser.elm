module Graphql.Parser exposing (..)

import Parser exposing (Parser, Count(..), (|.), (|=), repeat, ignore, symbol,
     oneOf, succeed, oneOrMore, zeroOrMore, source, fail, andThen, delayedCommit)
import Char

type Type = 
    IntType
    | StringType
    | FloatType
    | Required Type
    | ListOf Type
    | Ref Ref

type alias Ref = String

type alias Field = {name: String, typ: Type}

type alias RecordType = {name: String, fields: List Field}
type alias InterfaceType = {name: String, fields: List Field}

type Definition = RecordDef RecordType | InterfaceDef InterfaceType

parser : Parser (List Definition)
parser =
    succeed identity
        |. sp
        |= sequence definitionParser

definitionParser : Parser Definition
definitionParser =
    oneOf
        [ recordTypeParser
        , interfaceTypeParser
        ]

makeRecordDef : String -> List Field -> Definition
makeRecordDef name fields = RecordDef {name = name, fields = fields}

recordTypeParser : Parser Definition
recordTypeParser =
    succeed makeRecordDef
        |. symbol "type"
        |. space
        |= ident
        |. sp
        |. symbol "{"
        |. sp
        |= sequence fieldParser
        |. sp
        |. symbol "}"

interfaceTypeParser : Parser Definition
interfaceTypeParser =
    succeed (InterfaceDef {name = "test", fields=[]})

sequence : Parser a -> Parser (List a)
sequence parser =
    succeed (::)
        |= parser
        |= repeat zeroOrMore (delayedCommit space parser)

alpha : Char -> Bool
alpha c = Char.isLower c || Char.isUpper c

identChar : Char -> Bool
identChar c = Char.isDigit c || alpha c || c=='_'

makeField : String -> Type -> Field
makeField name typ = {name = name, typ = typ}

fieldParser : Parser Field
fieldParser =
    succeed makeField
        |= ident
        |. sp
        |. symbol ":"
        |. sp
        |= typeParser

parseType : String -> Result String Type
parseType name =
    case name of
        "int" -> Ok IntType
        "string" -> Ok StringType
        "float" -> Ok FloatType
        _ -> Err <| "bad type name:" ++ name

typeParser : Parser Type
typeParser =
    ident
        |> Parser.map parseType
        |> andThen (\name ->
            case name of
                Ok t -> succeed t
                Err err -> fail err
            )

ident : Parser String
ident =
    source <|
        ignore (Exactly 1) alpha
            |. ignore zeroOrMore identChar

sp : Parser ()
sp =
    ignore zeroOrMore (\c -> c==' ' || c=='\n' || c=='\t')

space : Parser ()
space =
    ignore oneOrMore (\c -> c==' ' || c=='\n' || c=='\t')