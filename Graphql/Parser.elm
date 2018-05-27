module Graphql.Parser exposing (..)

import Parser
    exposing
        ( Parser
        , Count(..)
        , (|.)
        , (|=)
        , repeat
        , ignore
        , symbol
        , oneOf
        , succeed
        , oneOrMore
        , zeroOrMore
        , source
        , fail
        , andThen
        , delayedCommit
        )
import Graphql.Util exposing (sp, space, sequence, ident, liftResult)


type Type
    = IntType
    | StringType
    | FloatType
    | Required Type
    | ListOf Type
    | Ref Ref


type alias Ref =
    String


type alias Field =
    { name : String, typ : Type }


type alias RecordType =
    { name : String, fields : List Field }


type alias InterfaceType =
    { name : String, fields : List Field }


type Definition
    = RecordDef RecordType
    | InterfaceDef InterfaceType


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
makeRecordDef name fields =
    RecordDef { name = name, fields = fields }


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
    succeed (InterfaceDef { name = "test", fields = [] })


makeField : String -> Type -> Field
makeField name typ =
    { name = name, typ = typ }


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
        "int" ->
            Ok IntType

        "string" ->
            Ok StringType

        "float" ->
            Ok FloatType

        _ ->
            Err <| "bad type name:" ++ name


typeParser : Parser Type
typeParser =
    ident
        |> Parser.map parseType
        |> liftResult
