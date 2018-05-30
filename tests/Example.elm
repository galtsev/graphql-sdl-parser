module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Graphql.Parser exposing (..)
import Parser exposing (run)


expectOk : Result x a -> Expectation
expectOk res =
    case res of
        Ok _ -> Expect.pass
        Err err -> Expect.fail <| toString err

expectType : Type -> Result x Field -> Expectation
expectType etyp res =
    case res of
        Ok {typ} ->
            if typ==etyp 
                then Expect.pass
                else Expect.fail <| "type mismatch" ++ toString typ
        Err err -> Expect.fail <| toString err

checkParseType : String -> Type -> Test
checkParseType src expectedType =
    let
        fn () =
            run typeParser src
                |> Expect.equal (Ok expectedType)
    in
    test src fn


typeParserTest : Test
typeParserTest =
    describe "type parser"
        [ checkParseType "Int" IntType
        , checkParseType "Int!" <| Required IntType
        , checkParseType "[Int]" <| ListOf IntType
        , checkParseType "[String]" <| ListOf StringType
        , checkParseType "[ID!]" <| ListOf (Required IDType)
        , checkParseType "[Int!]!" <| Required (ListOf (Required IntType))
        , checkParseType "[String]!" <| Required (ListOf StringType)

        ]

checkParseField : String -> String -> Type -> Test
checkParseField src fldName expectedType =
    let
        fn () = run fieldParser src 
            |> Expect.equal (Ok (Field fldName expectedType))
    in
    test src fn

fieldParserTest : Test
fieldParserTest = --skip <|
    describe "field parser"
        [ checkParseField "id: Int" "id" IntType
        , checkParseField "weigth :Float" "weigth" FloatType
        , checkParseField "name : String" "name" StringType
        , checkParseField "id: ID" "id" IDType
        , checkParseField "email: String!" "email" (Required StringType)
        , checkParseField "parents: [Int]" "parents" (ListOf IntType)
        ]

recordParserTest : Test
recordParserTest = --skip <|
    describe "record parser"
        [ test "recordTypeParser" <|
            \_ ->
                """type MyType {id: Int name: String }
                """
                    |> run recordTypeParser
                    |> expectOk
        , test "recordTypeParser multiline" <|
            \_ ->
                """type MyType {
                    id: Int
                    name: String
                }
                """
                    |> run recordTypeParser
                    |> expectOk
        , test "document with two types" <|
            \_ ->
                let
                    extractDef def =
                        case def of
                            RecordDef {name,fields} -> List.map (\f->name++"."++f.name) fields
                            InterfaceDef _ -> []
                    extractFields defs =
                        case defs of
                            Err err -> []
                            Ok defs ->
                                List.concatMap extractDef defs
                in
                """
                type Post {
                    id: Int
                    title: String
                    stars: Int
                }
                type Comment {
                    id: ID
                    user: String
                    post_id: Int
                }
                """
                    |> run parser
                    |> extractFields
                    |> Expect.equalLists
                        [ "Post.id"
                        , "Post.title"
                        , "Post.stars"
                        , "Comment.id"
                        , "Comment.user"
                        , "Comment.post_id"
                        ]
        , test "document with comments" <|
            \_ ->
                """
                # document header
                type Post{
                    # field comment
                    id: ID
                    # field comment 2
                    name: String #inline comment
                }
                """
                    |> run parser
                    |> expectOk
        ]