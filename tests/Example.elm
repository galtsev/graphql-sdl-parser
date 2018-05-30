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

suite : Test
suite = --skip <|
    describe "parser"
        [ test "fieldParser int" <|
            \_ ->
                run fieldParser "id: Int"
                    |> expectType IntType
        , test "fieldParser float" <|
            \_ ->
                run fieldParser "weigth :Float"
                    |> expectType FloatType
        , test "fieldParser string" <|
            \_ ->
                run fieldParser "name : String"
                    |> expectType StringType
        , test "fieldParser ID" <|
            \_ ->
                run fieldParser "id: ID"
                    |> expectType IDType
        , test "recordTypeParser" <|
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