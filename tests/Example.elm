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
suite =
    describe "parser"
        [ test "fieldParser int" <|
            \_ ->
                run fieldParser "id: int"
                    |> expectType IntType
        , test "fieldParser float" <|
            \_ ->
                run fieldParser "weigth :float"
                    |> expectType FloatType
        , test "fieldParser string" <|
            \_ ->
                run fieldParser "name : string"
                    |> expectType StringType
        , test "recordTypeParser" <|
            \_ ->
                """type MyType {id: int name: string }
                """
                    |> run recordTypeParser
                    |> expectOk
        , test "recordTypeParser multiline" <|
            \_ ->
                """type MyType {
                    id: int
                    name: string
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
                    id: int
                    title: string
                    stars: int
                }
                type Comment {
                    id: int
                    user: string
                    post_id: int
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
        ]