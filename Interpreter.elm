module Interpreter exposing (interpret)

import String
import List
import Lexer exposing (Token(..))


interpret : String -> Result String Int
interpret code =
    code |> Lexer.tokenize |> Result.andThen expr


expr : List Token -> Result String Int
expr tokens =
    let
        tokenTuple =
            tokens
                |> parseLeftInteger
                |> Result.andThen
                    parseOp
                |> Result.andThen
                    parseRightInteger
                |> Result.andThen parseEOF
    in
        case tokenTuple of
            Ok ( left, op, right ) ->
                case op of
                    Plus ->
                        Ok (left + right)

                    Minus ->
                        Ok (left - right)

                    _ ->
                        Err ("Impossible token: " ++ (Lexer.tokenName op))

            Err msg ->
                Err msg


parseLeftInteger : List Token -> Result String ( Int, List Token )
parseLeftInteger tokens =
    case tokens of
        [] ->
            Err "Unexpected EOF"

        first :: rest ->
            case first of
                Integer val ->
                    Ok ( val, rest )

                _ ->
                    Err ("Unexpected token: " ++ (Lexer.tokenName first) ++ ", expected Integer.")


parseOp : ( Int, List Token ) -> Result String ( Int, Token, List Token )
parseOp ( val, tokens ) =
    case tokens of
        [] ->
            Err "Unexpected EOF"

        first :: rest ->
            case first of
                Plus ->
                    Ok ( val, first, rest )

                Minus ->
                    Ok ( val, first, rest )

                _ ->
                    Err ("Unexpected token: " ++ (Lexer.tokenName first) ++ ", expected +.")


parseRightInteger : ( Int, Token, List Token ) -> Result String ( Int, Token, Int, List Token )
parseRightInteger ( firstVal, op, tokens ) =
    case tokens of
        [] ->
            Err "Unexpected EOF"

        first :: rest ->
            case first of
                Integer val ->
                    Ok ( firstVal, op, val, rest )

                _ ->
                    Err ("Unexpected token: " ++ (Lexer.tokenName first) ++ ", expected Integer.")


parseEOF : ( Int, Token, Int, List Token ) -> Result String ( Int, Token, Int )
parseEOF ( firstVal, op, secondVal, tokens ) =
    case tokens of
        [] ->
            Ok ( firstVal, op, secondVal )

        first :: rest ->
            Err ("Unexpected token" ++ (Lexer.tokenName first) ++ ", expected EOF.")


liftFirstError : List (Result String Token) -> Result String (List Token)
liftFirstError ints =
    case (List.all isOk ints) of
        True ->
            Ok (List.map (\x -> Result.withDefault Plus x) ints)

        False ->
            Err (firstError ints)


firstError : List (Result String Token) -> String
firstError tokens =
    case (find (isOk >> not) tokens) of
        Just err ->
            case err of
                Ok val ->
                    "Impossible state in firstError"

                Err msg ->
                    msg

        Nothing ->
            "Impossible state in firstError"


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first
            else
                find predicate rest


isOk : Result String Token -> Bool
isOk result =
    case result of
        Ok value ->
            True

        Err msg ->
            False
