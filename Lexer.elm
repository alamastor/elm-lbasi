module Lexer exposing (Token(..), tokenize, tokenName)

import Char


type Token
    = Integer Int
    | Plus
    | Minus


tokenName : Token -> String
tokenName token =
    case token of
        Integer val ->
            "Integer: " ++ (toString val)

        Plus ->
            "Plus"

        Minus ->
            "Minus"


tokenize : String -> Result String (List Token)
tokenize code =
    toTokens [] code


toTokens : List Token -> String -> Result String (List Token)
toTokens tokens code =
    let
        unconsed =
            String.uncons code
    in
        case unconsed of
            Just ( char, rest ) ->
                if char == '+' then
                    toTokens (tokens ++ [ Plus ]) rest
                else if char == '-' then
                    toTokens (tokens ++ [ Minus ]) rest
                else if Char.isDigit char then
                    let
                        ( t, s ) =
                            readInteger tokens (char |> String.fromChar |> unsafeToInt) rest
                    in
                        toTokens t s
                else if char == ' ' then
                    toTokens (tokens) rest
                else
                    Err ("Unexpected char :\"" ++ (String.fromChar char) ++ "\"")

            Nothing ->
                Ok tokens


readInteger : List Token -> Int -> String -> ( List Token, String )
readInteger tokens int code =
    let
        unconsed =
            String.uncons code
    in
        case unconsed of
            Just ( char, rest ) ->
                if Char.isDigit char then
                    let
                        newInt =
                            ((toString int) ++ (String.fromChar char)) |> unsafeToInt
                    in
                        readInteger tokens newInt rest
                else
                    ( tokens ++ [ Integer int ], code )

            Nothing ->
                ( tokens ++ [ Integer int ], code )


unsafeToInt : String -> Int
unsafeToInt intStr =
    intStr |> String.toInt |> Result.withDefault 0
