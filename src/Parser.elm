module Parser exposing (..)

import String.Extra exposing (toTitleCase)


type Parser a
    = Fail ParseError
    | Success a


type ParseError
    = ExpectedCharacter
    | ExpectedTwoChars
    | ExpectedElement
    | ExpectedListOfParsers


type
    Formula
    --= Formula ( Element, Int )
    = Formula (List Element)


type Element
    = H
    | Li
    | C
    | N
    | O
    | Na
    | K
    | I
    | Ca
    | Cl
    | Co


stringToElement : String -> Maybe Element
stringToElement str =
    case toTitleCase str of
        "H" ->
            Just H

        "C" ->
            Just C

        "N" ->
            Just N

        "O" ->
            Just O

        "K" ->
            Just K

        "Cl" ->
            Just Cl

        "I" ->
            Just I

        "Na" ->
            Just Na

        "Li" ->
            Just Li

        "Ca" ->
            Just Ca

        "Co" ->
            Just Co

        _ ->
            Nothing


runParseFormula : String -> Result ParseError Formula
runParseFormula inputText =
    runParser inputText parseFormula


parseFormula : String -> ( Parser Formula, String )
parseFormula str =
    parseElement str
        |> map makeFormula
        |> andMap parseElement


andMap : (String -> ( Parser a, String )) -> ( Parser (a -> b), String ) -> ( Parser b, String )
andMap parseA parseFn =
    case parseFn of
        ( Fail error, str ) ->
            ( Fail error, str )

        ( Success fn, str ) ->
            case parseA str of
                ( Fail error, _ ) ->
                    ( Fail error, str )

                ( Success a, newStr ) ->
                    ( Success (fn a), newStr )


makeFormula : Element -> Element -> Formula
makeFormula el1 el2 =
    Formula (el1 :: [ el2 ])



--|> andMap parseDigit


map : (a -> b) -> ( Parser a, String ) -> ( Parser b, String )
map fn parserA =
    case parserA of
        ( Success a, str ) ->
            ( Success (fn a), str )

        ( Fail error, str ) ->
            ( Fail error, str )


succeed : a -> (String -> ( Parser a, String ))
succeed a =
    \str -> ( Success a, str )


runParser : String -> (String -> ( Parser a, String )) -> Result ParseError a
runParser inputStr parseFunction =
    case parseFunction inputStr of
        ( Success str, rest ) ->
            Ok str

        ( Fail parseError, _ ) ->
            Err parseError


parseChar : String -> ( Parser String, String )
parseChar str =
    case String.uncons str of
        Nothing ->
            ( Fail ExpectedCharacter, str )

        Just ( char, rest ) ->
            ( Success (String.fromChar char), rest )


parseTwoChars : String -> ( Parser String, String )
parseTwoChars str =
    case parseChar str of
        ( Fail error, _ ) ->
            ( Fail error, str )

        ( Success inputStr, strLessOneChar ) ->
            case String.uncons strLessOneChar of
                Nothing ->
                    ( Fail ExpectedTwoChars, inputStr )

                Just ( char, rest ) ->
                    ( Success (inputStr ++ String.fromChar char), rest )


parseElement : String -> ( Parser Element, String )
parseElement str =
    oneOf [ parseOneLetterElement, parseTwoLetterElement ] str


parseOneLetterElement : String -> ( Parser Element, String )
parseOneLetterElement str =
    parseChar str
        |> andThen validateElement


parseTwoLetterElement : String -> ( Parser Element, String )
parseTwoLetterElement str =
    parseTwoChars str
        |> andThen validateElement


validateElement : String -> Parser Element
validateElement str =
    case stringToElement str of
        Nothing ->
            Fail ExpectedElement

        Just element ->
            Success element


oneOf : List (String -> ( Parser a, String )) -> String -> ( Parser a, String )
oneOf parsers str =
    case parsers of
        [] ->
            ( Fail ExpectedListOfParsers, str )

        x :: [] ->
            case x str of
                ( Success a, rest ) ->
                    ( Success a, rest )

                ( Fail error, _ ) ->
                    ( Fail error, str )

        x :: xs ->
            --recursive
            case x str of
                ( Success a, rest ) ->
                    ( Success a, rest )

                _ ->
                    oneOf xs str


andThen : (a -> Parser b) -> ( Parser a, String ) -> ( Parser b, String )
andThen fn input =
    case input of
        ( Fail error, str ) ->
            ( Fail error, str )

        ( Success str, inputStr ) ->
            ( fn str, inputStr )
