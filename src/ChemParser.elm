module ChemParser exposing (..)

import AmbiguousParser exposing (..)
import Parser exposing (..)
import String.Extra exposing (toTitleCase)


type Formula
    = Formula (List QuantifiedElement)


type QuantifiedElement
    = QuantifiedElement Element Int


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


runParseFormula : String -> Result ( ParseError, String ) Formula
runParseFormula inputText =
    runParser inputText parseFormula


runAmbiguousParseFormula : String -> ( Failures, List (List Element) )
runAmbiguousParseFormula inputText =
    runAmbiguousParser inputText parseAmbiguousFormula


parseAmbiguousFormula : String -> AmbiguousParser (List Element)
parseAmbiguousFormula str =
    ambiRepeat oneOrMore str parseAmbiguousElement


parseFormula : String -> ( Parser Formula, String )
parseFormula str =
    repeat oneOrMore str parseQuantifiedElement
        |> map Formula
        |> ignore parseEnd


parseQuantifiedElement : String -> ( Parser QuantifiedElement, String )
parseQuantifiedElement str =
    succeed QuantifiedElement str
        |> keep parseElement
        |> keep
            (oneOf
                [ parseInt
                , succeed 1
                ]
            )


makeFormula : List QuantifiedElement -> Formula
makeFormula quantifiedElements =
    Formula quantifiedElements


parseElement : String -> ( Parser Element, String )
parseElement str =
    oneOf [ parseTwoLetterElement, parseOneLetterElement ] str


parseAmbiguousElement : String -> AmbiguousParser Element
parseAmbiguousElement str =
    anyOf [ ambiguate parseOneLetterElement, ambiguate parseTwoLetterElement ] str


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
            Parser.Success element
