module AmbiguousParser exposing (..)

import Parser exposing (..)


type AmbiguousParser a
    = AmbiguousParser Failures (Successes a)


type alias Failures =
    List Failure


type alias Successes a =
    List (Success a)


type Failure
    = Failure ParseError String


type Success a
    = Success a String


runAmbiguousParser : String -> (String -> AmbiguousParser a) -> ( Failures, List a )
runAmbiguousParser inputText parseFormula =
    let
        (AmbiguousParser failures successes) =
            parseFormula inputText
    in
    ( failures, List.map (\(Success value _) -> value) successes )


ambiguate : (String -> ( Parser a, String )) -> String -> AmbiguousParser a
ambiguate parser str =
    case parser str of
        ( Parser.Success value, remainStr ) ->
            AmbiguousParser [] [ Success value remainStr ]

        ( Parser.Fail failure, _ ) ->
            AmbiguousParser [ Failure failure str ] []


mapAll : (a -> b) -> AmbiguousParser a -> AmbiguousParser b
mapAll fn (AmbiguousParser failures successes) =
    AmbiguousParser
        failures
        (List.map
            (\(Success value unparsedStr) ->
                Success (fn value) unparsedStr
            )
            successes
        )



--andThenAll I believe is not needed. mapAll does the same thing as it would do


anyOf :
    List (String -> AmbiguousParser a)
    -> String
    -> AmbiguousParser b
anyOf ambiParsers str =
    List.foldl (combAmbi str) (AmbiguousParser [] []) ambiParsers


combAmbi :
    String
    -> (String -> AmbiguousParser a)
    -> AmbiguousParser a
    -> AmbiguousParser a
combAmbi str ambiParser (AmbiguousParser failures successes) =
    case ambiParser str of
        AmbiguousParser [] success ->
            AmbiguousParser failures (success ++ successes)

        AmbiguousParser failure [] ->
            AmbiguousParser (failure ++ failures) successes


ambiKeep :
    (String -> AmbiguousParser a)
    -> AmbiguousParser (a -> b)
    -> AmbiguousParser b
ambiKeep parserA (AmbiguousParser failures successFuncs) =
    List.foldl (foldFns parserA) (AmbiguousParser failures []) successFuncs


foldFns :
    (String -> AmbiguousParser a)
    -> Success (a -> b)
    -> AmbiguousParser b
    -> AmbiguousParser b
foldFns parserA [ Success fn str ] (AmbiguousParser failures successes) =
    case parserA str of
        AmbiguousParser [] [ Success value remainStr ] ->
            AmbiguousParser failures (Success (fn value) :: successes)

        AmbiguousParser failure [] ->
            AmbiguousParser (failure :: failures) successes
