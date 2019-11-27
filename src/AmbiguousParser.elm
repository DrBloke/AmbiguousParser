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
runAmbiguousParser inputText parserA =
    let
        (AmbiguousParser failures successes) =
            parserA inputText
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
    -> AmbiguousParser a
anyOf ambiParsers str =
    List.foldl (combAmbi str) (AmbiguousParser [] []) ambiParsers


combAmbi :
    String
    -> (String -> AmbiguousParser a)
    -> AmbiguousParser a
    -> AmbiguousParser a
combAmbi str ambiParser (AmbiguousParser failures successes) =
    let
        (AmbiguousParser failure success) =
            ambiParser str
    in
    AmbiguousParser (failure ++ failures) (success ++ successes)


ambiKeep :
    (String -> AmbiguousParser a)
    -> AmbiguousParser (a -> b)
    -> AmbiguousParser b
ambiKeep parserA (AmbiguousParser failures successFuncs) =
    List.foldl (foldAndKeepFns parserA) (AmbiguousParser failures []) successFuncs


foldAndKeepFns :
    (String -> AmbiguousParser a)
    -> Success (a -> b)
    -> AmbiguousParser b
    -> AmbiguousParser b
foldAndKeepFns parserA (Success fn str) (AmbiguousParser failures successes) =
    let
        (AmbiguousParser fails succs) =
            parserA str
    in
    AmbiguousParser (fails ++ failures)
        (List.map
            (\(Success value remainStr) -> Success (fn value) remainStr)
            succs
            ++ successes
        )


ambiIgnore :
    (String -> AmbiguousParser a)
    -> AmbiguousParser b
    -> AmbiguousParser b
ambiIgnore parserA (AmbiguousParser failures successValues) =
    List.foldl (foldAndIgnoreFns parserA) (AmbiguousParser failures []) successValues


foldAndIgnoreFns :
    (String -> AmbiguousParser a)
    -> Success b
    -> AmbiguousParser b
    -> AmbiguousParser b
foldAndIgnoreFns parserA (Success value str) (AmbiguousParser failures successes) =
    let
        (AmbiguousParser fails succs) =
            parserA str
    in
    AmbiguousParser (fails ++ failures)
        (List.map
            (\(Success _ remainStr) -> Success value remainStr)
            succs
            ++ successes
        )



--TODO


ambiRepeat : Count -> String -> (String -> AmbiguousParser a) -> AmbiguousParser (List a)
ambiRepeat count str ambiParser =
    case count of
        AtLeast x ->
            --try parser at least x times and then until fails
            case runAmbiParserXTimes x [] str ambiParser of
                AmbiguousParser [] [ Success succs unpassedStr ] ->
                    runAmbiParserMaxTimes succs unpassedStr ambiParser

                AmbiguousParser failures successes ->
                    AmbiguousParser failures successes

        Exactly x ->
            --try parser x times
            runAmbiParserXTimes x [] str ambiParser


runAmbiParserXTimes : Int -> List a -> String -> (String -> AmbiguousParser a) -> AmbiguousParser (List a)
runAmbiParserXTimes x successes str ambiParserA =
    let
        count =
            x
    in
    case ambiParserA str of
        AmbiguousParser [] [ Success succs restStr ] ->
            if count == 1 then
                AmbiguousParser [] [ Success (List.reverse (succs :: successes)) restStr ]

            else
                runAmbiParserXTimes (count - 1) (succs :: successes) restStr ambiParserA

        AmbiguousParser failures _ ->
            AmbiguousParser failures [ Success successes str ]


runAmbiParserMaxTimes : List a -> String -> (String -> AmbiguousParser a) -> AmbiguousParser (List a)
runAmbiParserMaxTimes successes str ambiParserA =
    case ambiParserA str of
        AmbiguousParser [] [ Success succs restStr ] ->
            runAmbiParserMaxTimes (succs :: successes) restStr ambiParserA

        AmbiguousParser failures succs ->
            AmbiguousParser failures [ Success (List.reverse successes) str ]
