module Parser exposing (..)


type Parser a
    = Fail ParseError
    | Success a


type ParseError
    = ExpectedCharacter
    | ExpectedTwoChars
    | ExpectedElement
    | ExpectedListOfParsers
    | ExpectedEnd
    | ExpectedInt
    | ExpectedEndOfInt


parseInt : String -> ( Parser Int, String )
parseInt str =
    repeat oneOrMore str parseDigit
        |> map concatToInt
        |> andThen validatePositiveInt


validatePositiveInt : Int -> Parser Int
validatePositiveInt int =
    --string parsing prevents negative ints
    --a max int is mainly put in to prevent big int problems
    if int > 1000 then
        Fail ExpectedEndOfInt

    else
        Success int


concatToInt : List Int -> Int
concatToInt ints =
    joinInts 0 1 (List.reverse ints)


joinInts : Int -> Int -> List Int -> Int
joinInts acc multiplier ints =
    case ints of
        [] ->
            acc

        x :: xs ->
            joinInts (acc + x * multiplier) (multiplier * 10) xs


keep : (String -> ( Parser a, String )) -> ( Parser (a -> b), String ) -> ( Parser b, String )
keep parseA parseFn =
    case parseFn of
        ( Fail error, str ) ->
            ( Fail error, str )

        ( Success fn, str ) ->
            case parseA str of
                ( Fail error, _ ) ->
                    ( Fail error, str )

                ( Success a, newStr ) ->
                    ( Success (fn a), newStr )


ignore : (String -> ( Parser a, String )) -> ( Parser b, String ) -> ( Parser b, String )
ignore parseA parseFn =
    case parseFn of
        ( Fail error, str ) ->
            ( Fail error, str )

        ( Success fn, str ) ->
            case parseA str of
                ( Fail error, _ ) ->
                    ( Fail error, str )

                ( Success _, _ ) ->
                    ( Success fn, str )


type Count
    = AtLeast Int
    | Exactly Int


zeroOrMore : Count
zeroOrMore =
    AtLeast 0


oneOrMore : Count
oneOrMore =
    AtLeast 1


repeat : Count -> String -> (String -> ( Parser a, String )) -> ( Parser (List a), String )
repeat count str parser =
    case count of
        AtLeast x ->
            --try parser at least x times and then until fails
            case runParserXTimes x [] str parser of
                ( Success values, unpassedStr ) ->
                    runParserMaxTimes values unpassedStr parser

                ( Fail error, _ ) ->
                    ( Fail error, str )

        Exactly x ->
            --try parser x times
            runParserXTimes x [] str parser


runParserXTimes : Int -> List a -> String -> (String -> ( Parser a, String )) -> ( Parser (List a), String )
runParserXTimes x successes str parserA =
    let
        count =
            x
    in
    case parserA str of
        ( Success a, restStr ) ->
            if count == 1 then
                ( Success (List.reverse (a :: successes)), restStr )

            else
                runParserXTimes (count - 1) (a :: successes) restStr parserA

        ( Fail error, _ ) ->
            ( Fail error, str )


runParserMaxTimes : List a -> String -> (String -> ( Parser a, String )) -> ( Parser (List a), String )
runParserMaxTimes successes str parserA =
    case parserA str of
        ( Success a, restStr ) ->
            runParserMaxTimes (a :: successes) restStr parserA

        ( Fail error, _ ) ->
            ( Success (List.reverse successes), str )


map : (a -> b) -> ( Parser a, String ) -> ( Parser b, String )
map fn parserA =
    case parserA of
        ( Success a, str ) ->
            ( Success (fn a), str )

        ( Fail error, str ) ->
            ( Fail error, str )


succeed : a -> (String -> ( Parser a, String ))
succeed a str =
    ( Success a, str )


runParser : String -> (String -> ( Parser a, String )) -> Result ( ParseError, String ) a
runParser inputStr parseFunction =
    case parseFunction inputStr of
        ( Success str, rest ) ->
            Ok str

        ( Fail parseError, unpassedStr ) ->
            Err ( parseError, unpassedStr )


parseEnd : String -> ( Parser (), String )
parseEnd str =
    case String.uncons str of
        Nothing ->
            ( Success (), str )

        Just ( char, rest ) ->
            ( Fail ExpectedEnd, str )


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


parseDigit : String -> ( Parser Int, String )
parseDigit str =
    parseChar str
        |> andThen validateDigit


validateDigit : String -> Parser Int
validateDigit str =
    case String.toInt str of
        Nothing ->
            Fail ExpectedInt

        Just int ->
            Success int


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

        ( Success a, inputStr ) ->
            ( fn a, inputStr )
