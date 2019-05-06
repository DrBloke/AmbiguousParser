module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String.Extra
import Set
import Parser exposing (Parser, (|.), (|=))


-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    String


init : Model
init =
    ""



-- UPDATE


type Msg
    = Compound String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Compound string ->
            string



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Enter formula or equation here", size 100, value model, onInput Compound ] []
        , div []
            [ (findCompounds [] [ ( "", model ) ])
                |> String.join " "
                |> text
            ]
        , div []
            [ Parser.run
                (Parser.succeed
                    (\a b -> a ++ b)
                    |= parserOneLetterElement
                    |= parserOneLetterElement
                )
                "co"
                |> Debug.toString
                |> text
            ]
        , div []
            [ Parser.run
                parserTwoLetterElement
                "co"
                |> Debug.toString
                |> text
            ]
        ]



--helpers


parserOneLetterElement : Parser String
parserOneLetterElement =
    (Parser.mapChompedString (\str _ -> String.toUpper str)
        (Parser.succeed ()
            |. Parser.chompIf isOneLetterElement
        )
    )


parserTwoLetterElement : Parser String
parserTwoLetterElement =
    (Parser.mapChompedString (\str _ -> String.Extra.toSentenceCase str)
        (Parser.succeed ()
            |. Parser.chompIf (\c -> Char.isAlpha c)
            |. Parser.chompIf (\c -> Char.isAlpha c)
        )
        |> Parser.andThen isTwoLetterElement
    )


isOneLetterElement : Char -> Bool
isOneLetterElement char =
    Set.member (String.fromChar (Char.toUpper char)) oneLetterElements


isTwoLetterElement : String -> Parser String
isTwoLetterElement str =
    if Set.member str twoLetterElements then
        Parser.succeed str
    else
        Parser.problem (str ++ " is not a valid element")


type alias Examiner =
    List ( String, String )



{--type alias Formatted =
    List String
--}


findCompounds : List String -> Examiner -> List String
findCompounds completed examiner =
    case examiner of
        [] ->
            let
                _ =
                    Debug.log "completed" completed

                _ =
                    Debug.log "examiner" examiner
            in
                completed

        ( done, toDo ) :: otherAttempts ->
            let
                _ =
                    Debug.log "completed" completed

                _ =
                    Debug.log "examiner" examiner

                ( oneComplete, oneExaminer ) =
                    findElement 1 done toDo

                ( twoComplete, twoExaminer ) =
                    if String.length toDo > 1 then
                        findElement 2 done toDo
                    else
                        ( [], [] )
            in
                findCompounds
                    (completed ++ oneComplete ++ twoComplete)
                    (oneExaminer ++ twoExaminer ++ otherAttempts)


findElement n done toDo =
    let
        heads =
            toDo
                |> String.left n
                |> String.Extra.toSentenceCase

        tail =
            String.dropLeft n toDo
    in
        if not (Set.member heads elements) && not (isSingleDigit n (String.toList toDo)) then
            ( [], [] )
        else if String.isEmpty tail then
            ( [ done ++ heads ], [] )
        else
            ( [], [ ( done ++ heads, tail ) ] )


isSingleDigit : Int -> List Char -> Bool
isSingleDigit n listChar =
    case listChar of
        [] ->
            False

        char :: emptyList ->
            if n == 1 && Char.isDigit char then
                True
            else
                False


elements =
    Set.fromList (modifiers ++ twoLetterElementsList ++ oneLetterElementsList)


modifiers =
    [ "("
    , ")"
    , "["
    , "]"
    , "{"
    , "}"
    , "+"
    , "-"
    ]


oneLetterElements =
    Set.fromList oneLetterElementsList


oneLetterElementsList =
    [ "H"
    , "B"
    , "C"
    , "N"
    , "O"
    , "F"
    , "S"
    , "I"
    , "K"
    , "P"
    , "V"
    , "Y"
    , "W"
    , "U"
    ]


twoLetterElements =
    Set.fromList twoLetterElementsList


twoLetterElementsList =
    [ "He"
    , "Li"
    , "Be"
    , "Ne"
    , "Na"
    , "Mg"
    , "Al"
    , "Zn"
    , "Cr"
    , "Si"
    , "Cl"
    , "Ar"
    , "Ca"
    , "Sc"
    , "Ti"
    , "Cr"
    , "Mn"
    , "Fe"
    , "Co"
    , "Ni"
    , "Cu"
    , "Zn"
    , "Ga"
    , "Ge"
    , "As"
    , "Se"
    , "Br"
    , "Kr"
    , "Rb"
    , "Sr"
    , "Zr"
    , "Nb"
    , "Mo"
    , "Tc"
    , "Ru"
    , "Rh"
    , "Pd"
    , "Ag"
    , "Cd"
    , "In"
    , "Sn"
    , "Sb"
    , "Te"
    , "Xe"
    , "Cs"
    , "Ba"
    , "La"
    , "Ce"
    , "Pr"
    , "Nd"
    , "Pm"
    , "Sm"
    , "Eu"
    , "Gd"
    , "Tb"
    , "Dy"
    , "Ho"
    , "Er"
    , "Tm"
    , "Yb"
    , "Lu"
    , "Hf"
    , "Ta"
    , "Re"
    , "Os"
    , "Ir"
    , "Pt"
    , "Au"
    , "Hg"
    , "Tl"
    , "Pb"
    , "Bi"
    , "Po"
    , "At"
    , "Rn"
    , "Fr"
    , "Ra"
    , "Ac"
    , "Th"
    , "Pa"
    , "Np"
    , "Pu"
    , "Am"
    , "Cm"
    , "Bk"
    , "Cf"
    , "Es"
    , "Fm"
    , "Md"
    , "No"
    , "Lr"
    , "Rf"
    , "Db"
    , "Sg"
    , "Bh"
    , "Hs"
    , "Mt"
    , "Ds"
    , "Rg"
    , "Cn"
    , "Nh"
    , "Fl"
    , "Mc"
    , "Lv"
    , "Ts"
    , "Og"
    ]
