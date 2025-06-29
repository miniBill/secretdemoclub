module Parser.Extra exposing (errorsToString)

import Ansi.Color
import Json.Encode
import List.Extra
import Parser
import Set


errorsToString : String -> List Parser.DeadEnd -> String
errorsToString source errors =
    errors
        |> List.Extra.gatherEqualsBy (\{ row, col } -> ( row, col ))
        |> List.map (errorToString source)
        |> List.Extra.unique
        |> String.join "\n---\n"


errorToString : String -> ( Parser.DeadEnd, List Parser.DeadEnd ) -> String
errorToString source ( error, errors ) =
    let
        -- How many lines of context to show
        context : Int
        context =
            4

        lines : List String
        lines =
            String.split "\n" source
                |> List.drop (error.row - context)
                |> List.take (context * 2)
                |> List.map (String.replace "\t" " ")

        errorString : String
        errorString =
            [ String.repeat (error.col - 1) " "
            , Ansi.Color.fontColor Ansi.Color.red "^ "
            , " at row "
            , String.fromInt error.row
            , ", col "
            , String.fromInt error.col
            , " "
            , (error :: errors)
                |> List.map (\{ problem } -> problemToString problem)
                |> Set.fromList
                |> Set.toList
                |> String.join ", "
                |> Ansi.Color.fontColor Ansi.Color.yellow
            ]
                |> String.concat

        before : Int
        before =
            min error.row context
    in
    List.take before lines
        ++ errorString
        :: List.drop before lines
        |> String.join "\n"


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Parser.BadRepeat ->
            "Bad repeat"

        Parser.Expecting something ->
            "Expecting " ++ something

        Parser.ExpectingInt ->
            "Expecting int"

        Parser.ExpectingHex ->
            "Expecting hex"

        Parser.ExpectingOctal ->
            "Expecting octal"

        Parser.ExpectingBinary ->
            "Expecting binary"

        Parser.ExpectingFloat ->
            "Expecting float"

        Parser.ExpectingNumber ->
            "Expecting number"

        Parser.ExpectingVariable ->
            "Expecting variable"

        Parser.ExpectingSymbol value ->
            "Expecting symbol " ++ escape value

        Parser.ExpectingKeyword value ->
            "Expecting keyword " ++ value

        Parser.ExpectingEnd ->
            "Expecting end"

        Parser.UnexpectedChar ->
            "Unexpected char"

        Parser.Problem p ->
            p


escape : String -> String
escape input =
    input
        |> Json.Encode.string
        |> Json.Encode.encode 0
