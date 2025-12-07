module Types exposing (Theme(..), themeDecoder)

import Json.Decode


type Theme
    = Dark
    | Light


themeDecoder : Json.Decode.Decoder Theme
themeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\raw ->
                case raw of
                    "dark" ->
                        Json.Decode.succeed Dark

                    "light" ->
                        Json.Decode.succeed Light

                    _ ->
                        Json.Decode.fail ("Unexpected theme: " ++ raw)
            )
