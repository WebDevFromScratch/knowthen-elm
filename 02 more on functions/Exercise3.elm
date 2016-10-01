module Main exposing (..)

import Html
import String


wordCount : String -> Int
wordCount sentence =
    String.split " " sentence |> List.length


wordCountAlt =
    -- Function composition
    String.words >> List.length


main =
    -- wordCount "Lorem ipsum with five or more words."
    --     |> toString
    --     |> Html.text
    "Lorem ipsum with five or more words."
        |> wordCountAlt
        |> toString
        |> Html.text
