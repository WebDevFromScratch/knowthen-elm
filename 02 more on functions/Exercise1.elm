module Main exposing (..)

import Html
import String


(~=) string1 string2 =
    -- Alternate and simpler version (without accounting for if the letter is upper/lower case)
    -- String.left 1 string1 == String.left 1 string2
    let
        firstLetterString1 =
            -- This is the same as the version below, but this avoids nesting :)
            String.slice 0 1 string1 |> String.toLower

        firstLetterString2 =
            String.toLower (String.slice 0 1 string2)
    in
        firstLetterString1 == firstLetterString2


main =
    -- These two below are also the same --> currying, once more :)
    -- Html.text (toString ("Hey" ~= "ho"))
    --
    -- Solution to exercise 2 (super simple)
    -- (~=) "Hey" "ho"
    "Hey"
        ~= "ho"
        |> toString
        |> Html.text
