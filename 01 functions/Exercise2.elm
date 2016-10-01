module Main exposing (..)

import Html
import String


toUpperIfLongerThan10 name =
    if String.length name > 10 then
        String.toUpper name
    else
        name


prettifiedToUpperIfLongerThan10 name =
    (toUpperIfLongerThan10 name)
        ++ " - name length: "
        ++ toString (String.length name)


main =
    Html.text (prettifiedToUpperIfLongerThan10 "Piotr Klosinski")
