module Main exposing (..)

import Html


type alias CartItem =
    { name : String, qty : Int, freeQty : Int }


cart : List CartItem
cart =
    [ { name = "Lemon", qty = 1, freeQty = 0 }
    , { name = "Apple", qty = 5, freeQty = 0 }
    , { name = "Pear", qty = 10, freeQty = 0 }
    ]


giveFreeItems : Int -> Int -> CartItem -> CartItem
giveFreeItems minQty freeQtyToReceive item =
    if item.freeQty < 1 && item.qty >= minQty then
        { item | freeQty = freeQtyToReceive }
    else
        item


mappedCart : List CartItem
mappedCart =
    List.map ((giveFreeItems 10 3) >> (giveFreeItems 5 1)) cart


main : Html.Html msg
main =
    mappedCart
        |> toString
        |> Html.text
