module Phace exposing (fromEthAddress, fromString)

{-| ### A _phace_ is an [identicon](https://en.wikipedia.org/wiki/Identicon) styled to look like a face. This package generates phaces from [elm-ethereum](https://package.elm-lang.org/packages/cmditch/elm-ethereum/latest/) addresses or strings.


# Making Phaces
@docs fromEthAddress, fromString

-}

import Eth.Types exposing (Address)
import Eth.Utils
import Features
import Html exposing (Html)
import List.Extra
import Maybe.Extra
import Svg exposing (..)
import Svg.Attributes exposing (..)


{-| Generate a phace from an Ethereum `Address`.
-}
fromEthAddress : Address -> Html msg
fromEthAddress address =
    address
        |> addressToRelevantString
        |> fromString
        |> Maybe.withDefault (Html.div [] [ Html.text "Malformed Ethereum address" ])

{-| Generate a phace from a `String`. Returns `Nothing` if the string is less than 33 characters.
-}
fromString : String -> Maybe (Html msg)
fromString src =
    src
        |> Features.generatePhaceFromString
        |> Maybe.map
            (svg
                [ width "100"
                , height "100"
                , viewBox "-100 -100 200 200"
                ]
            )


addressToRelevantString : Address -> String
addressToRelevantString =
    Eth.Utils.addressToString
        >> String.dropLeft 2
