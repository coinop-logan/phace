module Phace exposing (..)

import Eth.Types exposing (Address)
import Eth.Utils
import Features
import Html exposing (Html)
import List.Extra
import Maybe.Extra
import Svg exposing (..)
import Svg.Attributes exposing (..)


fromEthAddress : Address -> Maybe (Html msg)
fromEthAddress address =
    address
        |> addressToRelevantString
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