module TupleHelpers exposing (..)

import List.Extra
import Tuple


extractTuple3Maybe : ( Maybe a, Maybe b, Maybe c ) -> Maybe ( a, b, c )
extractTuple3Maybe ( ma, mb, mc ) =
    case ( ma, mb, mc ) of
        ( Just a, Just b, Just c ) ->
            Just ( a, b, c )

        _ ->
            Nothing


tuple3ToList : ( a, a, a ) -> List a
tuple3ToList ( a, b, c ) =
    [ a, b, c ]


listToTuple3 : List a -> Maybe ( a, a, a )
listToTuple3 list =
    ( List.Extra.getAt 0 list
    , List.Extra.getAt 1 list
    , List.Extra.getAt 2 list
    )
        |> extractTuple3Maybe


mapTuple2 : (a -> b) -> ( a, a ) -> ( b, b )
mapTuple2 f =
    Tuple.mapBoth f f
