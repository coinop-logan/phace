module Phace exposing
    ( fromEthAddress, fromHexString, Error(..), errorToString
    , faceColorFromAddress
    )

{-|


# Making Phaces

@docs fromEthAddress, fromHexString, Error, errorToString, faceColorFromAddress

-}

import Eth.Types exposing (Address)
import Eth.Utils
import Features
import Html exposing (Html)
import List.Extra
import Maybe.Extra
import Result.Extra
import Svg exposing (..)
import Svg.Attributes exposing (..)
import SvgHelpers


{-| Phace generation will fail if the provided source is too short or not composed of hex characters.
-}
type Error
    = NotHexString
    | SourceTooSmall


{-| Get an explanation from an error type.
-}
errorToString : Error -> String
errorToString e =
    case e of
        NotHexString ->
            "String contains non-hex characters."

        SourceTooSmall ->
            "Source string is too short."


{-| Generate a phace from an Ethereum `Address`. Uses 32 chraracters, leaving 6 characters near the middle unused.

CAUTION: If you supply a malformed Eth address (maybe you were mucking about with `Eth.Utils.unsafeToAddress`?),
this will produce a plain div with an error message in it.

-}
fromEthAddress : Address -> Html msg
fromEthAddress address =
    address
        |> addressToRelevantString
        |> fromHexString
        |> Result.Extra.extract
            (errorToString
                >> (\s ->
                        Html.div []
                            [ Html.text <|
                                "Malformed ETH address: "
                                    ++ s
                            ]
                   )
            )


{-| Generate a phace from a hexadecimal `String`. Make sure to remove any "0x" from the source first.
-}
fromHexString : String -> Result Error (Html msg)
fromHexString src =
    let
        stringIsAllHex =
            src
                |> String.toList
                |> List.map Features.hexCharToFloat
                |> List.all (\f -> f <= 1)
    in
    if stringIsAllHex then
        src
            |> Features.generatePhaceFromString
            |> Maybe.map
                (svg
                    [ width "100"
                    , height "100"
                    , viewBox "-100 -100 200 200"
                    ]
                )
            |> Result.fromMaybe SourceTooSmall

    else
        Err NotHexString


addressToRelevantString : Address -> String
addressToRelevantString =
    Eth.Utils.addressToString
        >> String.dropLeft 2


{-| Just get the primary color of the Phace. Can be used to make a unique "character color" that matches the Phace.
-}
faceColorFromAddress : Address -> Result Error ( Float, Float, Float )
faceColorFromAddress address =
    let
        maybeFeature =
            Features.consumeFeatureFromString 0
                (addressToRelevantString address)
                |> Maybe.map Tuple.first
    in
    case maybeFeature of
        Just (Features.FaceColor ( r, g, b )) ->
            Ok <|
                ( Features.hexCharToFloat r
                , Features.hexCharToFloat g
                , Features.hexCharToFloat b
                )

        _ ->
            Err SourceTooSmall
