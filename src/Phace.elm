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
        |> Features.generatePhaceHalfFromString
        |> Maybe.map
            (\features ->
                svg
                    [ width "100"
                    , height "100"
                    , viewBox "-100 -100 200 200"
                    ]
                    (features
                        |> mirrorToNegativeX
                    )
            )


addressToRelevantString : Address -> String
addressToRelevantString =
    Eth.Utils.addressToString
        >> String.dropLeft 2



-- stringToSvgEls : String -> List (Svg msg)
-- stringToSvgEls s =
--     List.Extra.unfoldr addNextSvgElement ( 0, s )
-- addNextSvgElement : ( Int, String ) -> Maybe ( Svg msg, ( Int, String ) )
-- addNextSvgElement ( featureNum, remainingStr ) =
--     Features.getMaybeNextFeatureAndReduceStr featureNum remainingStr
--         |> Maybe.map
--             (\( nextFeature, remainingStr_ ) ->
--                 ( nextFeature, ( featureNum + 1, remainingStr_ ) )
--             )


mirrorToNegativeX : List (Svg msg) -> List (Svg msg)
mirrorToNegativeX svgs =
    [ g [ transform "scale(1,-1)" ] svgs
    , g
        [ transform
            "scale(-1,-1)"
        ]
        svgs
    ]



-- addressToReorderedString : Address -> String
-- addressToReorderedString address =
--     Eth.Utils.addressToString address
--         |> String.dropLeft 2
--         |> reorderFromEnds
-- reorderFromEnds : String -> String
-- reorderFromEnds s =
--     let
--         reorderHelper : ( String, String ) -> ( String, String )
--         reorderHelper ( built, source ) =
--             case String.length source of
--                 0 ->
--                     ( built, source )
--                 1 ->
--                     ( built ++ source, "" )
--                 _ ->
--                     let
--                         ( firstChar, lastChar, remaining ) =
--                             ( String.left 1 source
--                             , String.right 1 source
--                             , String.slice 1 -1 source
--                             )
--                         newBuilt =
--                             built ++ firstChar ++ lastChar
--                     in
--                     reorderHelper
--                         ( newBuilt, remaining )
--     in
--     Tuple.first <| reorderHelper ( "", s )
