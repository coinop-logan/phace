module Demo exposing (main)

import Browser
import Element exposing (Element)
import Eth.Types exposing (Address)
import Eth.Utils
import Hex
import Html exposing (Html)
import List.Extra
import Phace
import Random
import Svg
import Svg.Attributes as SvgA


main =
    Browser.sandbox
        { init = ()
        , view = always view
        , update = always always ()
        }


generator : Random.Generator Char
generator =
    Random.int 0 15
        |> Random.map Hex.toString
        |> Random.map String.toList
        |> Random.map List.head
        |> Random.map (Maybe.withDefault '0')


testEthAddresses : List Address
testEthAddresses =
    List.range 0 100
        |> List.map Random.initialSeed
        |> List.map randomEthAddr


randomEthAddr : Random.Seed -> Address
randomEthAddr seed =
    let
        getNextChar : ( Int, Random.Seed ) -> Maybe ( Char, ( Int, Random.Seed ) )
        getNextChar ( at, seed_ ) =
            if at < 40 then
                Just <|
                    let
                        ( char, newSeed ) =
                            Random.step generator seed_
                    in
                    ( char, ( at + 1, newSeed ) )

            else
                Nothing
    in
    List.Extra.unfoldr getNextChar ( 0, seed )
        |> String.fromList
        |> Eth.Utils.unsafeToAddress


view : Html ()
view =
    Element.layout [] <|
        Element.wrappedRow
            [ Element.width Element.fill
            ]
            (List.map
                viewPhaceForAddress
                testEthAddresses
            )


viewPhaceForAddress : Address -> Element ()
viewPhaceForAddress address =
    Element.el [ Element.padding 10 ] <|
        (Phace.fromEthAddress address
            |> Maybe.map
                (\phace ->
                    Element.html <|
                        phace
                )
            |> Maybe.withDefault
                (Element.text "Error!")
        )
