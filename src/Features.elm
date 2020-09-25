module Features exposing (..)

import Hex
import List.Extra
import Maybe.Extra
import Svg exposing (..)
import Svg.Attributes as SvgA
import SvgHelpers exposing (..)
import Tuple2
import Tuple3
import TupleHelpers exposing (..)
import Phace.Types exposing (..)


type Feature
    = FaceColor Color
    | Eye EyeData
    | Mouth MouthData
    | Hair HairData



-- | Cheek CheekData
-- | Shirt ShirtData


generatePhaceFromString : String -> Maybe (List (Svg ms))
generatePhaceFromString fullSrc =
    generateFeaturesFromString fullSrc
        |> Maybe.map
            (List.map renderFeature
                >> List.concat
                >> flipVertical
            )


generateFeaturesFromString : String -> Maybe (List Feature)
generateFeaturesFromString fullSrc =
    let
        --_ = Debug.log "src" fullSrc
        helper : Int -> Maybe ( String, List Feature ) -> Maybe ( String, List Feature )
        helper featureId maybeAcc =
            maybeAcc
                |> Maybe.andThen
                    (\( src, features ) ->
                        consumeFeatureFromString featureId src
                            |> Maybe.map
                                (\( newFeature, newSrc ) ->
                                    ( newSrc
                                    , List.append features [ newFeature ]
                                    )
                                )
                    )
    in
    List.range 0 3
        |> List.foldl helper (Just ( fullSrc, [] ))
        -- |> Debug.log "remaining"
        |> Maybe.map Tuple.second



-- Currently leaves 8 chars left to consume


consumeFeatureFromString : Int -> String -> Maybe ( Feature, String )
consumeFeatureFromString featureId src =
    case featureId of
        0 ->
            consumeColor src
                |> Maybe.map (Tuple.mapFirst FaceColor)

        1 ->
            consumeEye src
                |> Maybe.map (Tuple.mapFirst Eye)

        2 ->
            consumeMouth src
                |> Maybe.map (Tuple.mapFirst Mouth)

        3 ->
            src
                |> String.reverse
                |> consumeHair
                |> Maybe.map (Tuple.mapFirst Hair)
                |> Maybe.map (Tuple.mapSecond String.reverse)

        _ ->
            Nothing


renderFeature : Feature -> List (Svg msg)
renderFeature feature =
    case feature of
        FaceColor color ->
            renderFaceColor color

        Eye eye ->
            renderEyes eye

        Mouth mouth ->
            renderMouth mouth

        Hair hair ->
            renderHair hair


consumeColor : String -> Maybe ( Color, String )
consumeColor src =
    let
        charList =
            String.toList src
    in
    ( List.Extra.getAt 0 charList
    , List.Extra.getAt 1 charList
    , List.Extra.getAt 2 charList
    )
        |> extractTuple3Maybe
        |> Maybe.map
            (\color ->
                ( color, String.dropLeft 3 src )
            )


renderFaceColor : Color -> List (Svg msg)
renderFaceColor color =
    [ rect
        [ fillColor color
        , SvgA.stroke "none"
        , SvgA.width "200"
        , SvgA.height "200"
        , SvgA.y "-100"
        , SvgA.x "-100"
        ]
        []
    ]


type alias EyeData =
    { color : Color
    , points : List Point
    }


consumeEye : String -> Maybe ( EyeData, String )
consumeEye src =
    src
        |> consumeColor
        |> Maybe.andThen
            (\( color, src1 ) ->
                consumePoints 3 src1
                    |> Maybe.map
                        (\( srcPoints, remaining ) ->
                            ( makeEye color srcPoints
                            , remaining
                            )
                        )
            )


makeEye : Color -> List Point -> EyeData
makeEye color srcPoints =
    let
        points =
            srcPoints
                |> List.map
                    (scalePointToRect
                        ( ( 25, 10 ), ( 60, 50 ) )
                    )
    in
    EyeData
        color
        points


renderEyes : EyeData -> List (Svg msg)
renderEyes eye =
    mirrorHorizontal
        [ polygon
            [ fillColor eye.color
            , strokeColor eye.color
            , SvgA.strokeWidth "6"
            , SvgA.strokeLinejoin "round"
            , points eye.points
            ]
            []
        ]


type alias MouthData =
    { color : Color
    , startPoint : Point
    , bcPoint1 : Point
    , bcPoint2 : Point
    }


consumeMouth : String -> Maybe ( MouthData, String )
consumeMouth src =
    src
        |> consumeColor
        |> Maybe.andThen
            (\( color, src1 ) ->
                consumePoints 3 src1
                    |> Maybe.map
                        (\( srcPoints, remaining ) ->
                            ( makeMouth color srcPoints
                            , remaining
                            )
                        )
            )


makeMouth : Color -> List Point -> MouthData
makeMouth color srcPoints =
    let
        ( startPointSrc, bcPoint1Src, bcPoint2Src ) =
            listToTuple3 srcPoints
                |> squashMaybe (Tuple3.triple ( 0, 0 ))

        startPoint =
            startPointSrc |> scalePointToRect ( ( 10, -30 ), ( 70, -80 ) )

        bcPoints =
            ( bcPoint1Src, bcPoint2Src )
                |> mapTuple2
                    (scalePointToRect
                        (pointToBoundingRect ( 20, 40 ) startPoint)
                    )
    in
    MouthData
        color
        startPoint
        (Tuple.first bcPoints)
        (Tuple.second bcPoints)


renderMouth : MouthData -> List (Svg msg)
renderMouth mouth =
    [ path
        [ fillColor mouth.color
        , strokeColor mouth.color
        , SvgA.strokeWidth "6"
        , SvgA.strokeLinejoin "round"
        , bezierPath
            mouth.startPoint
            [ ( mouth.bcPoint1
              , negateX mouth.bcPoint1
              , negateX mouth.startPoint
              )
            , ( negateX mouth.bcPoint2
              , mouth.bcPoint2
              , mouth.startPoint
              )
            ]
        ]
        []
    ]


type alias HairData =
    { color : Color
    , points : List Point
    }


consumeHair : String -> Maybe ( HairData, String )
consumeHair src =
    src
        |> consumeColor
        |> Maybe.andThen
            (\( color, src1 ) ->
                consumePoints 5 src1
                    |> Maybe.map
                        (\( srcPoints, remaining ) ->
                            ( makeHair color srcPoints
                            , remaining
                            )
                        )
            )


makeHair : Color -> List Point -> HairData
makeHair color srcPoints =
    HairData
        color
        (makeHairPoints srcPoints)


makeHairPoints : List Point -> List Point
makeHairPoints srcPoints =
    srcPoints
        |> List.sortBy getX
        |> List.indexedMap
            (\id srcPoint ->
                let
                    boundingRect =
                        if id == 0 || id == 4 then
                            ( ( -150, -20 ), ( 150, 120 ) )

                        else
                            ( ( -150, 20 ), ( 150, 120 ) )
                in
                scalePointToRect boundingRect srcPoint
            )


renderHair : HairData -> List (Svg msg)
renderHair hair =
    let
        hairlineTermPoints =
            ( List.Extra.getAt 0 hair.points
            , List.Extra.getAt 4 hair.points
            )
                |> extractTuple2Maybe
                |> squashMaybe ( ( 0, 0 ), ( 0, 0 ) )
                |> (\( hsStart, hsEnd ) ->
                        ( ( -100, getY hsStart ), ( 100, getY hsEnd ) )
                   )

        hairPolygonPoints =
            [ ( -100, 100 )
            , Tuple.first hairlineTermPoints
            ]
                ++ hair.points
                ++ [ Tuple.second hairlineTermPoints
                   , ( 100, 100 )
                   ]
    in
    [ polygon
        [ fillColor hair.color
        , SvgA.stroke "none"
        , points hairPolygonPoints
        ]
        []
    ]


consumeFloat : String -> Maybe ( Float, String )
consumeFloat fullSrc =
    String.toList fullSrc
        |> List.head
        |> Maybe.map hexCharToFloat
        |> Maybe.map
            (\f -> ( f, String.dropLeft 1 fullSrc ))


consumePoints : Int -> String -> Maybe ( List Point, String )
consumePoints num fullSrc =
    let
        ( srcChars, remainingStr ) =
            fullSrc
                |> String.toList
                |> List.Extra.splitAt (num * 2)
                |> Tuple.mapSecond String.fromList
    in
    if List.length srcChars /= num * 2 then
        Nothing

    else
        Just <|
            ( srcChars
                |> List.map hexCharToFloat
                |> groupToTuples
            , remainingStr
            )


groupToTuples : List a -> List ( a, a )
groupToTuples l =
    l
        |> List.Extra.groupsOf 2
        |> List.map
            (\listOfMaybe2 ->
                case ( List.Extra.getAt 0 listOfMaybe2, List.Extra.getAt 1 listOfMaybe2 ) of
                    ( Just a, Just b ) ->
                        Just ( a, b )

                    _ ->
                        Nothing
            )
        |> Maybe.Extra.values


hexCharToFloat : Char -> Float
hexCharToFloat c =
    Hex.fromString (String.fromChar c)
        |> Result.withDefault 0
        |> toFloat
        |> (\f -> f / 15.0)


scalePointToRect : ( Point, Point ) -> Point -> Point
scalePointToRect ( rectBottomLeft, rectTopRight ) =
    Tuple.mapBoth
        (scaleFloatToRange ( getX rectBottomLeft, getX rectTopRight ))
        (scaleFloatToRange ( getY rectBottomLeft, getY rectTopRight ))


scaleFloatToRange : ( Float, Float ) -> Float -> Float
scaleFloatToRange ( min, max ) f =
    ((max - min) * f) + min


pointToBoundingRect : Point -> Point -> ( Point, Point )
pointToBoundingRect ( width, height ) point =
    ( ( getX point - width, getY point - height )
    , ( getX point + width, getY point + height )
    )


getX =
    Tuple.first


getY =
    Tuple.second


negateX =
    Tuple.mapFirst negate


mirrorHorizontal : List (Svg msg) -> List (Svg msg)
mirrorHorizontal svgs =
    [ g [] svgs
    , g
        [ SvgA.transform "scale(-1,1)" ]
        svgs
    ]


flipVertical : List (Svg msg) -> List (Svg msg)
flipVertical svgs =
    [ g
        [ SvgA.transform "scale(1,-1)" ]
        svgs
    ]
