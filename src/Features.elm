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
import Types exposing (..)


type Feature
    = FaceColor Color
    | Eye EyeData
    | Mouth MouthData



-- | Hair HairData
-- | Cheek CheekData
-- | Shirt ShirtData


generatePhaceHalfFromString : String -> Maybe (List (Svg ms))
generatePhaceHalfFromString fullSrc =
    generateFeaturesFromString fullSrc
        |> Maybe.map (List.map renderFeature)


generateFeaturesFromString : String -> Maybe (List Feature)
generateFeaturesFromString fullSrc =
    let
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
    List.range 0 2
        |> List.foldl helper (Just ( fullSrc, [] ))
        |> Maybe.map Tuple.second


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

        _ ->
            Nothing


renderFeature : Feature -> Svg msg
renderFeature feature =
    case feature of
        FaceColor color ->
            renderFaceColor color

        Eye eyeData ->
            renderEye eyeData

        Mouth mouthData ->
            renderMouth mouthData


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


renderFaceColor : Color -> Svg msg
renderFaceColor color =
    rect
        [ fillColor color
        , SvgA.stroke "none"
        , SvgA.width "200"
        , SvgA.height "200"
        , SvgA.y "-100"
        ]
        []


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
                        ( ( 10, 10 ), ( 60, 50 ) )
                    )
    in
    EyeData
        color
        points


renderEye : EyeData -> Svg msg
renderEye eye =
    polygon
        [ fillColor eye.color
        , strokeColor eye.color
        , SvgA.strokeWidth "6"
        , SvgA.strokeLinejoin "round"
        , points eye.points
        ]
        []


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
            startPointSrc |> scalePointToRect ( ( 10, -10 ), ( 50, -50 ) )

        bcPoints =
            ( bcPoint1Src, bcPoint2Src )
                |> mapTuple2
                    (scalePointToRect
                        (pointToBoundingRect ( 20, 20 ) startPoint)
                    )
    in
    MouthData
        color
        startPoint
        (Tuple.first bcPoints)
        (Tuple.second bcPoints)


renderMouth : MouthData -> Svg msg
renderMouth mouth =
    path
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
            , (negateX mouth.bcPoint2
              , mouth.bcPoint2
              , mouth.startPoint
              )
            ]
        ]
        []


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
