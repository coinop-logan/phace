module SvgHelpers exposing (..)

import Svg exposing (..)
import Svg.Attributes as SvgA
import Tuple2
import TupleHelpers exposing (..)
import Phace.Types exposing (..)


points : List Point -> Attribute msg
points =
    SvgA.points << toSvgPointsStr


fillColor : Color -> Attribute msg
fillColor =
    SvgA.fill << colorToStr


strokeColor : Color -> Attribute msg
strokeColor =
    SvgA.stroke << colorToStr


colorToStr color =
    color
        |> tuple3ToList
        |> (\l -> '#' :: l)
        |> String.fromList


toSvgPointsStr : List Point -> String
toSvgPointsStr =
    List.map (Tuple.mapBoth String.fromFloat String.fromFloat)
        >> List.map (\( xs, ys ) -> xs ++ "," ++ ys)
        >> String.join " "


bezierPath : Point -> List ( Point, Point, Point ) -> Attribute msg
bezierPath start segments =
    SvgA.d <|
        moveCommand start
            ++ (segments
                    |> List.map
                        (\( b1, b2, p ) ->
                            cubicCurveCommand b1 b2 p
                        )
                    |> String.join " "
               )


moveCommand : ( Float, Float ) -> String
moveCommand point =
    "M"
        ++ pointToPathCommandStr point
        ++ " "


cubicCurveCommand : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> String
cubicCurveCommand bc1 bc2 end =
    "C"
        ++ pointListToPathCommandStr
            [ bc1
            , bc2
            , end
            ]


pointToPathCommandStr : ( Float, Float ) -> String
pointToPathCommandStr point =
    point
        |> Tuple.mapBoth String.fromFloat String.fromFloat
        |> Tuple2.uncurry
            (\x y -> String.join " " [ x, y ])


pointListToPathCommandStr : List ( Float, Float ) -> String
pointListToPathCommandStr =
    List.map pointToPathCommandStr
        >> String.join ", "
