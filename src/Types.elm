module Types exposing (..)


type alias Point =
    ( Float, Float )


type alias Color =
    ( Char, Char, Char )


squashMaybe : a -> Maybe a -> a
squashMaybe default maybe =
    case maybe of
        Nothing ->
            let
                _ =
                    Debug.log "Uh oh! Squashing a maybe!" maybe
            in
            default

        Just a ->
            a
