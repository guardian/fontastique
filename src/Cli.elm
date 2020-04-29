port module Cli exposing (..)

import Font exposing (Font, fontFace, fontsDecoder)
import Json.Decode as D
import Json.Encode as E
import Platform



-- MAIN


main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    Int


init : D.Value -> ( Model, Cmd Msg )
init jsonFonts =
    let
        face =
            case D.decodeValue fontsDecoder jsonFonts of
                Ok font ->
                    String.join "\n" (List.map fontFace font)

                Err message ->
                    D.errorToString message
    in
    ( 0, outputFontFace <| E.string face )



-- UPDATE


type Msg
    = NoOp


update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions _ =
    Sub.none



-- PORTS


port outputFontFace : E.Value -> Cmd msg
