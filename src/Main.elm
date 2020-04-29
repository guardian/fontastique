port module Main exposing (..)

import Browser
import File.Download as Download
import Font exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck, onClick)
import Http
import Tuple exposing (pair, second)



-- PORTS


port sendMessage : String -> Cmd msg



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    List ( Selected, Font )


type alias Selected =
    Bool


init : () -> ( Model, Cmd Msg )
init _ =
    ( [], getFonts )


pickSelected : ( Selected, Font ) -> Maybe Font
pickSelected ( isSelected, font ) =
    if isSelected then
        Just font

    else
        Nothing


selectedFonts : Model -> List Font
selectedFonts =
    List.filterMap pickSelected


cssString : Model -> String
cssString model =
    selectedFonts model
        |> web
        |> String.join "\n"



-- UPDATE


type Msg
    = GotFonts (Result Http.Error (List Font))
    | Check Int
    | Uncheck Int
    | SelectAll
    | DeselectAll
    | DownloadWeb
    | CopyWeb


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFonts result ->
            case result of
                Ok fonts ->
                    ( List.map (pair False) fonts, Cmd.none )

                Err message ->
                    ( model, Cmd.none )

        Check index ->
            ( List.indexedMap (setSelected index True) model, Cmd.none )

        Uncheck index ->
            ( List.indexedMap (setSelected index False) model, Cmd.none )

        SelectAll ->
            ( List.map (\font -> ( True, second font )) model, Cmd.none )

        DeselectAll ->
            ( List.map (\font -> ( False, second font )) model, Cmd.none )

        DownloadWeb ->
            ( model, Download.string "fonts.css" "text/css" (cssString model) )

        CopyWeb ->
            ( model, sendMessage (cssString model) )


getFonts : Cmd Msg
getFonts =
    Http.get
        { url = "/fonts.json"
        , expect = Http.expectJson GotFonts fontsDecoder
        }


setSelected : Int -> Selected -> Int -> ( Selected, Font ) -> ( Selected, Font )
setSelected selectedIndex selected index font =
    if selectedIndex == index then
        ( selected, second font )

    else
        font



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ header [ class "main__header" ]
            [ h1 [ class "main__heading" ] [ text "Fontastique" ]
            ]
        , main_ [ class "main__content" ]
            [ fieldset [ class "fields" ]
                [ h2 [ class "fields__heading" ] [ text "Choose your fonts" ]
                , hr [ class "fields__keyline" ] []
                , button
                    [ class "fields__button"
                    , onClick SelectAll
                    ]
                    [ text "Select all" ]
                , button
                    [ class "fields__button fields__button--secondary"
                    , onClick DeselectAll
                    ]
                    [ text "Deselect all" ]
                , ul [ class "fields__font-list" ] <|
                    List.indexedMap viewFont model
                ]
            , viewFontFaces model
            ]
        ]


viewFontFaces : Model -> Html Msg
viewFontFaces model =
    let
        selected =
            selectedFonts model

        webFonts =
            web selected

        androidFonts =
            android selected
    in
    section [ class "font-faces" ]
        [ h2 [ class "font-faces__heading" ] [ text "Get your fonts" ]
        , hr [ class "font-faces__keyline" ] []
        , section []
            [ h3 [ class "font-faces__heading" ] [ text "Web" ]
            , viewFontSource webFonts
            , button
                [ class "font-faces__copy", onClick CopyWeb ]
                [ text "Copy CSS" ]
            , button
                [ class "font-faces__download", onClick DownloadWeb ]
                [ text "Download CSS File" ]
            ]
        , section []
            [ h3 [ class "font-faces__heading" ] [ text "Android" ]
            , viewFontSource androidFonts
            ]
        ]


viewFontSource : List FontFace -> Html Msg
viewFontSource fontFaces =
    details []
        [ summary [ class "font-faces__source" ] [ text "View Source" ]
        , pre
            [ class "font-faces__code" ]
            [ fontFaces
                |> String.join "\n"
                |> text
            ]
        ]


viewFont : Int -> ( Selected, Font ) -> Html Msg
viewFont index ( selected, font ) =
    li [ class "fields__font" ]
        [ label
            [ style "font-family" font.family
            , style "font-weight" <| weightToString font.weight
            , style "font-style" <| fontStyleProperty font.isItalic
            ]
            [ input
                [ type_ "checkbox"
                , onCheck <| check index
                , checked selected
                , class "fields__checkbox"
                ]
                []
            , span [ class "fields__font-name" ] [ text <| fontToString font ]
            ]
        ]


check : Int -> Bool -> Msg
check index isChecked =
    if isChecked then
        Check index

    else
        Uncheck index
