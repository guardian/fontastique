module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (type_, checked, style, class)
import Html.Events exposing (onCheck, onClick)
import Http
import Tuple exposing (pair, second)

import Font exposing (..)


-- MAIN

main = Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


-- MODEL

type alias Model =
    List (Selected, Font)

type alias Selected =
    Bool


init : () -> (Model, Cmd Msg)
init _ =
    ([], getFonts)


-- UPDATE

type Msg
    = GotFonts (Result Http.Error (List Font))
    | Check Int
    | Uncheck Int
    | SelectAll
    | DeselectAll

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotFonts result ->
            case result of
                Ok fonts ->
                    (List.map (pair False) fonts, Cmd.none)
                Err message ->
                    (model, Cmd.none)
        Check index ->
            (List.indexedMap (\idx font -> if idx == index then (True, second font) else font) model, Cmd.none)
        Uncheck index ->
            (List.indexedMap (\idx font -> if idx == index then (False, second font) else font) model, Cmd.none)
        SelectAll ->
            (List.map (\font -> (True, second font)) model, Cmd.none)
        DeselectAll ->
            (List.map (\font -> (False, second font)) model, Cmd.none)

getFonts : Cmd Msg
getFonts =
    Http.get
    { url = "/fonts.json"
    , expect = Http.expectJson GotFonts fontsDecoder
    }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ header [ class "main__header" ] [ h1 [ class "main__heading" ] [ text "Fontastique" ] ]
        , main_ [ class "main__content" ]
            [ fieldset [ class "main__fields" ]
                [ h2 [ style "font-family" "Guardian Headline", style "font-weight" "bold", class "main__fields-heading" ] [ text "Choose your fonts" ]
                , hr [ class "main__keyline" ] []
                , button (buttonStyles ++ [ onClick SelectAll ]) [ text "Select all" ]
                , button (buttonStyles ++ [ onClick DeselectAll, style "background-color" "#C1D8FC", style "color" "#052962" ]) [ text "Deselect all" ]
                , ul [ style "list-style" "none", style "padding" "0" ] <| List.indexedMap viewFont model
                ]
            , pre [ class "main__font-face" ] [ text <| String.join "\n" <| List.filterMap viewFontFaces model ]
            ]
        ]

buttonStyles : List (Attribute Msg)
buttonStyles =
    [ style "border-radius" "100px"
    , style "background-color" "#052962"
    , style "font-size" "1rem"
    , style "font-family" "Guardian Text Sans"
    , style "font-weight" "bold"
    , style "color" "#fff"
    , style "border" "none"
    , style "padding" "0.5rem 1rem"
    , style "cursor" "pointer"
    , class "main__fields-button"
    ]

viewFontFaces : (Selected, Font) -> Maybe String
viewFontFaces (isSelected, font) =
    if isSelected then Just (fontFace font) else Nothing

viewFont : Int -> (Selected, Font) -> Html Msg
viewFont index (selected, font) =
    li [ style "margin" "0.5rem 0" ]
        [ label
            [ style "font-family" font.family
            , style "font-weight" <| weightToString font.weight
            , style "font-style" <| fontStyleProperty font.isItalic
            ]
            [ input
                [ type_ "checkbox"
                , onCheck <| check index
                , checked selected
                , style "display" "inline-block"
                , style "vertical-align" "middle"
                , style "margin-right" "0.5rem"
                ]
                []
            , span
                [ style "display" "inline-block"
                , style "vertical-align" "middle"
                ]
                [ text <| fontToString font ]
            ]
        ]

check : Int -> Bool -> Msg
check index isChecked =
    if isChecked then Check index else Uncheck index
