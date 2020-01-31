module Font exposing
    ( Font
    , fontFace
    , fontsDecoder
    , fontToString
    , weightToString
    , fontStyleProperty
    )

import Json.Decode as D exposing (Decoder)


-- FONTS

type Weight
    = Thin
    | ExtraLight
    | Light
    | Regular
    | Medium
    | SemiBold
    | Bold
    | ExtraBold
    | Black

weightToString : Weight -> String
weightToString weight =
    case weight of
        Thin ->
            "100"
        ExtraLight ->
            "200"
        Light ->
            "300"
        Regular ->
            "400"
        Medium ->
            "500"
        SemiBold ->
            "600"
        Bold ->
            "700"
        ExtraBold ->
            "800"
        Black ->
            "900"

type alias Font =
    { family : String
    , weight : Weight
    , woff : String
    , woff2 : String
    , ttf : String
    , isItalic : Bool
    }

fontToString : Font -> String
fontToString { family, weight, isItalic } =
    family
        ++ " ("
        ++ weightToString weight
        ++ ", "
        ++ fontStyleProperty isItalic
        ++ ")"

type Format
    = Woff2
    | Woff
    | TTF

formatToString : Format -> String
formatToString format =
    case format of
        Woff2 ->
            "woff2"
        Woff ->
            "woff"
        TTF ->
            "truetype"

type alias FontFace =
    String

fontFace : Font -> FontFace
fontFace ({ family, weight, isItalic } as font) =
    let
        rules =
            String.join ";\n  "
                [ fontFamily family 
                , fontWeight weight
                , fontSrc font
                , fontStyle isItalic
                ]
    in
        "@font-face {\n  " ++ rules ++ ";\n}\n"

fontFamily : String -> String
fontFamily family =
    "font-family: \"" ++ family ++ "\""

fontWeight : Weight -> String
fontWeight weight =
    "font-weight: " ++ weightToString weight

fontSrc : Font -> String
fontSrc { woff2, woff, ttf } =
    "src: "
        ++ fontUrl woff2 Woff2
        ++ ",\n    "
        ++ fontUrl woff Woff
        ++ ",\n    "
        ++ fontUrl ttf TTF

fontUrl : String -> Format -> String
fontUrl url format =
    "url(\"" ++ url ++ "\") " ++ fontFormat format

fontFormat : Format -> String
fontFormat format =
    "format(\"" ++ formatToString format ++ "\")"

fontStyle : Bool -> String
fontStyle isItalic =
    "font-style: " ++ fontStyleProperty isItalic

fontStyleProperty : Bool -> String
fontStyleProperty isItalic =
    if isItalic then "italic" else "normal"


-- JSON

fontsDecoder : Decoder (List Font)
fontsDecoder = 
    D.field "fonts" (D.list fontDecoder)

fontDecoder : Decoder Font
fontDecoder =
    D.map6 Font
        (D.field "family" D.string)
        (D.field "weight" weightDecoder)
        (D.field "woff" D.string)
        (D.field "woff2" D.string)
        (D.field "ttf" D.string)
        (D.field "isItalic" D.bool)

weightDecoder : Decoder Weight
weightDecoder =
    D.string
        |> D.andThen decoderWeightString

decoderWeightString : String -> Decoder Weight
decoderWeightString weightString =
    case weightString of
        "Thin" ->
            D.succeed Thin
        "ExtraLight" ->
            D.succeed ExtraLight
        "Light" ->
            D.succeed Light
        "Regular" ->
            D.succeed Regular
        "Medium" ->
            D.succeed Medium
        "Semibold" ->
            D.succeed SemiBold
        "Bold" ->
            D.succeed Bold
        "ExtraBold" ->
            D.succeed ExtraBold
        "Black" ->
            D.succeed Black
        _ ->
            D.fail ("Invalid weight: " ++ weightString)
