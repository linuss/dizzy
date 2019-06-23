module Main exposing (view)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (get)
import Parser as Parser exposing ((|.), (|=), end, int, keyword, run, spaces, symbol, variable)
import Set



-- Main


main : Program Url Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Url =
    String


type alias DeepZoomXML =
    { tileSize : Int
    , overlap : Int
    , format : String
    , width : Int
    , height : Int
    }


type alias Model =
    { url : String
    , zoomLevel : Int
    , xml : Maybe DeepZoomXML
    , xmlParseErrors : List Parser.DeadEnd
    , httpError : Maybe Http.Error
    }


init : Url -> ( Model, Cmd Msg )
init url =
    ( { url = url
      , zoomLevel = 0
      , xml = Nothing
      , xmlParseErrors = []
      , httpError = Nothing
      }
    , fetchXML url
    )



-- Update


type Msg
    = GotXML (Result Http.Error String)
    | ZoomIn
    | ZoomOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotXML (Ok xml) ->
            case run parseXML xml of
                Err errors ->
                    ( { model | xmlParseErrors = errors }, Cmd.none )

                Ok deepzoomxml ->
                    ( { model | xml = Just deepzoomxml }, Cmd.none )

        GotXML (Err err) ->
            ( { model | httpError = Just err }, Cmd.none )

        ZoomIn ->
            ( { model | zoomLevel = model.zoomLevel + 1 }, Cmd.none )

        ZoomOut ->
            ( { model | zoomLevel = Basics.max 0 (model.zoomLevel - 1) }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.xml of
        Just xml ->
            div []
                [ p [] [ text <| "tile size: " ++ String.fromInt xml.tileSize ]
                , p [] [ text <| "width: " ++ String.fromInt xml.width ]
                , p [] [ text <| "height: " ++ String.fromInt xml.height ]
                , p [] [ text <| "zoomlevel: " ++ String.fromInt model.zoomLevel ]
                , p [] [ text <| "rows: " ++ String.fromInt (1 + Basics.min model.zoomLevel (xml.height // xml.tileSize)) ]
                , p [] [ text <| "columns: " ++ String.fromInt (1 + Basics.min model.zoomLevel (xml.width // xml.tileSize)) ]
                , viewDeepZoom model xml
                ]

        Nothing ->
            text ""


viewDeepZoom : Model -> DeepZoomXML -> Html Msg
viewDeepZoom model xml =
    let
        rows =
            Basics.min model.zoomLevel (xml.height // xml.tileSize)

        cols =
            Basics.min model.zoomLevel (xml.width // xml.tileSize)
    in
    div []
        [ div
            [ style "display" "grid"
            , style "max-width" (String.fromInt xml.width ++ "px")
            ]
            (List.concatMap
                (\row ->
                    List.map
                        (\col -> viewTile model xml row col)
                        (List.range 0 cols)
                )
                (List.range 0 rows)
            )
        , button [ onClick ZoomIn ] [ text "zoom in" ]
        , button [ onClick ZoomOut ] [ text "zoom out" ]
        ]


dropLast : List a -> List a
dropLast xs =
    let
        len =
            List.length xs
    in
    List.take (len - 1) xs


viewTile : Model -> DeepZoomXML -> Int -> Int -> Html msg
viewTile model xml row col =
    let
        minZoom =
            round (logBase 2 (toFloat xml.tileSize))

        source =
            String.split "." model.url
                |> dropLast
                |> String.join "."
                |> (\s ->
                        s
                            ++ "_files/"
                            ++ String.fromInt (model.zoomLevel + minZoom)
                            ++ "/"
                            ++ String.fromInt col
                            ++ "_"
                            ++ String.fromInt row
                            ++ "."
                            ++ xml.format
                   )
    in
    img
        [ src source
        , style "grid-column" (String.fromInt (1 + col))
        , style "grid-row" (String.fromInt (1 + row))
        ]
        []



-- Helpers


fetchXML : String -> Cmd Msg
fetchXML url =
    Http.get
        { url = url
        , expect = Http.expectString GotXML
        }


parseQuote : Parser.Parser ()
parseQuote =
    symbol "\""


parseKeyValue : String -> Parser.Parser a -> Parser.Parser a
parseKeyValue s p =
    Parser.succeed identity
        |. keyword s
        |. symbol "="
        |. parseQuote
        |= p
        |. parseQuote


parseString : Parser.Parser String
parseString =
    variable
        { start = Char.isAlphaNum
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }


parseXMLHeader : Parser.Parser ()
parseXMLHeader =
    keyword "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"


parseNameSpace : Parser.Parser ()
parseNameSpace =
    keyword "xmlns=\"http://schemas.microsoft.com/deepzoom/2008\""


parseXML : Parser.Parser DeepZoomXML
parseXML =
    Parser.succeed DeepZoomXML
        |. parseXMLHeader
        |. spaces
        |. symbol "<"
        |. keyword "Image"
        |. spaces
        |. parseNameSpace
        |. spaces
        |= parseKeyValue "TileSize" int
        |. spaces
        |= parseKeyValue "Overlap" int
        |. spaces
        |= parseKeyValue "Format" parseString
        |. symbol ">"
        |. spaces
        |. symbol "<"
        |. keyword "Size"
        |. spaces
        |= parseKeyValue "Width" int
        |. spaces
        |= parseKeyValue "Height" int
