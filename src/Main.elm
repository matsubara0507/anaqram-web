module Main exposing (main)

import AnaQRam.QRCode as QRCode exposing (QRCode)
import Browser as Browser
import Html as Html exposing (..)
import Html.Attributes exposing (autoplay, height, id, style, width)
import Html.Events exposing (onClick)
import Json.Decode exposing (Error, errorToString)


main : Program QRCode.Config Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { config : QRCode.Config
    , qrcode : Maybe QRCode
    , error : String
    }


init : QRCode.Config -> ( Model, Cmd Msg )
init config =
    ( Model config Nothing "", Cmd.none )


type Msg
    = OnCamera
    | CaptureImage
    | UpdateQRCode (Result Error (Maybe QRCode))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCamera ->
            ( model, QRCode.startCamera () )

        CaptureImage ->
            ( model, QRCode.captureImage () )

        UpdateQRCode (Ok qrcode) ->
            ( { model | qrcode = qrcode }, Cmd.none )

        UpdateQRCode (Err message) ->
            ( { model | error = errorToString message }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        code =
            model.qrcode
                |> Maybe.map .data
                |> Maybe.withDefault ""
    in
    div []
        [ video
            [ id model.config.ids.video
            , style "background-color" "#000"
            , autoplay True
            , width model.config.size.width
            , height model.config.size.height
            ]
            []
        , p []
            [ button [ onClick OnCamera ] [ text "On Camera" ]
            , button [ onClick CaptureImage ] [ text "Capture Image" ]
            ]
        , canvas [ id model.config.ids.capture ] []
        , p [] [ text ("QR Code: " ++ code) ]
        , p [] [ text ("Error: " ++ model.error) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    QRCode.updateQRCodeWithDecode UpdateQRCode
