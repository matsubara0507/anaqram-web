module Main exposing (main)

import AnaQRam.QRCode as QRCode exposing (QRCode)
import Browser as Browser
import Html as Html exposing (..)
import Html.Attributes exposing (attribute, autoplay, class, height, hidden, id, style, type_, width)
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

        UpdateQRCode (Ok Nothing) ->
            ( { model | error = "QR code is not found." }, Cmd.none )

        UpdateQRCode (Ok qrcode) ->
            ( { model | qrcode = qrcode, error = "" }, Cmd.none )

        UpdateQRCode (Err message) ->
            ( { model | error = errorToString message }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ video
            [ class "my-2"
            , id model.config.ids.video
            , style "background-color" "#000"
            , autoplay True
            , attribute "playsinline" ""
            , width model.config.size.width
            , height model.config.size.height
            ]
            []
        , p []
            [ button
                [ class "btn mx-1", type_ "button", onClick OnCamera ]
                [ text "On Camera" ]
            , button
                [ class "btn mx-1", type_ "button", onClick CaptureImage ]
                [ text "Decode QR" ]
            ]
        , canvas [ id model.config.ids.capture, hidden True ] []
        , viewResult model
        ]


viewResult : Model -> Html Msg
viewResult model =
    let
        code =
            model.qrcode
                |> Maybe.map .data
                |> Maybe.withDefault ""

        attr =
            class "mx-5 mb-2 flash text-left"
    in
    if String.isEmpty model.error then
        div [ attr, class "flash-success" ] [ text ("QR Code: " ++ code) ]

    else
        div [ attr, class "flash-error" ] [ text ("Error: " ++ model.error) ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    QRCode.updateQRCodeWithDecode UpdateQRCode
