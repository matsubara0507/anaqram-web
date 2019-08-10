module Main exposing (main)

import AnaQRam.Image as Image
import Browser as Browser
import Html as Html exposing (..)
import Html.Attributes exposing (autoplay, height, id, style, width)
import Html.Events exposing (onClick)


main : Program Image.Config Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { config : Image.Config
    }


init : Image.Config -> ( Model, Cmd Msg )
init config =
    ( Model config, Cmd.none )


type Msg
    = OnCamera


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCamera ->
            ( model, Image.startCamera () )


view : Model -> Html Msg
view model =
    div []
        [ video
            [ id model.config.video
            , style "background-color" "#000"
            , autoplay True
            , width model.config.size.width
            , height model.config.size.height
            ]
            []
        , p [] [ button [ onClick OnCamera ] [ text "映像表示開始" ] ]
        ]
