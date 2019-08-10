module Main exposing (main)

import AnaQRam.Image as Image exposing (ImageData)
import Browser as Browser
import Html as Html exposing (..)
import Html.Attributes exposing (autoplay, height, id, style, width)
import Html.Events exposing (onClick)
import Json.Decode exposing (Error, errorToString)


main : Program Image.Config Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { config : Image.Config
    , image : Maybe ImageData
    , error : String
    }


init : Image.Config -> ( Model, Cmd Msg )
init config =
    ( Model config Nothing "", Cmd.none )


type Msg
    = OnCamera
    | CaptureImage
    | UpdateImage (Result Error (Maybe ImageData))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCamera ->
            ( model, Image.startCamera () )

        CaptureImage ->
            ( model, Image.captureImage () )

        UpdateImage (Ok image) ->
            ( { model | image = image }, Cmd.none )

        UpdateImage (Err message) ->
            ( { model | error = errorToString message }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        imageSize =
            model.image
                |> Maybe.map Image.size
                |> Maybe.withDefault 0
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
            [ button [ onClick OnCamera ] [ text "映像表示開始" ]
            , button [ onClick CaptureImage ] [ text "静止画取得" ]
            ]
        , canvas [ id model.config.ids.capture ] []
        , p [] [ text ("Image size: " ++ String.fromInt imageSize) ]
        , p [] [ text ("Error: " ++ model.error) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Image.updateImageWithDecode UpdateImage
