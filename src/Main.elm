module Main exposing (main)

import AnaQRam.Puzzle as Puzzle exposing (Piece, Puzzle)
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
    , answer : String
    , puzzle : Puzzle
    , click : Maybe Int
    , clear : Bool
    }


init : QRCode.Config -> ( Model, Cmd Msg )
init config =
    let
        answer =
            "あなくらむ！"
    in
    ( Model config Nothing "" answer Puzzle.empty Nothing False
    , Puzzle.shuffle ShufflePuzzle (Puzzle.init answer)
    )


type Msg
    = OnCamera
    | CaptureImage
    | UpdateQRCode (Result Error (Maybe QRCode))
    | ShufflePuzzle Puzzle
    | ClickPiece Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCamera ->
            ( model, QRCode.startCamera () )

        CaptureImage ->
            ( model, QRCode.captureImage () )

        UpdateQRCode (Ok Nothing) ->
            ( { model | error = "QR code is not found." }, Cmd.none )

        UpdateQRCode (Ok (Just qrcode)) ->
            updatePuzzle qrcode model

        UpdateQRCode (Err message) ->
            ( { model | error = errorToString message }, Cmd.none )

        ShufflePuzzle puzzle ->
            ( { model | puzzle = puzzle }, Cmd.none )

        ClickPiece idx ->
            updatePiece idx model


updatePuzzle : QRCode -> Model -> ( Model, Cmd Msg )
updatePuzzle qrcode model =
    case String.toInt qrcode.data of
        Nothing ->
            ( { model | qrcode = Just qrcode, error = "" }, Cmd.none )

        Just pIdx ->
            let
                updated =
                    Puzzle.display pIdx model.puzzle
            in
            ( { model | qrcode = Just qrcode, error = "", puzzle = updated }, Cmd.none )


updatePiece : Int -> Model -> ( Model, Cmd Msg )
updatePiece idx model =
    case model.click of
        Nothing ->
            ( { model | click = Just idx }, Cmd.none )

        Just oldIdx ->
            let
                updated =
                    Puzzle.swapPiece idx oldIdx model.puzzle

                clear =
                    Puzzle.success model.answer updated
            in
            ( { model | click = Nothing, puzzle = updated, clear = clear }, Cmd.none )


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
        , viewPuzzle model
        , viewResult model
        ]


viewResult : Model -> Html Msg
viewResult model =
    let
        piece_ =
            model.qrcode
                |> Maybe.map .data
                |> Maybe.andThen String.toInt
                |> Maybe.andThen (\idx -> Puzzle.getPiece idx model.puzzle)

        attr =
            class "mx-5 mb-2 text-left"
    in
    case ( model.clear, model.error, piece_ ) of
        ( True, _, _ ) ->
            div [ attr, class "flash" ] [ text "Success!!" ]

        ( _, "", Just piece ) ->
            div [ attr, class "flash flash-success" ] [ text ("Found Piece: " ++ String.fromChar piece.char) ]

        ( _, "", Nothing ) ->
            div [] [ text "" ]

        _ ->
            div [ attr, class "flash flash-error" ] [ text ("Error: " ++ model.error) ]


viewPuzzle : Model -> Html Msg
viewPuzzle model =
    div [ class "mb-2" ] (Puzzle.map (viewPiece model) model.puzzle)


viewPiece : Model -> Int -> Piece -> Html Msg
viewPiece model viewIdx piece =
    let
        clicked =
            if Just viewIdx == model.click then
                class "btn btn-danger"

            else
                class "btn"
    in
    button [ class "mx-1", type_ "button", clicked, onClick (ClickPiece viewIdx) ]
        [ text (Puzzle.pieceToString piece) ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    QRCode.updateQRCodeWithDecode UpdateQRCode
