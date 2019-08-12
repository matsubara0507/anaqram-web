module Main exposing (main)

import AnaQRam.Puzzle as Puzzle exposing (Piece, Puzzle)
import AnaQRam.QRCode as QRCode exposing (QRCode)
import Browser as Browser
import Dict
import Html as Html exposing (..)
import Html.Attributes exposing (attribute, autoplay, class, height, hidden, id, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
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
    , start : Bool
    }


init : QRCode.Config -> ( Model, Cmd Msg )
init config =
    let
        answer =
            "あなくらむ！"
    in
    ( Model config Nothing "" answer Puzzle.empty Nothing False
    , Cmd.none
    )


type Msg
    = StartGame
    | ChoiceAnswer String
    | ShufflePuzzle Puzzle
    | CaptureImage
    | UpdateQRCode (Result Error (Maybe QRCode))
    | ChoiceWordSize Int
    | ClickPiece Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.start, msg ) of
        ( False, StartGame ) ->
            ( { model | start = True }
            , Cmd.batch
                [ QRCode.startCamera ()
                , Puzzle.problem ChoiceAnswer (Puzzle.size model.puzzle)
                ]
            )

        ( True, ChoiceAnswer answer ) ->
            ( { model | answer = answer }
            , Puzzle.shuffle ShufflePuzzle (Puzzle.init answer)
            )

        ( True, ShufflePuzzle puzzle ) ->
            ( { model | puzzle = puzzle }, Cmd.none )

        ( True, CaptureImage ) ->
            ( model, QRCode.captureImage () )

        ( True, UpdateQRCode (Ok Nothing) ) ->
            ( { model | error = "QR code is not found." }, Cmd.none )

        ( True, UpdateQRCode (Ok (Just qrcode)) ) ->
            updatePuzzle qrcode model

        ( _, UpdateQRCode (Err message) ) ->
            ( { model | error = errorToString message }, Cmd.none )

        ( _, ChoiceWordSize 0 ) ->
            ( model, Cmd.none )

        ( False, ChoiceWordSize wordSize ) ->
            ( { model | puzzle = Puzzle.dummy wordSize }, Cmd.none )

        ( True, ClickPiece idx ) ->
            updatePiece idx model

        _ ->
            ( model, Cmd.none )


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
            in
            ( { model | click = Nothing, puzzle = updated }, Cmd.none )


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
            [ viewSelectMenu model
            , button
                [ class "btn mx-1", type_ "button", onClick StartGame ]
                [ text "Game Start" ]
            , button
                [ class "btn mx-1", type_ "button", onClick CaptureImage ]
                [ text "Decode QR" ]
            ]
        , canvas [ id model.config.ids.capture, hidden True ] []
        , viewPuzzle model
        , viewResult model
        ]


viewSelectMenu : Model -> Html Msg
viewSelectMenu model =
    let
        viewItem v =
            option [ value (String.fromInt v) ] [ text (String.fromInt v) ]
    in
    select
        [ class "form-select mx-1"
        , onInput (ChoiceWordSize << Maybe.withDefault 0 << String.toInt)
        ]
        (option [] [ text "Choose Word Size" ] :: List.map viewItem (Dict.keys Puzzle.problems))


viewResult : Model -> Html Msg
viewResult model =
    let
        scaned =
            model.qrcode
                |> Maybe.map .data
                |> Maybe.andThen String.toInt
                |> Maybe.andThen (\idx -> Puzzle.getPiece idx model.puzzle)

        attr =
            class "mx-5 mb-2 text-left"
    in
    case ( Puzzle.success model.answer model.puzzle, model.error, scaned ) of
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
