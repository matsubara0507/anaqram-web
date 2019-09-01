module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import QRCode
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), (<?>))
import Url.Parser.Query as UrlQuery


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


type alias Model =
    { key : Nav.Key
    , size : Int
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    update (OnUrlChange url) (Model key 0)


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest (Browser.Internal url) ->
            ( model, Cmd.none )

        OnUrlRequest (Browser.External link) ->
            ( model, Nav.load link )

        OnUrlChange url ->
            ( updateModelWithUrl url model, Cmd.none )


updateModelWithUrl : Url -> Model -> Model
updateModelWithUrl url model =
    case Url.parse parser { url | path = "" } of
        Just (Just n) ->
            { model | size = n }

        _ ->
            model


parser : Url.Parser (Maybe Int -> a) a
parser =
    Url.top <?> UrlQuery.int "size"


view : Model -> Browser.Document Msg
view model =
    { title = "QR Codes: size " ++ String.fromInt model.size
    , body = [ viewBody model ]
    }


viewBody : Model -> Html msg
viewBody model =
    let
        linkPrimer =
            Html.node "link"
                [ Attr.rel "stylesheet"
                , Attr.type_ "text/css"
                , Attr.href "https://cdnjs.cloudflare.com/ajax/libs/Primer/11.0.0/build.css"
                ]
                []
    in
    (model.size - 1)
        |> List.range 0
        |> List.map String.fromInt
        |> List.map viewQRCode
        |> Html.ul [ Attr.class "list-style-none" ]
        |> (\html -> Html.div [] [ linkPrimer, html ])


viewQRCode : String -> Html msg
viewQRCode message =
    QRCode.encode message
        |> Result.map QRCode.toSvg
        |> Result.withDefault (Html.text "Error while encoding to QRCode.")
        |> List.singleton
        |> Html.div
            [ Attr.class "d-inline-block"
            , Attr.style "transform" "scale(2)"
            , Attr.style "padding" "10%"
            ]
