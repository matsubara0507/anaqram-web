port module AnaQRam.Image exposing (Config, ImageData, captureImage, decoder, size, startCamera, updateImage, updateImageWithDecode)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type alias ImageData =
    { data : Array Int
    , width : Int
    , height : Int
    }


decoder : Decoder ImageData
decoder =
    D.map3 ImageData
        (D.field "data" <| D.array D.int)
        (D.field "width" D.int)
        (D.field "height" D.int)


size : ImageData -> Int
size image =
    Array.length image.data


type alias Config =
    { ids : { video : String, capture : String }
    , size : { width : Int, height : Int }
    }


port startCamera : () -> Cmd msg


port captureImage : () -> Cmd msg


port updateImage : (E.Value -> msg) -> Sub msg


updateImageWithDecode : (Result D.Error (Maybe ImageData) -> msg) -> Sub msg
updateImageWithDecode msg =
    updateImage (msg << D.decodeValue (D.nullable decoder))
