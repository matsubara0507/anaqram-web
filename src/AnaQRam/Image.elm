port module AnaQRam.Image exposing (Config, startCamera)


type alias Config =
    { video : String
    , size : { width : Int, height : Int }
    }


port startCamera : () -> Cmd msg
