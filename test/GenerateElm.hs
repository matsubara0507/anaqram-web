{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           RIO

import           AnaQRam.API    (CRUD)
import           Servant        ((:>))
import           Servant.Elm    (defElmImports, defElmOptions,
                                 generateElmModuleWith)
import           System.Process (system)

main :: IO ()
main = do
  generateElmModuleWith
    defElmOptions
    ["AnaQRam", "Generated", "API"]
    defElmImports
    "elm-src"
    []
    (Proxy @ ("api" :> CRUD))
  mapM_ system
    [ "elm make elm-src/Main.elm --output=docs/static/main.js --optimize"
    , "elm-format --yes elm-src/AnaQRam/Generated/"
    ]
