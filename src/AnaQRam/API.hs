{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module AnaQRam.API where

import           RIO
import qualified RIO.Map                as Map
import qualified RIO.Text               as T
import qualified RIO.Vector             as V

import           AnaQRam.Env            (Env)
import qualified AnaQRam.Env            as AnaQRam
import           Data.Extensible
import           Data.Fallible
import           Mix.Plugin.Logger      ()
import qualified Mix.Plugin.Logger.JSON as Mix
import           Servant
import           System.Random

type API
      = "api" :> "sizes" :> Get '[JSON] [Int]
   :<|> "api" :> "problem" :> QueryParam' '[Required] "size" Int :> Get '[JSON] String

api :: Proxy API
api = Proxy

server :: ServerT API (RIO Env)
server = getSizes :<|> getProblem

getSizes :: RIO Env [Int]
getSizes = do
  Mix.logDebugR "request GET: api=/sizes" nil
  problems <- AnaQRam.problems
  pure $ Map.keys problems

getProblem :: Int -> RIO Env String
getProblem wordSize = evalContT $ do
  Mix.logDebugR "request GET: api=/problem" (#size @= wordSize <: nil)
  problems <- lift (Map.lookup wordSize <$> AnaQRam.problems) !?? exit'
  idx <- liftIO $ randomRIO (0, length problems - 1)
  problem <- problems V.!? idx ??? exit'
  pure $ T.unpack problem
  where
    exit' :: ContT String (RIO Env) a
    exit' = do
      Mix.logErrorR "problem not found." (#size @= wordSize <: nil)
      exitA ""
