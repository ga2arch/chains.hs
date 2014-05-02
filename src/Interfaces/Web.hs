{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Interfaces.Web where

import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Web.Scotty

import Types
import Utils

import qualified Data.ByteString.Lazy as BL

root = "src/Interfaces/Web/"

web :: IO ()
web = do
    loadChains >>= 
        updateChains >>= 
            BL.writeFile "chains.db" . encode

    scotty 3000 $ do
        get "/" $ do
            file $ root ++ "index.html"

        post "/save" $ do
            chains <- jsonData
            liftIO $ BL.writeFile "chains.db" $ encode (chains :: Chains)

        get "/chains.json" $ do
            file "chains.db"

        get (regex "^/static/(.*)$") $ do
            path <- param "1"
            file $ root ++ path
