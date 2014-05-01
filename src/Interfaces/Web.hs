{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Interfaces.Web where

import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Web.Scotty

import Types
import Utils

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL

root = "/Users/ga2arch/Progetti/haskell/hs-chainhabit/src/Interfaces/Web/"

web :: IO ()
web = do
    chains <- loadChains >>= updateChains

    scotty 3000 $ do
        get "/" $ do
            file $ root ++ "index.html"

        post "/save" $ do
            chains <- jsonData
            liftIO $ BL.writeFile "chains" $ encode (chains :: Chains)

        get "/chains.json" $ do
            file "chains"

        get (regex "^/static/(.*)$") $ do
            path <- param "1"
            file $ root ++ path
