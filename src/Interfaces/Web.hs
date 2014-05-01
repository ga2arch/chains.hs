{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Interfaces.Web where

import Data.Aeson hiding (json)
import Web.Scotty

import Types
import Utils

import qualified Data.Text.Lazy as TL

web :: IO ()
web = do
    chains <- loadChains >>= updateChains

    scotty 3000 $ do
        get "/" $ do
            json chains
