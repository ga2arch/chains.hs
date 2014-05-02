{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Utils where

import Control.Applicative
import Data.Aeson
import Data.Char
import Data.Time
import System.Directory

import qualified Data.ByteString as B
import qualified Data.Map as M

import Types

lower :: String -> String
lower = map toLower

loadChains :: IO (Maybe Chains)
loadChains = do 
    exists <- doesFileExist "chains"
    if exists
        then fmap decodeStrict $ B.readFile "chains"
        else writeFile "chains" "" >> loadChains

updateChains :: Maybe Chains -> IO Chains
updateChains Nothing       = return $ M.empty
updateChains (Just chains) = do 
    timezone <- getCurrentTimeZone
    today <- utcToLocalTime <$> getCurrentTimeZone 
                            <*> getCurrentTime
    return $ M.map (go today timezone) chains
  where 
    go today timezone c@Chain{..} = do
        let start = utcToLocalTime timezone chainStart
        let days = fromIntegral $ diffDays (localDay today) 
                                           (localDay start)
        let missed = take (days - length chainProgress) $ repeat False
        let progress = chainProgress ++ missed
        c { chainProgress = progress }