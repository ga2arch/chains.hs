{-# LANGUAGE RecordWildCards, LambdaCase, TemplateHaskell #-}

module Main where 

import Prelude hiding (readFile)

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import Data.List (intersperse, transpose)
import Data.Time
--import System.Console.ANSI
import System.Directory
import System.Locale
import System.Environment
import System.IO.Strict

import qualified Data.Map as M

type Chains = M.Map String Chain

data Chain = Chain {
    chainName     :: String
,   chainStart    :: LocalTime
,   chainEnd      :: Maybe LocalTime
,   chainRunning  :: Bool
,   chainProgress :: [Bool]
} deriving (Show, Read)

process :: [String] -> StateT Chains IO ()
process [] = showChains
process (cmd:args) |  cmd == "add"  = addChain args  >> showChains
                   |  cmd == "show" = showChains
                   |  cmd == "done" = doneChain args >> showChains
                   |  cmd == "rm"   = rmChain args   >> showChains


addChain :: [String] -> StateT Chains IO ()
addChain (name:_) = do
    time <- liftIO $ utcToLocalTime <$> getCurrentTimeZone 
                                    <*> getCurrentTime
    modify $ \chains -> do 
        let chain = Chain name time Nothing True []
        M.insert name chain chains

showChains :: StateT Chains IO ()
showChains = do
    chains <- fmap M.elems get
    --liftIO $ print r
    mapM_ (liftIO . putStrLn . showChain) chains

showChain :: Chain -> String
showChain Chain{..} = 
    (concat $ intersperse "\n" temp) ++ "\n"
  where
    separator = "---" --take (length chainName) $ repeat '-'
    start = formatTime defaultTimeLocale "%F" chainStart
    streak = length $ takeWhile (== True) $ reverse chainProgress
    spaces = " " --take (length separator `div` 2) $ repeat ' '
    progress = map (\case True -> spaces ++ "V" 
                          False -> spaces ++ "X") chainProgress
    temp = [chainName, show streak, separator] ++ progress

doneChain :: [String] -> StateT Chains IO ()
doneChain (name:_) = do
    modify $ \chains -> do
        let c@Chain{..} = chains M.! name
        M.insert name (c { chainProgress = chainProgress ++ [True]}) chains

rmChain :: [String] -> StateT Chains IO ()
rmChain (name:_) = do
    modify $ \chains -> M.delete name chains 

loadChains :: IO (Maybe Chains)
loadChains = do 
    exists <- doesFileExist "chains"
    if exists
        then fmap maybeRead $ readFile "chains"
        else writeFile "chains" "" >> loadChains
  where
    maybeRead = fmap fst . listToMaybe . reads

updateChains :: Chains -> IO Chains
updateChains chains = do 
    today <- utcToLocalTime <$> getCurrentTimeZone 
                            <*> getCurrentTime
    return $ M.map (go today) chains
  where 
    go today c@Chain{..} = do
        let days = fromIntegral $ diffDays (localDay today) 
                                           (localDay chainStart)
        let missed = take (days - length chainProgress) $ repeat False
        let progress = chainProgress ++ missed
        (c { chainProgress = progress }) 

main :: IO ()
main = do
    args <- getArgs 
    putStrLn "Chains\n==="
    chains  <- loadChains >>= \case
        Just chains -> updateChains chains >>= execStateT (process args) 
        Nothing     -> execStateT (process args) M.empty
    writeFile "chains" $ show chains
