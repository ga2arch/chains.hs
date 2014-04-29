{-# LANGUAGE RecordWildCards, LambdaCase #-}

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
process (cmd:args) |  cmd == "add"  = addChain args  >> showChains
                   |  cmd == "show" = showChains
                   |  cmd == "done" = doneChain args >> showChains
                   |  cmd == "rm"   = rmChain args   >> showChains

addChain :: [String] -> StateT Chains IO ()
addChain (name:_) = do
    chains <- get
    time <- liftIO $ utcToLocalTime <$> getCurrentTimeZone 
                                    <*> getCurrentTime
    let chain = Chain name time Nothing True []
    put $ M.insert name chain chains

showChains :: StateT Chains IO ()
showChains = do
    chains <- fmap M.elems get
    let r = transpose $ map showChain $ zip (iterate (+1) 0) chains
    liftIO $ print r
    mapM_ (liftIO . putStrLn . concat) r

showChain :: (Int, Chain) -> [String]
showChain (i, Chain{..}) = 
    [chainName ++ tab, show streak ++ tab, "---\t", progress]
  where
    tab = "\t"
    tabs = take i $ repeat '\t'
    start = formatTime defaultTimeLocale "%F" chainStart
    streak = length $ takeWhile (== True) $ reverse chainProgress
    progress = concat $ 
               intersperse "\n" $ 
               map (\x -> if x 
                            then tabs ++ " V" 
                            else tabs ++ " X") chainProgress

doneChain :: [String] -> StateT Chains IO ()
doneChain (name:_) = do
    chains <- get
    let (Chain n s e r p) = chains M.! name
    let nchain = Chain n s e r $ p ++ [True]
    put $ M.insert name nchain chains

rmChain :: [String] -> StateT Chains IO ()
rmChain (name:_) = do
    chains <- get
    put $ M.delete name chains 

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
    today <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
    return $ M.map (go today) chains
  where 
    go today c@Chain{..} = do
        let days = fromIntegral $ diffDays (localDay today) 
                                           (localDay chainStart)
        let p = chainProgress ++ take (days - length chainProgress) 
                                      (repeat False)
        (c { chainProgress = p}) 

main :: IO ()
main = do
    args <- getArgs 
    chains  <- loadChains >>= \case
        Just chains -> updateChains chains >>= execStateT (process args) 
        Nothing     -> execStateT (process args) M.empty
    writeFile "chains" $ show chains
