{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Main where 

import Prelude hiding (readFile)

import Control.Monad.State
import Data.Maybe
import Data.List (intersperse, transpose)
import System.Environment
import System.IO.Strict

import qualified Data.Map as M

type Chains = M.Map String Chain

data Chain = Chain {
    chainName  :: String
,   chainStart :: Int
,   chainEnd   :: Int
,   chainRunning :: Bool
,   chainProgress :: [Bool]
} deriving (Show, Read)

maybeRead = fmap fst . listToMaybe . reads

process :: [String] -> StateT Chains IO ()
process (cmd:args) |  cmd == "add"  = addChain args  >> showChains
                   |  cmd == "show" = showChains
                   |  cmd == "done" = doneChain args >> showChains

editChains = undefined
loadChains = fmap maybeRead $ readFile "chains"

addChain :: [String] -> StateT Chains IO ()
addChain (name:_) = do
    chains <- get
    let chain = Chain name 1 0 True []
    put $ M.insert name chain chains

showChains :: StateT Chains IO ()
showChains = do
    chains <- fmap M.elems get
    let r = transpose $ map showChain $ zip (iterate (+1) 0) chains
    liftIO $ print r
    mapM_ (\x -> liftIO $ putStrLn $ concat x) r

showChain :: (Int, Chain) -> [String]
showChain (i, Chain{..}) = [chainName++tab, show chainStart ++ tab,"---\t",progress]
  where
    tab = "\t"
    tabs = take i $ repeat '\t'
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

main :: IO ()
main = do
    args <- getArgs 
    chains  <- loadChains >>= \case
        Just chains -> execStateT (process args) chains 
        Nothing     -> execStateT (process args) M.empty
    writeFile "chains" $ show chains


