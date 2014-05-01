module Main where 

import System.Environment

import Cli.Cli

main :: IO ()
main = do
    (arg:args) <- getArgs 
    case arg of 
      "-c" -> cli args
      _     -> putStrLn "Error"