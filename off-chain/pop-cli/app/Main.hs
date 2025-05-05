module Main where

import qualified Pop.Cli as Cli
import System.Environment (getArgs)

main :: IO ()
main =
    getArgs >>= Cli.pop >>= print
