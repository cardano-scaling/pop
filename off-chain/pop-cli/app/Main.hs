module Main where

import System.Environment(getArgs)
import qualified Pop.Cli as Cli

main :: IO ()
main =
  getArgs >>= Cli.pop >>= print
