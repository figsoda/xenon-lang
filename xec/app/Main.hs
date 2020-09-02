module Main where

import Data.Text ( pack )
import System.IO ( hFlush, stdout )
import Text.Megaparsec ( parseTest )
import Xenon.Parser ( test )

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  xs <- getLine
  parseTest test (pack xs)
  main
