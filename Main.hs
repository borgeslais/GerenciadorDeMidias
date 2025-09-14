module Main where

import Interface
import System.IO (stdout, hSetBuffering, BufferMode (NoBuffering))

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          interface
