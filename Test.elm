module Main where

import open IO.IO
import IO.Runner (Request, Response)
import IO.Runner as Run

import String

mapIO : (a -> IO ()) -> [a] -> IO ()
mapIO f xs = foldr ((>>) . f) (pure ()) xs

putStr : String -> IO ()
putStr = mapIO putChar . String.toList

putStrLn : String -> IO ()
putStrLn s = putStr s >> putChar '\n'

program : IO ()
program = pure "Hello, Console!" >>= \s ->
          putStrLn s >> 
          exit 1

port requests : Signal (Maybe { mPut  : Maybe String
                              , mExit : Maybe Int
                              })
port requests = Run.run program responses

port responses : Signal ()
