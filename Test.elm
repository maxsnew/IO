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
program = putStrLn "Hello, Console!"

reqs = Run.run program responses

port requests : Signal (Maybe String)
port requests = reqs

port responses : Signal ()
--responses = (\_ -> ()) <~ every millisecond