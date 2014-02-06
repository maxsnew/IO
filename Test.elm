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

readUntil : Char -> IO String
readUntil end = let go s = getChar >>= \c ->
                           if c == end
                           then pure s
                           else go (String.append s (String.cons c ""))                             
                in go ""

getLine : IO String
getLine = readUntil '\n'

forever : IO a -> IO ()
forever m = m >>= (\_ -> forever m)

echo : IO ()
echo = forever (getLine >>= putStrLn)

hello : IO ()
hello = putStrLn "Hello, Console!" >>
        putStrLn "I'll echo your input:" >>
        (getLine >>= putStrLn) >>
        putStrLn "That's all, folks!" >>
        exit 0

port requests : Signal { mPut  : Maybe String
                       , mExit : Maybe Int
                       , mGet  : Bool
                       }
port requests = Run.run responses hello

port responses : Signal (Maybe String)
