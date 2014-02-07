{-# OPTIONS_GHC -Wall #-}

import           Control.Monad
import           System.Exit
import           System.FilePath
import           System.Process

catToFile :: [FilePath] -> FilePath -> IO ()
catToFile files outfile = do
	writeFile outfile ""
	forM_ files $ \filename -> do
	   	contents <- readFile filename
	   	appendFile outfile contents

main :: IO ()
main = do
  code <- rawSystem "elm" ["-mo", "Test.elm"]
  case code of
    ExitFailure _ -> do
      putStrLn "Something went wrong during compilation"
      exitWith code
    ExitSuccess -> do
      putStrLn "Making exe"
      catToFile [ "prescript.sh"
                , "elm-runtime.js"
                , "build" </> "Test.js"
                , "handler.js" ]
                "runTest"
