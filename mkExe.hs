#!/usr/bin/env runhaskell

import System.Process
import System.Exit
import System.Info
import System.FilePath
import Control.Monad

catToFile :: [FilePath] -> FilePath -> IO ()
catToFile files outfile = do
	writeFile outfile ""
	forM_ files $ \filename -> do
	   	contents <- readFile filename
	   	appendFile outfile contents

main = do
	ExitSuccess <- rawSystem "elm" ["-mo", "Test.elm"]

	putStrLn "Making exe"

	let filename = 
		if (os == "mingw32") 
		then "runTest.js"
		else "runTest"

	catToFile ["prescript.sh", "elm-runtime.js", combine "build" "Test.js", "handler.js"] filename

	ExitSuccess <- 
		if (os == "mingw32") 
		then return ExitSuccess
		else rawSystem "chmod" ["+x", filename]

	return ()
