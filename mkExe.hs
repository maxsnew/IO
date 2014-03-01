{-# OPTIONS_GHC -Wall #-}

import           Control.Monad
import           System.Exit
import           System.FilePath
import           System.Process
import           System.Environment

import           Elm.Internal.Paths as Elm
import qualified Paths_ElmIO as ElmIO

catToFile :: [FilePath] -> FilePath -> IO ()
catToFile files outfile = do
	writeFile outfile ""
	forM_ files $ \filename -> do
	   	contents <- readFile filename
	   	appendFile outfile contents

main :: IO ()
main = do
  args <- getArgs
  case args of
    [infile, outfile] -> do
      dir <- ElmIO.getDataDir
      code <- rawSystem "elm" ["-mo", "--src-dir=" ++ dir, infile]
      case code of
        ExitFailure _ -> do
          putStrLn "Something went wrong during compilation"
          exitWith code
        ExitSuccess -> do
          putStrLn "Making exe"
          prescript <- ElmIO.getDataFileName "prescript.js"
          handler   <- ElmIO.getDataFileName "handler.js"
          catToFile [ prescript
                    , Elm.runtime
                    , "build" </> replaceExtension infile "js"
                    , handler ]
                    outfile
    _ -> putStrLn $ "Expected input file and output file arguments, but got " ++ show (length args) ++ " args."
