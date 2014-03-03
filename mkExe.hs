{-# OPTIONS_GHC -Wall #-}

import           Control.Monad
import           System.Exit
import           System.FilePath
import           System.Process
import           System.Environment
import           System.IO (hClose, openTempFile)
import           System.Directory

import           Elm.Internal.Paths as Elm
import qualified Paths_ElmIO as ElmIO

catToFile :: [FilePath] -> FilePath -> IO ()
catToFile files outfile = do
	writeFile outfile ""
	forM_ files $ \filename -> do
	   	contents <- readFile filename
	   	appendFile outfile contents

buildJS :: ExitCode -> FilePath -> FilePath -> IO ()
buildJS code infile outfile = case code of
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

compile :: FilePath -> IO ExitCode
compile infile = do
  dir <- ElmIO.getDataDir
  rawSystem "elm" ["-mo", "--src-dir=" ++ dir, infile]

addImports :: FilePath -> IO ()
addImports src = do
  imports <- ElmIO.getDataFileName "imports.elm"
  imports' <- readFile imports
  srcFile <- strictRead src
  let (firstLine:rest) = lines srcFile 
  let new = unlines $ [firstLine, imports'] ++ rest
  writeFile src new
  where strictRead file = do 
          string <- readFile file 
          length string `seq` return string

main :: IO ()
main = do
  args <- getArgs
  case args of
    [infile, outfile] -> do
      code <- compile infile
      buildJS code infile outfile
    [flag, infile, outfile] -> do
      case flag of
        "--default-ports" -> do
          -- Adding boilerplate to a temp file
          ports <- ElmIO.getDataFileName "boilerplate.elm"
          (src,handle) <- openTempFile "" "ElmIO.elm"
          hClose handle
          catToFile [infile, ports] src
          addImports src

          -- Build output js file
          code <- compile src
          buildJS code src outfile

          -- Cleanup
          removeFile src
          removeFile $ "build" </> replaceExtension src "js"
          removeFile $ "cache" </> replaceExtension src "elmi"
          removeFile $ "cache" </> replaceExtension src "elmo"

        _ -> putStrLn $ "Invalid flag: " ++ show flag ++ ". Valid flags are: --default-ports" 
    _ -> putStrLn $ "Expected input file and output file arguments, but got " ++ show (length args) ++ " args."
