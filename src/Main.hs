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

shareFile :: FilePath -> IO FilePath
shareFile name = do
  path <- ElmIO.getDataFileName $ "share" </> name
  exists <- doesFileExist path
  if exists
    then return path
    else do
      env <- getEnv "ELM_IO"
      return $ env </> "share" </> name

getDataDir :: IO FilePath
getDataDir = do
  path <- ElmIO.getDataDir
  exists <- doesFileExist path
  if exists
    then return path
    else do
      env <- getEnv "ELM_IO"
      return env

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
    prescript <- shareFile "prescript.js"
    handler   <- shareFile "handler.js"
    catToFile [ prescript
              , Elm.runtime
              , "build" </> replaceExtension infile "js"
              , handler ]
              outfile

compile :: FilePath -> [String] -> IO ExitCode
compile infile elmFlags = do
  dir <- getDataDir
  system "elm" $ ["-mo", "--src-dir=" ++ dir] ++ elmFlags ++ [infile]
  where system cmd args = do
          putStrLn . unwords $ cmd:args
          rawSystem cmd args

addImports :: FilePath -> IO ()
addImports src = do
  imports <- shareFile "imports.elm"
  imports' <- readFile imports
  srcFile <- strictRead src
  let (firstLine:rest) = lines srcFile
  let new = unlines $ [firstLine, imports'] ++ rest
  writeFile src new
  where strictRead file = do
          string <- readFile file
          length string `seq` return string

parseArgs :: [String] -> IO (Bool, [String], FilePath, FilePath)
parseArgs raw = case raw of
  [] -> usageExit
  [_] -> usageExit
  ("--default-ports"):inF:outF:rest -> return (True, rest, inF, outF)
  inF:outF:rest -> return (False, rest, inF, outF)
  where usageExit = usage >> (exitWith $ ExitFailure 1)

main :: IO ()
main = do
  rawArgs <- getArgs
  (defaultPorts, elmFlags, infile, outfile) <- parseArgs rawArgs
  if not defaultPorts
    then do code <- compile infile elmFlags
            buildJS code infile outfile
    else do -- Adding boilerplate to a temp file
            ports <- shareFile "boilerplate.elm"
            (src,handle) <- openTempFile "" infile
            hClose handle
            catToFile [infile, ports] src
            addImports src

            -- Build output js file
            code <- compile src elmFlags
            buildJS code src outfile

            -- Cleanup
            removeFile src
            removeFile $ "build" </> replaceExtension src "js"
            removeFile $ "cache" </> replaceExtension src "elmi"
            removeFile $ "cache" </> replaceExtension src "elmo"

usage :: IO ()
usage = putStrLn $ "USAGE: elm-io [--default-ports] <input-file> <output-file> [elm-flags]*"
