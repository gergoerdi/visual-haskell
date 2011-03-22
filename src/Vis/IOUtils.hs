module Vis.IOUtils (withOpenFile, withOutput, fileName) where

import HscTypes
import Module

import IO
import System.FilePath (replaceExtension)
import Control.Applicative

withOpenFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withOpenFile fn mode f = do
  h <- openFile fn mode
  f h <* hClose h

withOutput :: FilePath -> (Handle -> IO a) -> IO a
withOutput fn f = do
  liftIO $ putStrLn . unwords $ ["Creating", fn]
  withOpenFile fn WriteMode f

fileName :: ModSummary -> String -> FilePath
fileName mod ext = replaceExtension (ml_hi_file . ms_location $ mod) ('.':ext)
