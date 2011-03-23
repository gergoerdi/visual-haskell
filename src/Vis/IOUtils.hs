module Vis.IOUtils (withOpenFile, withOutput, withInput, fileName) where

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
  putStrLn . unwords $ ["Creating", fn]
  withOpenFile fn WriteMode f

withInput :: FilePath -> (Handle -> IO a) -> IO a
withInput fn f = do
  putStrLn . unwords $ ["Reading", fn]
  withOpenFile fn ReadMode f

fileName :: String -> ModSummary -> FilePath
fileName ext mod = replaceExtension (ml_hi_file . ms_location $ mod) ('.':ext)
