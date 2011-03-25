module Main where

import Vis.GHC.CompileToSTG
import Vis.SSTG.SimpleSTG
import Vis.IOUtils
import Vis.SSTG.Serialization

import Outputable
import StgSyn (pprStgBindings)
import HscTypes
import Module
import GHC

import SrcLoc
import Name
import Unique
import Literal

import IO
import System.Environment (getArgs)
import Control.Monad
import System.Directory (canonicalizePath)
import GHC.Paths (libdir)
import System.FilePath
import System.Directory

import Outputable

main :: IO ()
main = do
  [fileName] <- getArgs
  runGhc (Just libdir) $ do
    sstgs <- do
      liftIO $ putStrLn . unwords $ ["Reading", fileName]
      readStgb fileName
    forM_ sstgs $ \(SStgBinding id rhs) -> do
      liftIO $ putStrLn $ showSDoc $ ppr id
    return ()
  return ()

testNames :: IO ()
testNames = do
  uniq <- return $ mkUnique 'c' 0
  occ <- return $ mkOccName varName "foo"
  name <- return $ mkInternalName uniq occ noSrcSpan
  sstg <- return $ SStgBinding name $ SStgRhsClosure SReEntrant [] $ SStgLit $ MachChar 'z'
  writeStgb fn [sstg, sstg]
  
  [sstg', sstg''] <- runGhc (Just libdir) $ readStgb fn
  let (SStgBinding name' _) = sstg'
      (SStgBinding name'' _) = sstg''
      
  print $ name' == name''
  return ()  
  
  where fn = "/tmp/foo.stgb"
