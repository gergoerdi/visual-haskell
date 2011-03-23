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

import IO
import System.Environment (getArgs)
import Control.Monad
import System.Directory (canonicalizePath)
import GHC.Paths (libdir)

writeStg mod stg = withOutput (fileName "stg" mod) $ \h -> do
    let fnSrc = ml_hs_file . ms_location $ mod
    case fnSrc of        
      Just fnSrc -> do
        fnSrc' <- canonicalizePath fnSrc
        hPutStrLn h $ unwords ["--", "Compiled from", fnSrc']
      Nothing -> return ()
    printForUser h neverQualify $ pprStgBindings $ map fst stg    

mainR :: IO ()
mainR = runGhc (Just libdir) $ do
  -- sstg <- readStgb "/tmp/base/Data/List.stgb"
  sstg <- readStgb "/tmp/GHC/Integer/Type.stgb"
  -- sstg <- readStgb "/tmp/GHC/Bool.stgb"
  return ()

mainW :: IO ()
mainW = do    
  args <- getArgs
  stgs <- toStg args
  forM_ stgs $ \(mod, stg) -> do
    writeStg mod stg
    writeStgb mod stg

main = mainR
