-- module Main where

import Vis.GHC.CompileToSTG
import Vis.GHC.SimpleSTG

import Outputable
import StgSyn (pprStgBindings)
import HscTypes
import Module
import Binary

import IO
import System.Environment (getArgs)
import Control.Monad
import System.FilePath (replaceExtension)
import System.Directory (canonicalizePath)

import Data.Word

withOpenFile fn mode f = do
  h <- liftIO $ openFile fn mode
  f h
  liftIO $ hClose h

main :: IO ()
main = do    
  args <- getArgs
  stgs <- toStg args
  forM_ stgs $ \(mod, stg) -> do
    withWrite (modFile mod "stg") $ \h -> do
      let fnSrc = ml_hs_file . ms_location $ mod
      case fnSrc of        
        Just fnSrc -> do
          fnSrc' <- canonicalizePath fnSrc
          hPutStrLn h $ unwords ["--", "Compiled from", fnSrc']
        Nothing -> return ()
      printForUser h neverQualify $ pprStgBindings $ map fst stg
    
    withWrite (modFile mod "stgb") $ \h -> do      
      bh <- openBinIO h      
      mapM_ (put_ bh) $ map (simplifyBinding . fst) stg

  where modFile mod ext = replaceExtension (ml_hi_file . ms_location $ mod) ('.':ext)
        withWrite fn f = (putStrLn $ unwords ["Creating", fn]) >> (withOpenFile fn WriteMode f)
