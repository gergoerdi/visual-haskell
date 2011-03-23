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

writeStg fn mod stg = withOutput fn $ \h -> do
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
  sstg <- readStgb "/tmp/GHC/Integer.stgb"
  -- sstg <- readStgb "/tmp/GHC/Bool.stgb"
  return ()

mainW :: IO ()
mainW = do    
  args <- getArgs
  stgs <- toStg args
  forM_ stgs $ \(mod, stg) -> do
    writeStg (fileName "stg" mod) mod stg
    putStrLn . unwords $ ["Creating", fileName "stgb" mod]
    writeStgb (fileName "stgb" mod) $ concatMap (simplifyBinding . fst) stg

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

main = testNames
