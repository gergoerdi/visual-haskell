module Main where

import Vis.GHC.CompileToSTG
import Vis.SSTG.SimpleSTG
import Vis.IOUtils
import Vis.SSTG.Serialization

import Outputable
import StgSyn (pprStgBindings)
import HscTypes
import Module

import IO
import System.Environment (getArgs)
import Control.Monad
import System.FilePath
import System.Directory

writeStg fn mod stg = withOutput fn $ \h -> do
    let fnSrc = ml_hs_file . ms_location $ mod
    case fnSrc of        
      Just fnSrc -> do
        fnSrc' <- canonicalizePath fnSrc
        hPutStrLn h $ unwords ["--", "Compiled from", fnSrc']
      Nothing -> return ()
    printForUser h neverQualify $ pprStgBindings $ map fst stg    

main :: IO ()
main = do    
  args <- getArgs
  cr <- compileToStg args
  
  let stgs = cr_stgs cr
  when (DumpStg `elem` (cr_opts cr)) $ 
    forM_ stgs $ \(mod, stg) -> do
      writeStg (fileName "stg" mod) mod stg    
    
  outDir <- maybe getCurrentDirectory return (cr_outDir cr)
  let stgbName = outDir </> packageIdString (cr_packageId cr) `addExtension` ".stgb"
  putStrLn . unwords $ ["Creating", stgbName]
  let bindings = map fst $ concatMap snd stgs
  writeStgb stgbName $ concatMap simplifyBinding bindings
