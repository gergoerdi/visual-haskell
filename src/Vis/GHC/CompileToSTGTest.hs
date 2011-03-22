module Main where

import Vis.GHC.CompileToSTG
import Vis.SSTG.SimpleSTG
import Vis.IOUtils
import Vis.SSTG.Serialization

import Outputable
import StgSyn (pprStgBindings)
import HscTypes
import Module
import Binary
import Name
import UniqFM
import FastMutInt
import Unique

import Data.Array
import Data.IORef
import IO
import System.Environment (getArgs)
import Control.Monad
import System.FilePath (replaceExtension)
import System.Directory (canonicalizePath)
import Data.Word
import Control.Arrow

writeStg mod stg = withOutput (fileName mod "stg") $ \h -> do
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
  stgs <- toStg args
  forM_ stgs $ \(mod, stg) -> do
    writeStg mod stg
    writeStgb mod stg
