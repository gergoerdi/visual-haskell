module Main where

import Vis.GHC.CompileToSTG

import Outputable (printDump)
import StgSyn (pprStgBindingsWithSRTs)
import System.Environment (getArgs)

main :: IO ()
main = do    
  (file:_) <- getArgs
  stg <- toStg file
  mapM_ (printDump . pprStgBindingsWithSRTs) stg
