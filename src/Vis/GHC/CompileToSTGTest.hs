module Main where

import Vis.GHC.CompileToSTG
import Vis.GHC.SimpleSTG

import Outputable (printDump)
import StgSyn (pprStgBindingsWithSRTs)
import System.Environment (getArgs)
import Control.Monad

import Name

main :: IO ()
main = do    
  (file:_) <- getArgs
  stgs <- toStg file
  forM_ stgs $ \stg -> do
    printDump $ pprStgBindingsWithSRTs stg
    mapM_ print $ map (simplifyBinding . fst) stg

instance (Show Name) where
  show name = show (occNameString $ nameOccName name) ++ show (nameUnique name)
