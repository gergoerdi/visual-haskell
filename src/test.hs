module Main where

import Language.SSTG.Syntax
import Language.SSTG.Serialization
import Language.SSTG.GHC.CompileToSTG

import Vis.FromSSTG
import Vis.Flatten
import Vis.ToSource
import Vis.CNode
import Vis.Reduce

import Data.Map (Map, (!))
import qualified Data.Map as Map

import System.Environment (getArgs)
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad

import Language.Haskell.Pretty

import Outputable
import StgSyn (pprStgBindings)
import RdrName
import Data.Maybe

import Name
import Unique
import SrcLoc
import PrelNames

main = do
  args <- getArgs
  sourceFile <- case args of
    [fileName] -> return fileName
    _ -> error $ "Usage: test source.hs"
  
  stgs <- concat <$> (mapM readStgb $ map (\p -> "../lib/" ++ p ++ ".stgb") ["ghc-prim", "base"])
  cr <- compileToStg [sourceFile]
  -- mapM_ (putStrLn . showSDoc . pprStgBindings . map fst . snd) $ cr_stgs cr
  let stgProg = map (simplifyBinding . fst) $ concatMap snd $ cr_stgs cr
      
  runCNodeM $ do
    cmap <- runFromSSTG (fromSSTG $ stgs ++ stgProg)
    let testName = mkOrig mAIN $ mkVarOcc "test"
        cnodeTest = cmap!testName
    let step = do
          fnode <- flatten cnodeTest
          liftIO . putStrLn . prettyPrint . toSource $ fnode
          reduceStep cnodeTest
          return fnode
    replicateM_ 5 step
