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
import Control.Monad.ST.Strict
import Control.Applicative
import Control.Monad

import Language.Haskell.Pretty

import Outputable
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
  let stgProg = map (simplifyBinding . fst) $ concatMap snd $ cr_stgs cr
      
  let fnode = runST $ runCNodeM $ do
        cmap <- runFromSSTG (fromSSTG $ stgs ++ stgProg)
        let testName = mkOrig mAIN $ mkVarOcc "test"
            cnodeTest = cmap!testName
        fnode <- flatten cnodeTest
        return fnode
        -- let step = do
        --       fnode <- flatten cnodeTest
        --       reduceStep cnodeTest
        --       return fnode
        -- replicateM 3 step
      src = toSource fnode
  putStrLn . prettyPrint $ src
