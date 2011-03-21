module Vis.FromSTGTest where

import Vis.FromSTG
import Vis.Node
import Vis.CNode
import Vis.GHC.CompileToSTG
import Vis.Reduce

import Vis.Flatten
import Vis.ToSource
import Control.Monad.ST (runST)
import Language.Haskell.Pretty (prettyPrint)
import Data.List

import StgSyn
import Outputable
import Name

import Control.Applicative
import Control.Monad (replicateM)

import Debug.Trace

main :: IO ()
main = do
  stgs <- toStg "../test/Hello.hs"
  mapM_ (printDump . pprStgBindingsWithSRTs) stgs
  
  putStrLn $ unlines $ runST $ do
    fnodes <- runCNodeM $ do
      cnodes <- runFromSTG (fromSTG (concatMap (map fst) stgs))
      let (Just cnode) = find isMain cnodes
      replicateM steps $ do
        result <- flatten cnode        
        reduceStep cnode
        return result
      -- mapM flatten cnodes
    -- return $ map show fnodes
    return $ map (prettyPrint . toSource) fnodes
    -- return $ map show fnodes
  where steps = 1
        isMain cnode = fmap (occNameString . nameOccName) (cnodeName cnode) == Just "main"
