module Vis.FromSTGTest where

import Vis.FromSTG
import Vis.Node
import Vis.CNode
import Vis.GHC.CompileToSTG

import Vis.Flatten
import Vis.ToSource
import Control.Monad.ST (runST)
import Language.Haskell.Pretty (prettyPrint)

import StgSyn
import Outputable

import Control.Applicative

main :: IO ()
main = do
  stg <- toStg "../test/Hello.hs"
  printDump $ pprStgBindingsWithSRTs stg    
  
  putStrLn $ unlines $ runST $ do
    fnodes <- runCNodeM $ do
      cnodes <- runFromSTG (concat <$> fromBindings (map fst stg))
      mapM flatten cnodes
    return $ map (prettyPrint . toSource) fnodes ++ map show fnodes
