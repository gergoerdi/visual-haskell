module Vis.FromSTGTest where

import Vis.FromSTG
import Vis.Node
import Vis.Monad
import Vis.GHC.CompileToSTG

import Vis.Flatten
import Vis.ToSource
import Control.Monad.ST (runST)
import Language.Haskell.Pretty (prettyPrint)

import StgSyn
import Outputable

import Control.Applicative
import Control.Monad.RWS
import qualified Data.Map as Map

main :: IO ()
main = do
  stg <- toStg "../test/Hello.hs"
  printDump $ pprStgBindingsWithSRTs stg    
  
  putStrLn $ unlines $ runST $ do
    cnodes <- runVis $ liftM fst $ evalRWST `flip` Map.empty `flip` firstSerial $ do
      concat <$> fromBindings (map fst stg)
    fnodes <- mapM flatten cnodes
    return $ map (prettyPrint . toSource) fnodes ++ map show fnodes
