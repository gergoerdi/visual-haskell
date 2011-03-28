module Main where

import Language.SSTG.Syntax
import Language.SSTG.Serialization

import Vis.FromSSTG
import Vis.Flatten
import Vis.ToSource
import Vis.CNode

import System.Environment (getArgs)
import Control.Monad.ST.Strict
import Control.Applicative
import Control.Monad

import Language.Haskell.Pretty

import Outputable
import RdrName
import Data.Maybe

main = do
  stgbs <- getArgs
  groups <- concat <$> mapM readStgb stgbs
  
  let names = runST $ runCNodeM $ do
        cnodes <- runFromSSTG (fromSSTG groups)
        return $ map cnodeName cnodes
  forM_ (catMaybes names) $ \name -> do
    putStrLn $ showRdrName name
  
