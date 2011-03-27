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

main = do
  stgbs <- getArgs
  groups <- concat <$> mapM readStgb stgbs
  forM_ groups $ \group -> do
    forM group $ \(SStgBinding name _) ->
      putStrLn $ showSDoc $ ppr name
    let fnodes = runST $ runCNodeM $ (mapM flatten =<< runFromSSTG (fromSSTG [group]))
    forM_ fnodes $ putStrLn . prettyPrint . toSource

  -- let fnodes = runST $ runCNodeM $ do
  --       cnodes <- runFromSSTG (fromSSTG groups)
  --       mapM flatten cnodes
  -- -- forM_ fnodes $ putStrLn . prettyPrint . toSource
  -- forM_ fnodes $ const $ putStrLn "foo"
