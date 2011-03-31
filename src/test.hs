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

var i name = mkInternalName (mkUnique 'v' i) (mkVarOcc name) noSrcSpan
mkCon u name = mkExternalName u gHC_TYPES (mkDataOcc name) noSrcSpan

map_id = var 0 "map"
map_stg = SStgBinding map $ SStgRhsClosure ReEntrant [f, ds] $
          SStgCase wild (SStgApp ds []) $          
            [SStgAlt (SStgPatData nil []) $ SStgConApp nil [],
             SStgAlt (SStgPatData cons [x, xs]) $
               SStgLet [SStgBinding ys $ SStgRhsClosure Updatable [] $ map `app` [f, xs],
                        SStgBinding y $ SStgRhsClosure Updatable [] $ f `app` [x]] $
                 cons `con` [y, ys]]
  where app f args = SStgApp f $ Prelude.map SStgArgVar args
        con c args = SStgConApp c $ Prelude.map SStgArgVar args
        map = map_id
        ds = var 1 "ds"
        wild = var 2 "wild"
        
        f = var 3 "f"
        x = var 4 "x"
        xs = var 5 "xs"
        
        y = var 6 "y"
        ys = var 7 "ys"
        
nil = mkCon nilDataConKey "[]"
cons = mkCon consDataConKey ":"

test_stg = SStgBinding test $ SStgRhsClosure Updatable [] $
           SStgLet [SStgBinding id $ SStgRhsClosure ReEntrant [x] $ SStgApp x [],
                    SStgBinding ones $ SStgRhsCon cons [SStgArgLit (mkMachInt 1), SStgArgVar ones]] $
             SStgApp map_id [SStgArgVar id, SStgArgVar ones]         
  where test = var 100 "test"
        id = var 101 "id"
        x = var 102 "x"
        ones = var 103 "ones"

main = do
  -- stgs <- concat <$> (mapM readStgb $ map (\p -> "../lib/" ++ p ++ ".stgb") ["ghc-prim"])
  -- cr <- compileToStg ["../test/first.hs"]
  -- let stgProg = map (simplifyBinding . fst) $ concatMap snd $ cr_stgs cr
      
  let fnode = runST $ runCNodeM $ do
        -- cmap <- runFromSSTG (fromSSTG $ stgs ++ stgProg)
        cmap <- runFromSSTG (fromSSTG [[map_stg, test_stg]])
        let testName = mkRdrUnqual $ mkVarOcc "test"
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
