module Vis.ToSource (toSource) where

import Vis.Node

import qualified Language.Haskell.Syntax as H
import Language.Haskell.Pretty
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.STRef

toSource = projectNode 0

projectNode :: Int -> Node s -> ST s H.HsExp
projectNode depth (Node serial refPayload) | depth < 5 = do
  payload <- readSTRef refPayload
  projectPayload depth payload
projectNode _ _ =  return $ H.HsWildCard

toApp = foldl1 H.HsApp 

projectName (Name n) = H.UnQual n
projectName (Special s) = H.Special s

noLoc = error "No location"

ensureLen l es = es ++ replicate (l - length es) H.HsWildCard

projectPayload :: Int -> Payload s -> ST s H.HsExp
projectPayload depth Uninitialized = return $ H.HsWildCard
projectPayload depth (ParamRef x) = return $ H.HsVar $ projectName x
projectPayload depth (IntLit n) = return $ H.HsLit $ H.HsInt n  
projectPayload depth (App e f) = do
  liftM2 H.HsApp (projectNode (succ depth) e) (projectNode (succ depth) f)
projectPayload depth (BuiltinFunApp op args) = do
  projectArgs <- mapM (projectNode (succ depth)) args
  let fun = case op of
        IntPlus -> H.HsVar (H.UnQual $ H.HsSymbol "+#")
        IntMinus -> H.HsVar (H.UnQual $ H.HsSymbol "-#")
  return $ toApp $ fun:projectArgs
projectPayload depth (ConApp c args) = do
  projectArgs <- mapM (projectNode (succ depth)) args
  let con = H.HsCon $ projectName c
  return $ toApp $ con:projectArgs
projectPayload depth (SwitchApp arity alts args) = do
  projectAlts <- forM alts $ \ (Match pats node) -> do
    body <- projectNode (succ depth) node
    return $ H.HsAlt noLoc (H.HsPTuple pats) (H.HsUnGuardedAlt body) []
  projectArgs <- mapM (projectNode (succ depth)) args  
  let expr = H.HsTuple $ ensureLen arity projectArgs
  return $ H.HsCase expr projectAlts
