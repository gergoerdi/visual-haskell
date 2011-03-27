module Vis.FromSSTG (runFromSSTG, fromSSTG) where

import Language.SSTG.Syntax

import Vis.Node
import Vis.CNode

import StgSyn
import Name
import ForeignCall
import FastString
import PrimOp

import Control.Applicative
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Outputable

data VarBind s = LetBound (CNode s Name)
               | Param

type FromSSTG s a = ReaderT (Map Name (VarBind s)) (CNodeM s) a

runFromSSTG = runReaderT `flip` Map.empty

withParams vars f = do
  let newVars = map (\var -> (var, Param)) vars
  local (Map.union `flip` Map.fromList newVars) f

withVars vars f = do
  newBinds <- forM vars $ \var -> do
    node <- mkCNode_ (Just var)
    return (var, LetBound node)
  local (Map.union `flip` Map.fromList newBinds) f

withBindings :: SStgBindingGroup Name -> FromSSTG s a -> FromSSTG s a
withBindings bindings f = withVars (map getVar bindings) $ do
  forM_ bindings $ \(SStgBinding var rhs) -> 
    setVar var =<< fromRhs rhs
  f
  where getVar (SStgBinding var _) = var
    
lookupBind x = asks $ Map.lookup x

setVar :: Name -> CNode s Name -> FromSSTG s ()
setVar x node = do
  payload <- readThunk node
  lookup <- asks (lookupBind x)
  case lookup of
    Just (LetBound node') -> writeThunk node' payload    

fromExpr :: SStgExpr Name -> FromSSTG s (CNode s Name)
fromExpr (SStgApp f args) = mkCNode Nothing False =<< App <$> fromVar f <*> mapM fromArg args
fromExpr (SStgLit lit) = mkCNode Nothing False $ Literal lit
fromExpr (SStgConApp con args) = mkCNode Nothing False =<< ConApp con <$> mapM fromArg args
fromExpr (SStgOpApp op args) = mkCNode Nothing False =<< PrimApp prim <$> mapM fromArg args
  where prim = case op of
          StgPrimOp prim -> prim
          StgPrimCallOp call -> let PrimCall fs = call in error $ unwords ["Unimplemented", "StgPrimCallOp", unpackFS fs]
          StgFCallOp (CCall ccall) _ -> error $ unwords ["Unimplemented", "StgFCallOp", unpackFS target]
            where CCallSpec (StaticTarget target) conv safety = ccall
fromExpr (SStgLam vars body) = 
  withParams vars $
    mkCNode Nothing False =<< Lambda vars <$> fromExpr body
fromExpr (SStgCase var expr (a:as)) = do
  alts <- withParams [var] $ mapM fromAlt (as ++ [a]) -- we rotate a:as to make sure the wildcard case (if any) is the last one
  node <- fromExpr expr
  mkCNode Nothing False $ Case var node alts
fromExpr (SStgLet bindings body) = withBindings bindings $ fromExpr body

fromVar :: Name -> FromSSTG s (CNode s Name)
fromVar x = do
  lookup <- lookupBind x
  case lookup of
    Nothing -> error . unwords $ ["Unbound variable:", showSDoc . ppr $ x]
    Just Param -> mkCNode Nothing False $ ParamRef x
    Just (LetBound node) -> return node

fromAlt :: SStgAlt Name -> FromSSTG s (Alt Name (CNode s Name))
fromAlt (SStgAlt pat expr) = let (pat', vars) = fromPat pat
                             in Alt pat' <$> withParams vars (fromExpr expr)

fromPat :: SStgPat Name -> (Pat Name, [Name])
fromPat SStgPatWildcard = (PWildcard, [])
fromPat (SStgPatData con vars) = (PConApp con $ map PVar vars, vars)
fromPat (SStgPatLit lit) = (PLiteral lit, [])

fromArg :: SStgArg Name -> FromSSTG s (CNode s Name)
fromArg (SStgArgVar v) = fromVar v
fromArg (SStgArgLit lit) = mkCNode Nothing False $ Literal lit

fromRhs :: SStgRhs Name -> FromSSTG s (CNode s Name)
fromRhs (SStgRhsCon con args) = fromExpr (SStgConApp con args)
fromRhs (SStgRhsClosure update vars expr) = 
  withParams vars $ 
    mkCNode Nothing reentrant =<< (Lambda vars <$> fromExpr expr)
  where reentrant = case update of
          Updatable -> True
          _ -> False

fromSSTG :: [SStgBindingGroup Name] -> FromSSTG s [CNode s Name]
fromSSTG [] = asks (map getNode . Map.toList)
  where getNode (_, LetBound node) = node
        getNode (x, Param) = error . unwords $ ["Escaped parameter:", showSDoc . ppr $ x]
fromSSTG (g:gs) = withBindings g $ fromSSTG gs
