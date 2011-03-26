module Vis.FromSSTG (runFromSSTG, fromSSTG) where

import Language.SSTG.Syntax

import Vis.Node
import Vis.CNode

import StgSyn
import Name

import Control.Applicative
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type FromSSTG s a = ReaderT (Map Name (CNode s Name)) (CNodeM s) a

runFromSSTG = runReaderT `flip` Map.empty

withVars vars f = do
  newBinds <- forM vars $ \var -> do
    node <- mkCNode_ (Just var)
    return (var, node)
  local (Map.union `flip` Map.fromList newBinds) f

withBindings :: [SStgBinding Name] -> FromSSTG s a -> FromSSTG s a
withBindings bindings f = withVars (map getVar bindings) $ do
  forM_ bindings $ \(SStgBinding var rhs) -> setVar var =<< fromRhs rhs
  f
  where getVar (SStgBinding var _) = var
    
lookupBind x = asks $ Map.lookup x

setVar :: Name -> CNode s Name -> FromSSTG s ()
setVar x node = do
  payload <- readThunk node
  node' <- fromJust <$> lookupBind x
  writeThunk node' payload

fromExpr :: SStgExpr Name -> FromSSTG s (CNode s Name)
fromExpr (SStgApp f args) = mkCNode Nothing False =<< App <$> fromVar f <*> mapM fromArg args
fromExpr (SStgLit lit) = mkCNode Nothing False $ Literal lit
fromExpr (SStgConApp con args) = mkCNode Nothing False =<< ConApp con <$> mapM fromArg args
fromExpr (SStgOpApp op args) = mkCNode Nothing False =<< PrimApp prim <$> mapM fromArg args
  where StgPrimOp prim = op
fromExpr (SStgLam vars body) = mkCNode Nothing False =<< Lambda vars <$> fromExpr body
fromExpr (SStgCase expr (a:as)) = do
  alts <- mapM fromAlt (as ++ [a]) -- we rotate a:as to make sure the wildcard case (if any) is the last one
  node <- fromExpr expr
  mkCNode Nothing False $ Case node alts
fromExpr (SStgLet bindings body) = withBindings bindings $ fromExpr body

fromVar :: Name -> FromSSTG s (CNode s Name)
fromVar x = do
  lookup <- lookupBind x
  case lookup of
    Just node -> return node
    Nothing -> mkCNode Nothing False $ ParamRef x  

fromAlt :: SStgAlt Name -> FromSSTG s (Alt Name (CNode s Name))
fromAlt (SStgAlt pat expr) = Alt (fromPat pat) <$> fromExpr expr

fromPat :: SStgPat Name -> Pat Name
fromPat SStgPatWildcard = PWildcard
fromPat (SStgPatData con vars) = PConApp con $ map PVar vars
fromPat (SStgPatLit lit) = PLiteral lit

fromArg :: SStgArg Name -> FromSSTG s (CNode s Name)
fromArg (SStgArgVar v) = fromVar v
fromArg (SStgArgLit lit) = mkCNode Nothing False $ Literal lit

fromRhs :: SStgRhs Name -> FromSSTG s (CNode s Name)
fromRhs (SStgRhsCon con args) = fromExpr (SStgConApp con args)
fromRhs (SStgRhsClosure update vars expr) = mkCNode Nothing reentrant =<< (Lambda vars <$> fromExpr expr)
  where reentrant = case update of
          Updatable -> True
          _ -> False

fromSSTG :: [SStgBinding Name] -> FromSSTG s [CNode s Name]
fromSSTG bs = withBindings bs $ asks (map snd . Map.toList)
