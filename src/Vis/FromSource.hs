{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Vis.FromSource where

import Vis.Node
import Vis.Monad

import qualified Language.Haskell.Syntax as H
import Control.Applicative
import Control.Monad.Error
import Control.Monad.ST
import Data.STRef
import Control.Monad.State (StateT, evalStateT)
import Data.Function (on)
import Data.Maybe

fromExpr (H.HsLit (H.HsInt n)) = mkNode (IntLit n)
fromExpr (H.HsApp e f) = mkNode =<< (App <$> fromExpr e <*> fromExpr f)
fromExpr (H.HsVar (H.UnQual op@(H.HsSymbol s))) = do
  mkNode $ PartialFunApp op []
fromExpr (H.HsVar x) = do
  x' <- fromName x
  bind <- lookupBind x'
  case bind of
    Nothing -> mkNode $ VarRef x'
    Just node -> return node
fromExpr (H.HsLet decls body) = do
  vars <- mapM varFromDecl decls
  withVars vars $ do    
    forM_ decls $ \decl -> do
      (x, node') <- nodeFromDecl decl      
      payload' <- liftST $ readSTRef (nodePayload node')
      node <- fromJust <$> lookupBind x
      writePayload node payload'
    fromExpr body            
  -- where kvFromDecl (H.HsPatBind _ (H.HsPVar x) (H.HsUnGuardedRhs expr) []) = do
  --         node <- mkNode_
  --         return (x, node)
  where varFromDecl (H.HsPatBind _ (H.HsPVar x) _ _) = return x
        nodeFromDecl (H.HsPatBind _ (H.HsPVar x) (H.HsUnGuardedRhs expr) []) = do
          node <- fromExpr expr
          return (x, node)
          
fromExpr (H.HsCon c) = mkNode =<< (PartialConApp <$> fromName c <*> return [])
fromExpr e = unsupported e

fromName (H.UnQual n) = return n
fromName n = unsupported "qualified name"
