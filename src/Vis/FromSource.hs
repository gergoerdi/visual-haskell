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
import Language.Haskell.Pretty

builtinFromName (Name (H.HsSymbol "+")) = Just IntPlus
builtinFromName _ = Nothing

fromExpr (H.HsLit (H.HsInt n)) = mkNode (IntLit n)
fromExpr (H.HsApp f x) = mkNode =<< (App <$> fromExpr f <*> fromExpr x)
fromExpr (H.HsVar x) = do
  x' <- fromName x
  case builtinFromName x' of
    Just builtin -> mkNode $ PartialFunApp (BuiltinFun builtin) []
    Nothing -> do
      bind <- lookupBind x'
      case bind of
        Nothing -> do
          arity <- lookupArity x'
          case arity of
            Just arity -> mkNode $ PartialFunApp (Matches x' arity) []
            Nothing -> mkNode $ ParamRef x'
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
  where varFromDecl (H.HsPatBind _ (H.HsPVar x) _ _) = return $ Name x
        nodeFromDecl (H.HsPatBind _ (H.HsPVar x) (H.HsUnGuardedRhs expr) []) = do
          node <- fromExpr expr
          return (Name x, node)          
fromExpr (H.HsCon c) = mkNode =<< (PartialConApp <$> fromName c <*> return [])
fromExpr (H.HsList es) = fromExpr $ foldr cons nil es
  where cons x xs = H.HsApp (H.HsApp (H.HsCon $ H.Special $ H.HsCons) x) xs
        nil =  H.HsCon $ H.Special $ H.HsListCon
fromExpr (H.HsInfixApp left op right) = fromExpr $ H.HsApp (H.HsApp f left) right
  where f = case op of
          H.HsQVarOp var -> H.HsVar var
          H.HsQConOp con -> H.HsCon con
fromExpr e = unsupported $ prettyPrint e

fromName (H.UnQual name) = return $ Name name
fromName (H.Special special) = return $ Special special
fromName n = unsupported "qualified name"
