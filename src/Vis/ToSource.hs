{-# LANGUAGE PatternGuards #-}
module Vis.ToSource (toSource) where

import Vis.Node

import qualified Language.Haskell.Syntax as H
import Language.Haskell.Pretty
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State (execStateT)
import Control.Monad.ST
import Data.STRef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

toSource :: FNode -> H.HsExp
toSource = parenExpr . projectNode

parenExpr (H.HsApp (H.HsApp op left) right) | Just opName <- getInfix = H.HsInfixApp left' opName right'
  where getInfix = case op of
          H.HsCon c@(H.Special H.HsCons) -> Just $ H.HsQConOp c
          H.HsVar sym@(H.UnQual (H.HsSymbol _)) -> Just $ H.HsQVarOp sym
          _ -> Nothing
        
        forceTopParen e@(H.HsApp _ _) = H.HsParen e
        forceTopParen e = e
        
        left' = forceTopParen $ parenExpr left
        right' = forceTopParen $ parenExpr right
          
parenExpr (H.HsApp e f@(H.HsApp _ _)) = H.HsApp (parenExpr e) (H.HsParen (parenExpr f))
parenExpr (H.HsApp e f) = H.HsApp (parenExpr e) (parenExpr f)
parenExpr (H.HsLambda loc pats body) = H.HsLambda loc pats $ parenExpr body
parenExpr (H.HsLet decls body) = H.HsLet (map parenDecl decls) $ parenExpr body
parenExpr (H.HsCase e alts) = H.HsCase (parenExpr e) (map parenAlt alts)
parenExpr expr = expr

parenAlt (H.HsAlt loc pat gAlt wheres) = H.HsAlt loc pat (parenGAlt gAlt) (map parenDecl wheres)

parenGAlt (H.HsUnGuardedAlt expr) = H.HsUnGuardedAlt $ parenExpr expr

parenDecl (H.HsPatBind loc pat rhs wheres) = H.HsPatBind loc pat (parenRhs rhs) (map parenDecl wheres)
parenDecl (H.HsFunBind matches) = H.HsFunBind $ map parenMatch matches

parenMatch (H.HsMatch loc name pats rhs wheres) = H.HsMatch loc name pats (parenRhs rhs) (map parenDecl wheres)

parenRhs (H.HsUnGuardedRhs expr) = H.HsUnGuardedRhs $ parenExpr expr


projectNode :: FNode -> H.HsExp
projectNode (FNode payload) = projectPayload payload
projectNode (FVarRef x) = H.HsVar $ projectFName x
projectNode (FLet binds body) = H.HsLet (map toBind binds) $ projectNode body
  where 
    toBind (Bind var (FNode (CaseApp arity alts []))) = H.HsFunBind $ map toMatch alts
      where toMatch (Alt pats body) = H.HsMatch noLoc name pats (H.HsUnGuardedRhs $ projectNode body) []
            name = projectVar var
    toBind (Bind var def) = H.HsPatBind noLoc 
                            (H.HsPVar $ projectVar var) 
                            (H.HsUnGuardedRhs $ projectNode def) []
    projectVar (Given (Name n)) = n
    projectVar (Generated s) = H.HsIdent $ "v" ++ show (unSerial s)

toApp = foldl1 H.HsApp

projectFName (Given n) = projectName n
projectFName (Generated s) = H.UnQual $ H.HsIdent $ "v" ++ show (unSerial s)

projectName (Name n) = H.UnQual n
projectName (Special s) = H.Special s

noLoc = error "No location"

ensureLen l es = es ++ replicate (l - length es) H.HsWildCard

projectPayload :: Payload FNode -> H.HsExp
projectPayload Uninitialized = H.HsWildCard
projectPayload (ParamRef x) = H.HsVar $ projectName x
projectPayload (IntLit n) = H.HsLit $ H.HsInt n  
projectPayload (App e f) = H.HsApp (projectNode e) (projectNode f)
projectPayload (BuiltinFunApp op args) = toApp $ fun:(map projectNode args)
  where fun = case op of
          IntPlus -> H.HsVar (H.UnQual $ H.HsSymbol "+#")
          IntMinus -> H.HsVar (H.UnQual $ H.HsSymbol "-#")
projectPayload (ConApp c args) = toApp $ con:(map projectNode args)
  where con = H.HsCon $ projectName c
projectPayload (CaseApp arity [Alt pats body] []) = H.HsParen $ H.HsLambda noLoc pats $ projectNode body
projectPayload (CaseApp 1 alts args) = toApp $ (H.HsCase expr $ map projectAlt alts):map projectNode args
  where expr = case args of 
          [] -> H.HsWildCard
          [arg] -> projectNode arg        
        projectAlt (Alt [pat] node) = H.HsAlt noLoc pat (H.HsUnGuardedAlt $ projectNode node) []
projectPayload (CaseApp arity alts args) = toApp $ (H.HsCase expr $ map projectAlt alts):map projectNode args

  where expr = H.HsTuple $ ensureLen arity $ map projectNode args
        projectAlt (Alt pats node) = let body = projectNode node
                                     in H.HsAlt noLoc (H.HsPTuple pats) (H.HsUnGuardedAlt body) []
