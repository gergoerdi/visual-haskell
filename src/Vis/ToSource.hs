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

import Name

toSource :: FNode Name -> H.HsExp
toSource = parenExpr . projectNode

paren :: H.HsExp -> H.HsExp                 
paren e@(H.HsParen _) = e
paren e@(H.HsLit _) = e
paren e@(H.HsVar _) = e
paren e@(H.HsTuple _) = e
paren e = H.HsParen e
                 
parenExpr (H.HsApp (H.HsApp op left) right) | Just opName <- getInfix = H.HsInfixApp left' opName right'
  where getInfix = case op of
          H.HsCon c@(H.Special H.HsCons) -> Just $ H.HsQConOp c
          H.HsVar sym@(H.UnQual (H.HsSymbol _)) -> Just $ H.HsQVarOp sym
          _ -> Nothing
        
        forceTopParen e@(H.HsApp _ _) = paren e
        forceTopParen e = e
        
        left' = forceTopParen $ parenExpr left
        right' = forceTopParen $ parenExpr right          
parenExpr (H.HsApp e f@(H.HsApp _ _)) = H.HsApp (parenExpr e) (paren (parenExpr f))
parenExpr (H.HsApp e f@(H.HsLambda _ _ _)) = H.HsApp (parenExpr e) (paren (parenExpr f))
parenExpr (H.HsApp e f) = H.HsApp (parenExpr e) (parenExpr f)
parenExpr (H.HsLambda loc pats body) = H.HsLambda loc pats $ parenExpr body
parenExpr (H.HsLet decls body) = H.HsLet (map parenDecl decls) $ parenExpr body
parenExpr (H.HsCase e alts) = H.HsCase (parenExpr e) (map parenAlt alts)
parenExpr (H.HsIrrPat e) = H.HsIrrPat $ parenExpr e
parenExpr expr = expr

parenAlt (H.HsAlt loc pat gAlt wheres) = H.HsAlt loc pat (parenGAlt gAlt) (map parenDecl wheres)

parenGAlt (H.HsUnGuardedAlt expr) = H.HsUnGuardedAlt $ parenExpr expr

parenDecl (H.HsPatBind loc pat rhs wheres) = H.HsPatBind loc pat (parenRhs rhs) (map parenDecl wheres)
parenDecl (H.HsFunBind matches) = H.HsFunBind $ map parenMatch matches

parenMatch (H.HsMatch loc name pats rhs wheres) = H.HsMatch loc name pats (parenRhs rhs) (map parenDecl wheres)

parenRhs (H.HsUnGuardedRhs expr) = H.HsUnGuardedRhs $ parenExpr expr


projectNode :: FNode Name -> H.HsExp
projectNode (FNode payload) = projectPayload payload
projectNode (FVarRef x) = H.HsVar $ projectFName x
projectNode (FLet binds body) = H.HsLet (map toBind binds) $ projectNode body
  where 
    -- toBind (Bind var (FNode (Case alts []))) = H.HsFunBind $ map toMatch alts
    --   where toMatch (Alt pats body) = H.HsMatch noLoc name pats (H.HsUnGuardedRhs $ projectNode body) []
    --         name = projectVar var
    toBind (Bind var def) = H.HsPatBind noLoc 
                            (H.HsPVar $ projectVar var) 
                            (H.HsUnGuardedRhs $ projectNode def) []
    projectVar (Given n) = H.HsIdent $ occNameString $ nameOccName n
    projectVar (Generated s) = H.HsIdent $ "v" ++ show (unSerial s)

toApp = foldl1 H.HsApp

projectFName (Given n) = projectName n
projectFName (Generated s) = H.UnQual $ H.HsIdent $ "v" ++ show (unSerial s)

-- projectName (Name n) = H.UnQual n
-- projectName (Special s) = H.Special s
projectName name = H.UnQual $ H.HsSymbol $ occNameString $ nameOccName name

noLoc = error "No location"

ensureLen l es = es ++ replicate (l - length es) H.HsWildCard

projectPayload :: Payload Name (FNode Name) -> H.HsExp
projectPayload Uninitialized = H.HsWildCard
projectPayload (Knot node) = H.HsIrrPat $ paren $ projectNode node
projectPayload (Lambda pat node) = H.HsLambda noLoc [projectPat pat] $ projectNode node
projectPayload (ParamRef x) = H.HsVar $ projectName x
projectPayload (Lit (IntLit n)) = H.HsLit $ H.HsInt n  
projectPayload (App e args) = toApp (projectNode e:map projectNode args)
projectPayload (BuiltinFunApp op args) = toApp $ fun:(map projectNode args)
  where fun = case op of
          IntPlus -> H.HsVar (H.UnQual $ H.HsSymbol "+#")
          IntMinus -> H.HsVar (H.UnQual $ H.HsSymbol "-#")
projectPayload (ConApp c args) = toApp $ con:(map projectNode args)
  where con = H.HsCon $ projectName c
-- projectPayload (Case [Alt pats body] []) = paren $ H.HsLambda noLoc pats $ projectNode body
-- projectPayload (CaseApp 1 alts args) = H.HsCase expr $ map projectAlt alts
--   where expr = case args of 
--           [] -> H.HsWildCard
--           [arg] -> projectNode arg        
--         projectAlt (Alt [pat] node) = H.HsAlt noLoc pat (H.HsUnGuardedAlt $ projectNode node) []
projectPayload (Case alts [arg]) = H.HsCase (projectNode arg) $ map projectAlt alts
  where projectAlt (Alt [pat] node) = H.HsAlt noLoc (projectPat pat) (H.HsUnGuardedAlt $ projectNode node) []
projectPayload (Case alts args) = H.HsCase expr $ map projectAlt alts
  where expr = H.HsTuple $ map projectNode args
        projectAlt (Alt pats node) = let body = projectNode node
                                     in H.HsAlt noLoc (H.HsPTuple $ map projectPat pats) (H.HsUnGuardedAlt body) []

projectPat :: Pat Name -> H.HsPat
projectPat = error "projectPat"
