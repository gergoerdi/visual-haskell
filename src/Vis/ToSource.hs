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
import Literal

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
parenExpr (H.HsApp e f) = H.HsApp (parenExpr e) (paren (parenExpr f))
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
projectNode (FVarRef x) = H.HsVar $ H.UnQual $ projectFName x
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
projectFName (Generated s) = H.HsIdent $ "v" ++ show (unSerial s)

-- projectName (Name n) = H.UnQual n
-- projectName (Special s) = H.Special s
projectName name = (if isSymOcc occ then H.HsSymbol else H.HsIdent) $ occNameString occ
  where occ = nameOccName name

noLoc = H.SrcLoc "foo" 0 0 -- error "No location"

projectPayload :: Payload Name (FNode Name) -> H.HsExp
projectPayload (Lambda [] node) = projectNode node
projectPayload (Lambda vars node) = H.HsLambda noLoc (map (H.HsPVar . projectName) vars) $ projectNode node
projectPayload (ParamRef x) = H.HsVar $ H.UnQual $ projectName x
projectPayload (Literal lit) = H.HsLit $ case lit of
  MachInt n -> H.HsInt n
projectPayload (App e args) = toApp (projectNode e:map projectNode args)
projectPayload (PrimApp op args) = toApp $ fun:(map projectNode args)
  where fun = H.HsVar $ H.UnQual $ H.HsIdent $ show op
projectPayload (ConApp c args) = toApp $ con:(map projectNode args)
  where con = H.HsCon $ H.UnQual $ projectName c
projectPayload (Case e alts) = H.HsCase (projectNode e) $ map projectAlt alts
  where projectAlt (Alt pat node) = H.HsAlt noLoc (projectPat pat) (H.HsUnGuardedAlt $ projectNode node) []

projectPat :: Pat Name -> H.HsPat
projectPat (PVar x) = H.HsPVar $ projectName x
projectPat (PAsPat x p) = H.HsPAsPat (projectName x) $ projectPat p
projectPat (PConApp c ps) = H.HsPApp (H.UnQual $ projectName c) $ map projectPat ps
