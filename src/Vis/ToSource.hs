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
toSource = projectNode

projectNode :: FNode -> H.HsExp
projectNode (FNode payload) = projectPayload payload
projectNode (FVarRef x) = H.HsVar $ projectFName x
projectNode (FLet binds body) = H.HsLet (map toBind binds) $ projectNode body
  where toBind (Bind var pats def) = H.HsPatBind noLoc 
                                       (H.HsPVar $ projectVar var) 
                                       (H.HsUnGuardedRhs $ projectNode def) []
        projectVar (Given (Name n)) = n
        projectVar (Generated v) = H.HsIdent v

toApp = foldl1 H.HsApp 

projectFName (Given n) = projectName n
projectFName (Generated v) = H.UnQual $ H.HsIdent v

projectName (Name n) = H.UnQual n
projectName (Special s) = H.Special s

noLoc = error "No location"

ensureLen l es = es ++ replicate (l - length es) H.HsWildCard

projectPayload :: Payload FNode -> H.HsExp
projectPayload Uninitialized = H.HsWildCard
projectPayload (ParamRef x) = H.HsVar $ projectName x
projectPayload (IntLit n) = H.HsLit $ H.HsInt n  
projectPayload (App e f) = H.HsApp (projectNode e) (projectNode f)
projectPayload (BuiltinFunApp op args) = H.HsParen $ toApp $ fun:(map projectNode args)
  where fun = case op of
          IntPlus -> H.HsVar (H.UnQual $ H.HsSymbol "+#")
          IntMinus -> H.HsVar (H.UnQual $ H.HsSymbol "-#")
projectPayload (ConApp c args) = toApp $ con:(map projectNode args)
  where con = H.HsCon $ projectName c
projectPayload (CaseApp arity alts args) = H.HsCase expr $ map projectAlt alts
  where expr = H.HsTuple $ ensureLen arity $ map projectNode args
        projectAlt (Alt pats node) = let body = projectNode node
                                     in H.HsAlt noLoc (H.HsPTuple pats) (H.HsUnGuardedAlt body) []
