{-# LANGUAGE StandaloneDeriving #-}
module Vis.GHC.SimpleSTG where

import StgSyn
import CoreSyn (AltCon(..))
import DataCon
import Name
import Literal
import Type

import PrimOp
import ForeignCall

import GHC.Show

import Control.Arrow
-- import Control.Applicative
-- import Control.Monad.Reader
-- import Data.Map (Map)
-- import qualified Data.Map as Map
-- import Data.Maybe
import Debug.Trace

deriving instance Show PrimCall
deriving instance Show ForeignCall
deriving instance Show CCallSpec
deriving instance Show CCallTarget
deriving instance Show CCallConv

instance (Show StgOp) where
  showsPrec n (StgPrimOp op) = showParen (n > appPrec) $ showString $ 
                                 unwords ["StgPrimOp", show op]
  showsPrec n (StgPrimCallOp pcall) = showParen (n > appPrec) $ showString $ 
                                        unwords ["StgPrimCallOp", show pcall]
  showsPrec n (StgFCallOp fcall _) = showParen (n > appPrec) $ showString $ 
                                       unwords ["StgFCallOp", show fcall]
                                       
data SStgExpr id = SStgApp id [SStgArg id]
                 | SStgLit Literal
                 | SStgConApp id [SStgArg id]
                 | SStgOpApp StgOp [SStgArg id]
                 | SStgLam [id] (SStgExpr id)
                 | SStgCase (SStgExpr id) [SStgAlt id]
                 | SStgLet [SStgBinding id] (SStgExpr id)
                 deriving Show
                           
data SStgArg id = SStgArgVar id
                | SStgArgLit Literal
                  -- | SStgArgType Type -- TODO: Type?
                deriving Show

data SStgAlt id = SStgAlt (SStgAltCon id) (SStgExpr id)
                deriving Show
                          
data SStgAltCon id = SStgAltData id [id]
                   | SStgAltLit Literal
                   | SStgAltWildcard
                   deriving Show
                      
data SStgBinding id = SStgBinding id (SStgRhs id)
                    deriving Show
                              
data SStgRhs id = SStgRhsCon id [SStgArg id]
                | SStgRhsClosure SUpdateFlag [id] (SStgExpr id)
                deriving Show
                          
data SUpdateFlag = SUpdatable | SReEntrant                          
                 deriving Show

simplifyBinding :: StgBinding  -> [SStgBinding Name]
simplifyBinding (StgNonRec x body) = [SStgBinding (getName x) (simplifyRhs body)]
simplifyBinding (StgRec binds) = map (uncurry SStgBinding . (getName *** simplifyRhs)) binds

simplifyRhs :: StgRhs -> SStgRhs Name
simplifyRhs (StgRhsCon _ con args) = SStgRhsCon (getName con) $ map simplifyArg args
simplifyRhs (StgRhsClosure _ _ _ update _ args expr) = SStgRhsClosure update' (map getName args) (simplifyExpr expr)
  where update' = case update of          
          ReEntrant -> SReEntrant
          _ -> SUpdatable

simplifyArg :: StgArg -> SStgArg Name
simplifyArg (StgVarArg x) = SStgArgVar (getName x)
simplifyArg (StgLitArg lit) = SStgArgLit lit
simplifyArg (StgTypeArg _) = error "StgTypeArg"

simplifyExpr :: StgExpr -> SStgExpr Name
simplifyExpr (StgApp f args) = SStgApp (getName f) $ map simplifyArg args
simplifyExpr (StgLit lit) = SStgLit lit
simplifyExpr (StgConApp con args) = SStgConApp (getName con) $ map simplifyArg args
simplifyExpr (StgOpApp op args _) = SStgOpApp op $ map simplifyArg args
simplifyExpr (StgLam _ xs body) = SStgLam (map getName xs) $ simplifyExpr body
simplifyExpr (StgCase e _ _ _ _ _ alts) = SStgCase (simplifyExpr e) $ map simplifyAlt alts
simplifyExpr (StgLet binding body) = SStgLet (simplifyBinding binding) $ simplifyExpr body
simplifyExpr (StgLetNoEscape _ _ binding body) = simplifyExpr (StgLet binding body)
simplifyExpr (StgSCC _ e) = simplifyExpr e
simplifyExpr (StgTick _ _ e) = simplifyExpr e

simplifyAlt (con, vars, _, e) = SStgAlt con' $ simplifyExpr e
  where con' = case con of
          DataAlt con -> SStgAltData (getName con) (map getName vars)
          LitAlt lit -> SStgAltLit lit
          DEFAULT -> SStgAltWildcard
