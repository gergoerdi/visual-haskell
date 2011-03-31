{-# LANGUAGE PatternGuards #-}
module Vis.FromSSTG (runFromSSTG, fromSSTG) where

import Language.SSTG.Syntax

import Vis.Node
import Vis.CNode

import StgSyn
import Name
import RdrName
import ForeignCall
import FastString
import PrimOp

import PrelNames
import MkId

import Control.Applicative
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.Char

import Outputable

data VarBind s = LetBound (CNode s VarName)
               | Param

type FromSSTG s a = ReaderT (Map VarName (VarBind s)) (CNodeM s) a

runFromSSTG = runReaderT `flip` Map.empty

withParams :: [VarName] -> FromSSTG s a -> FromSSTG s a
withParams vars f = do
  let newVars = map (\var -> (var, Param)) vars
  local (Map.union `flip` Map.fromList newVars) f

withVars :: [VarName] -> FromSSTG s a -> FromSSTG s a
withVars vars f = do
  newBinds <- forM vars $ \var -> do
    node <- mkCNode_ (Just var)
    return (var, LetBound node)
  local (Map.union `flip` Map.fromList newBinds) f

withBindings :: SStgBindingGroup Name -> FromSSTG s a -> FromSSTG s a
withBindings bindings f = 
  withVars (map boundVar bindings) $ do
    forM_ bindings $ \(SStgBinding x rhs) -> 
      setVar (fromName x) =<< fromRhs rhs
    f
  where boundVar (SStgBinding x _) = fromName x
    
lookupBind x = asks $ Map.lookup $ x

setVar :: VarName -> CNode s VarName -> FromSSTG s ()
setVar x node = do
  payload <- readThunk node
  lookup <- asks (lookupBind x)
  case lookup of
    Just (LetBound node') -> writeThunk node' payload    

fromExpr :: SStgExpr Name -> FromSSTG s (CNode s VarName)
fromExpr (SStgApp f args) = mkCNode Nothing False =<< App <$> fromVar f <*> mapM fromArg args
fromExpr (SStgLit lit) = mkCNode Nothing False $ Literal lit
fromExpr (SStgConApp con args) = mkCNode Nothing False =<< ConApp (fromName con) <$> mapM fromArg args
fromExpr (SStgOpApp op args) = mkCNode Nothing False =<< PrimApp prim <$> mapM fromArg args
  where prim = case op of
          StgPrimOp prim -> prim
          StgPrimCallOp call -> error . unwords $ ["Unimplemented", "StgPrimCallOp", unpackFS fs]
            where PrimCall fs = call
          StgFCallOp (CCall ccall) _ -> error . unwords $ ["Unimplemented", "StgFCallOp", unpackFS target]
            where CCallSpec (StaticTarget target) conv safety = ccall
fromExpr (SStgLam xs body) = 
  withParams vars $
    mkCNode Nothing False =<< Lambda vars <$> fromExpr body
  where vars = map fromName xs
fromExpr (SStgCase x expr (a:as)) = do
  let var = fromName x
  alts <- withParams [var] $ mapM (fromAlt var) (as ++ [a]) 
         -- we rotate a:as to make sure the wildcard case (if any) is the last one
  node <- fromExpr expr
  mkCNode Nothing False $ Case node alts
fromExpr (SStgLet bindings body) = withBindings bindings $ fromExpr body

primNames = Set.fromList $ map getName wiredInIds

builtinName ToInt = mkOrig gHC_INTEGER $ mkOccName varName "toInt#"
builtinName op = mkOrig mod $ mkOccName varName name
  where mod = case op of
          DebugErrLn -> mkPrimModule $ fsLit "GHC.Debug"
          RealWorld -> gHC_PRIM
          Error -> gHC_ERR          
          IrrefutPatError -> cONTROL_EXCEPTION_BASE
          PatError -> cONTROL_EXCEPTION_BASE
          _ -> gHC_INTEGER
        name = case op of
          ToInt -> "toInt#"
          RealWorld -> "realWorld#"
          _ -> uncapitalize $ show name
        uncapitalize (c:cs) = toLower c : cs  
        
builtinNames = Map.fromList $ map (\op -> (builtinName op, op)) $ builtinOps

integerNames = Set.fromList (map fromName [smallIntegerName, plusIntegerName, timesIntegerName]) `Set.union`
               Set.fromList (map mkName ["toInt#", 
                                         "eqInteger", "neqInteger", 
                                         "compareInteger", "ltInteger", "geInteger", "gtInteger", "leInteger",
                                         "quotRemInteger", "remInteger", "quotInteger",
                                         "divModInteger", "divInteger", "modInteger",
                                         "negateInteger", "minusInteger", "absInteger", "signumInteger",
                                         "floatFromInteger", "encodeFloatInteger", 
                                         "doubleFromInteger", "encodeDoubleInteger", "decodeDoubleInteger",
                                         "wordToInteger", "integerToWord",
                                         "shiftLInteger", "shiftRInteger",
                                         "orInteger", "andInteger", "complementInteger", "xorInteger"
                                        ])
                 where mkName = mkOrig gHC_INTEGER . mkOccName varName

debugNames = Set.fromList $ map mkName ["debugErrLn"]
  where mkName = mkOrig mod . mkOccName varName
        mod = mkPrimModule (fsLit "GHC.Debug")

fromName n | isExternalName n = mkOrig (nameModule n) (nameOccName n)
           | otherwise = mkRdrUnqual (nameOccName n)

fromPrimOp :: Name -> Maybe (FromSSTG s (CNode s VarName))
fromPrimOp x | x `Set.member` primNames = trace (unwords ["TODO:", "prim:", showSDoc . ppr $ x]) $ 
                                          Just $ (mkCNode (Just v) False $ ParamRef v)
             | otherwise = Nothing
  where v = fromName x
                                                              
fromIntegerOp :: Name -> Maybe (FromSSTG s (CNode s VarName))
fromIntegerOp x | v `Set.member` integerNames = trace (unwords ["TODO:", "integer:", showRdrName v]) $ 
                                                Just $ mkCNode (Just v) False $ ParamRef v
                | v `Set.member` debugNames = trace (unwords ["TODO:", "integer:", showRdrName v]) $ 
                                              Just $ mkCNode (Just v) False $ ParamRef v
                | otherwise = Nothing
  where v = fromName x

fromVar :: Name -> FromSSTG s (CNode s VarName)
fromVar x | Just node <- (fromPrimOp x) `mplus` (fromIntegerOp x) = node
          | otherwise = do
  let v = fromName x            
  lookup <- lookupBind v
  case lookup of
    Nothing -> do
      vars <- asks Map.keys      
      error . unlines $ [unwords ["Unbound variable:", showRdrName v]] -- ,
                         -- unwords $ map showRdrName vars]
    Just Param -> mkCNode Nothing False $ ParamRef v
    Just (LetBound node) -> return node
    -- _ -> mkCNode Nothing False $ ParamRef x
    
fromAlt :: VarName -> SStgAlt Name -> FromSSTG s (Alt VarName (CNode s VarName))
fromAlt var (SStgAlt pat expr) = let (pat', vars) = fromPat pat
                                 in Alt (PAsPat var pat') <$> withParams vars (fromExpr expr)

fromPat :: SStgPat Name -> (Pat VarName, [VarName])
fromPat SStgPatWildcard = (PWildcard, [])
fromPat (SStgPatData con xs) = (PConApp (fromName con) $ map PVar vars, vars)
  where vars = map fromName xs
fromPat (SStgPatLit lit) = (PLiteral lit, [])

fromArg :: SStgArg Name -> FromSSTG s (CNode s VarName)
fromArg (SStgArgVar v) = fromVar v
fromArg (SStgArgLit lit) = mkCNode Nothing False $ Literal lit

fromRhs :: SStgRhs Name -> FromSSTG s (CNode s VarName)
fromRhs (SStgRhsCon con args) = fromExpr (SStgConApp con args)
fromRhs (SStgRhsClosure update xs expr) = 
  withParams vars $ 
    mkCNode Nothing reentrant =<< (Lambda vars <$> fromExpr expr)
  where reentrant = case update of
          ReEntrant -> True
          _ -> False
        vars = map fromName xs
          

fromSSTG :: [SStgBindingGroup Name] -> FromSSTG s (Map VarName (CNode s VarName))
-- fromSSTG [] = asks (map getNode . Map.toList)
--   where getNode (_, LetBound node) = node
--         getNode (x, Param) = error . unwords $ ["Escaped parameter:", showSDoc . ppr $ x]
-- fromSSTG (g:gs) = withBindings g $ fromSSTG gs
fromSSTG gs = withBindings (concat gs) $ asks (Map.mapWithKey getNode)
  where getNode _ (LetBound node) = node
        getNode x Param = error . unwords $ ["Escaped parameter:", showSDoc . ppr $ x]
