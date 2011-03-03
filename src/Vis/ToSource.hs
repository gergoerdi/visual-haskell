{-# LANGUAGE NoMonomorphismRestriction #-}
module Vis.ToSource where
-- module Vis.ToSource (toSource) where

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

newtype SeenNodes = SeenNodes { unSeenNodes :: Map Serial Bool }

-- instance Monoid SeenNodes where
--   mempty = SeenNodes mempty
--   mappend (SeenNodes old) (SeenNodes new) = SeenNodes $ merge old new
--     where merge = Map.unionWith (\ (name1, _) (name2, _) -> (Nothing, True))

sharedNodes :: CNode s -> ST s [Serial]
sharedNodes node = map fst <$> filter snd <$> Map.toAscList <$> 
                     unSeenNodes <$> execStateT (collectNode node) (SeenNodes mempty)
  where collectNode (CNode serial name refPayload) = do
          lookup <- gets $ Map.lookup serial . unSeenNodes 
          case lookup of
            Nothing -> do
              modify $ SeenNodes . Map.insert serial False . unSeenNodes
              payload <- lift $ readSTRef refPayload
              collectPayload payload
            Just False -> do
              modify $ SeenNodes . Map.insert serial True . unSeenNodes
            Just True -> return ()
        
        collectPayload (App e f) = collectNode e >> collectNode f
        collectPayload (BuiltinFunApp _ args) = mapM_ collectNode args
        collectPayload (ConApp _ args) = mapM_ collectNode args
        collectPayload (CaseApp arity alts args) = mapM_ collectAlt alts >> mapM_ collectNode args        
        collectPayload _ = return ()
        
        collectAlt (Alt pats body) = collectNode body

type Var = Name
newtype VarMap = VarMap { unVarMap :: Map Serial (Maybe Var) }

mkVarMap shareds = VarMap $ Map.fromAscList $ map (id &&& const Nothing) shareds

type ToSource s a = RWST () [(Var, H.HsExp)] (Serial, VarMap) (ST s) a

ensureVar :: CNode s -> ToSource s H.HsExp -> ToSource s H.HsExp
ensureVar node@(CNode serial name _) f = do
  lookup <- gets $ Map.lookup serial . unVarMap . snd         
  case lookup of
    Nothing -> f
    Just Nothing -> do
      let insert v = VarMap . Map.insert serial (Just v) . unVarMap          
      var <- case name of
        Nothing -> do
          varNum <- gets fst
          let var = Name $ H.HsIdent $ "v" ++ show (unSerial varNum)
          modify $ (succ *** insert var)
          return var
        Just var -> do
          modify $ (second $ insert var)
          return var
      proj <- f
      tell [(var, proj)]
      return $ toHsVar var
    Just (Just var) -> return $ toHsVar var
  where toHsVar n = H.HsVar $ projectName n

scope f = do
  (expr, binds) <- censor (const mempty) $ listen f  
  return $ case binds of
    [] -> expr
    _ -> H.HsLet (map (uncurry toBind) binds) expr
    
  where toBind var val = H.HsPatBind noLoc (projectVar var) (H.HsUnGuardedRhs val) [] 
        projectVar (Name n) = H.HsPVar $ n

toSource :: CNode s -> ST s H.HsExp
toSource node = do
  shareds <- sharedNodes node  
  (src, []) <- evalRWST (scope $ projectNode node) () (firstSerial, mkVarMap shareds)
  return src

projectNode :: CNode s -> ToSource s H.HsExp
projectNode node@(CNode serial name refPayload) = ensureVar node $ do
  payload <- lift $ readSTRef refPayload
  projectPayload payload

toApp = foldl1 H.HsApp 

projectName (Name n) = H.UnQual n
projectName (Special s) = H.Special s

noLoc = error "No location"

ensureLen l es = es ++ replicate (l - length es) H.HsWildCard

projectPayload :: Payload (CNode s) -> ToSource s H.HsExp
projectPayload Uninitialized = return $ H.HsWildCard
projectPayload (ParamRef x) = return $ H.HsVar $ projectName x
projectPayload (IntLit n) = return $ H.HsLit $ H.HsInt n  
projectPayload (App e f) = do
  liftM2 H.HsApp (projectNode e) (projectNode f)
projectPayload (BuiltinFunApp op args) = do
  projectArgs <- mapM projectNode args
  let fun = case op of
        IntPlus -> H.HsVar (H.UnQual $ H.HsSymbol "+#")
        IntMinus -> H.HsVar (H.UnQual $ H.HsSymbol "-#")
  return $ H.HsParen $ toApp $ fun:projectArgs
projectPayload (ConApp c args) = do
  projectArgs <- mapM projectNode args
  let con = H.HsCon $ projectName c
  return $ toApp $ con:projectArgs
projectPayload (CaseApp arity alts args) = do
  projectAlts <- forM alts $ \ (Alt pats node) -> do
    body <- projectNode node
    return $ H.HsAlt noLoc (H.HsPTuple pats) (H.HsUnGuardedAlt body) []
  projectArgs <- mapM projectNode args  
  let expr = H.HsTuple $ ensureLen arity projectArgs
  return $ H.HsCase expr projectAlts
