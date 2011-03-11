module Vis.Flatten (flatten) where

import Vis.Node

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

sharedNodes :: CNode s name -> ST s [Serial]
sharedNodes node = map fst <$> filter snd <$> Map.toAscList <$> 
                     unSeenNodes <$> execStateT (collectNode node) (SeenNodes mempty)
  where collectNode = collectNode' False
                      
        collectNode' force (CNode serial name refPayload) = do
          lookup <- gets $ Map.lookup serial . unSeenNodes 
          case lookup of
            Nothing -> do
              modify $ SeenNodes . Map.insert serial force . unSeenNodes
              payload <- lift $ readSTRef refPayload
              collectPayload payload
            Just False -> do
              modify $ SeenNodes . Map.insert serial True . unSeenNodes
            Just True -> return ()
        
        collectPayload (Knot node) = collectNode' True node
        collectPayload (Lambda pat node) = collectNode node
        collectPayload (App e args) = collectNode e >> mapM_ collectNode args
        collectPayload (BuiltinFunApp _ args) = mapM_ collectNode args
        collectPayload (ConApp _ args) = mapM_ collectNode args
        collectPayload (Case alts args) = mapM_ collectAlt alts >> mapM_ collectNode args        
        collectPayload _ = return ()
        
        collectAlt (Alt pats body) = collectNode body

newtype VarMap name = VarMap { unVarMap :: Map Serial (Maybe (FName name)) }

mkVarMap shareds = VarMap $ Map.fromAscList $ map (id &&& const Nothing) shareds

type ToSource s name a = RWST () [(FName name, FNode name)] (VarMap name) (ST s) a

ensureVar :: CNode s name -> ToSource s name (FNode name) -> ToSource s name (FNode name)
ensureVar node@(CNode serial name _) f = do
  lookup <- gets $ Map.lookup serial . unVarMap
  case lookup of
    Nothing -> f
    Just Nothing -> do
      let insert v = VarMap . Map.insert serial (Just v) . unVarMap          
      var <- case name of
        Nothing -> do
          let var = Generated serial
          modify $ insert var
          return var
        Just varName -> do
          let var = Given varName
          modify $ insert var
          return var
      proj <- f
      tell [(var, proj)]
      return $ FVarRef var
    Just (Just var) -> return $ FVarRef var

scope f = do
  (expr, binds) <- censor (const mempty) $ listen f  
  return $ case binds of
    [] -> expr
    _ -> FLet (map (uncurry Bind) binds) expr    

flatten :: CNode s name -> ST s (FNode name)
flatten node = do
  shareds <- sharedNodes node 
  (src, []) <- evalRWST (scope $ flattenNode node) () (mkVarMap shareds)
  return src

flattenNode :: CNode s name -> ToSource s name (FNode name)
flattenNode node@(CNode serial name refPayload) = ensureVar node $ do
  payload <- lift $ readSTRef refPayload
  liftM FNode $ flattenPayload payload

flattenPayload :: Payload name (CNode s name) -> ToSource s name (FPayload name)
flattenPayload (Knot node) = Knot <$> flattenNode node
flattenPayload (Lambda pat node) = Lambda pat <$> flattenNode node
flattenPayload (App e args) = liftM2 App (flattenNode e) (mapM flattenNode args)
flattenPayload (BuiltinFunApp op args) = liftM (BuiltinFunApp op) $ mapM flattenNode args
flattenPayload (ConApp c args) = liftM (ConApp c) $ mapM flattenNode args
flattenPayload (Case alts args) = liftM2 Case (mapM flattenAlt alts) (mapM flattenNode args)
  where flattenAlt (Alt pats node) = liftM (Alt pats) $ flattenNode node
flattenPayload (Uninitialized) = return $ Uninitialized
flattenPayload (ParamRef x) = return $ ParamRef x
flattenPayload (Lit l) = return $ Lit l
