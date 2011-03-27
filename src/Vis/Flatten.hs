module Vis.Flatten (flatten) where

import Vis.Node
import Vis.CNode

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State (execStateT)
import Data.Map (Map)
import qualified Data.Map as Map

newtype SeenNodes = SeenNodes { unSeenNodes :: Map Serial Bool }

sharedNodes :: CNode s name -> CNodeM s [Serial]
sharedNodes node = map fst <$> filter snd <$> Map.toAscList <$> 
                     unSeenNodes <$> execStateT (collectNode node) (SeenNodes mempty)
  where collectNode node = do
          let serial = cnodeSerial node
          lookup <- gets $ Map.lookup serial . unSeenNodes 
          case lookup of
            Nothing -> do
              thunk <- readThunk node
              modify $ SeenNodes . Map.insert serial (False && cthunkReentrant thunk) . unSeenNodes
              collectPayload $ cthunkPayload thunk
            Just False -> do
              modify $ SeenNodes . Map.insert serial True . unSeenNodes
            Just True -> return ()
        
        collectPayload (Lambda pat node) = collectNode node
        collectPayload (App e args) = collectNode e >> mapM_ collectNode args
        collectPayload (PrimApp _ args) = mapM_ collectNode args
        collectPayload (ConApp _ args) = mapM_ collectNode args
        collectPayload (Case _ e alts) = collectNode e >> mapM_ collectAlt alts
        collectPayload _ = return ()
        
        collectAlt (Alt _ body) = collectNode body

newtype VarMap name = VarMap { unVarMap :: Map Serial (Maybe (FName name)) }

mkVarMap shareds = VarMap $ Map.fromAscList $ map (id &&& const Nothing) shareds

type ToSource s name a = RWST () [(FName name, FNode name)] (VarMap name) (CNodeM s) a

ensureVar :: CNode s name -> ToSource s name (FNode name) -> ToSource s name (FNode name)
ensureVar node f = do
  let serial = cnodeSerial node
  lookup <- gets $ Map.lookup serial . unVarMap
  case lookup of
    Nothing -> f
    Just Nothing -> do
      let insert v = VarMap . Map.insert serial (Just v) . unVarMap          
      var <- case cnodeName node of
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

flatten :: CNode s name -> CNodeM s (FNode name)
flatten node = do
  shareds <- sharedNodes node 
  (src, []) <- evalRWST (scope $ flattenNode node) () (mkVarMap shareds)
  return src

flattenNode :: CNode s name -> ToSource s name (FNode name)
flattenNode node = ensureVar node $ do
  FNode <$> (readThunk node >>= flattenThunk)
  
flattenThunk :: CThunk s name -> ToSource s name (FPayload name)
flattenThunk = flattenPayload . cthunkPayload
  
flattenPayload :: Payload name (CNode s name) -> ToSource s name (FPayload name)
flattenPayload (Lambda pat node) = Lambda pat <$> flattenNode node
flattenPayload (App e args) = liftM2 App (flattenNode e) (mapM flattenNode args)
flattenPayload (PrimApp op args) = liftM (PrimApp op) $ mapM flattenNode args
flattenPayload (ConApp c args) = liftM (ConApp c) $ mapM flattenNode args
flattenPayload (Case x e alts) = liftM2 (Case x) (flattenNode e) (mapM flattenAlt alts)
  where flattenAlt (Alt pats node) = liftM (Alt pats) $ flattenNode node
flattenPayload (ParamRef x) = return $ ParamRef x
flattenPayload (Literal lit) = return $ Literal lit
