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

newtype VarMap = VarMap { unVarMap :: Map Serial (Maybe FName) }

mkVarMap shareds = VarMap $ Map.fromAscList $ map (id &&& const Nothing) shareds

type ToSource s a = RWST () [(FName, FNode)] (Serial, VarMap) (ST s) a

ensureVar :: CNode s -> ToSource s FNode -> ToSource s FNode
ensureVar node@(CNode serial name _) f = do
  lookup <- gets $ Map.lookup serial . unVarMap . snd
  case lookup of
    Nothing -> f
    Just Nothing -> do
      let insert v = VarMap . Map.insert serial (Just v) . unVarMap          
      var <- case name of
        Nothing -> do
          varNum <- gets $ unSerial . fst
          let var = Generated $ "v" ++ show varNum
          modify $ (succ *** insert var)
          return var
        Just varName -> do
          let var = Given varName
          modify $ (second $ insert var)
          return var
      proj <- f
      tell [(var, proj)]
      return $ FVarRef var
    Just (Just var) -> return $ FVarRef var

scope f = do
  (expr, binds) <- censor (const mempty) $ listen f  
  return $ case binds of
    [] -> expr
    _ -> FLet (map (uncurry toBind) binds) expr    
  where toBind var val = Bind var [] val

flatten :: CNode s -> ST s FNode
flatten node = do
  shareds <- sharedNodes node 
  (src, []) <- evalRWST (scope $ flattenNode node) () (firstSerial, mkVarMap shareds)
  return src

flattenNode :: CNode s -> ToSource s FNode
flattenNode node@(CNode serial name refPayload) = ensureVar node $ do
  payload <- lift $ readSTRef refPayload
  liftM FNode $ flattenPayload payload

flattenPayload :: Payload (CNode s) -> ToSource s (Payload FNode)
flattenPayload (App e f) = liftM2 App (flattenNode e) (flattenNode f)
flattenPayload (BuiltinFunApp op args) = liftM (BuiltinFunApp op) $ mapM flattenNode args
flattenPayload (ConApp c args) = liftM (ConApp c) $ mapM flattenNode args
flattenPayload (CaseApp arity alts args) = liftM2 (CaseApp arity) (mapM flattenAlt alts) (mapM flattenNode args)
  where flattenAlt (Alt pats node) = liftM (Alt pats) $ flattenNode node
flattenPayload (Uninitialized) = return $ Uninitialized
flattenPayload (ParamRef x) = return $ ParamRef x
flattenPayload (IntLit n) = return $ IntLit n
