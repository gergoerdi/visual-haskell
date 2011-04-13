{-# LANGUAGE PatternGuards #-}
module Vis.Flatten (flatten) where

import Vis.Node
import Vis.CNode

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.RWS
import Control.Monad.State (execStateT)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import RdrName
import PrelNames

data Occurance = Once
               | Many
               deriving (Eq, Show)

newtype SeenNodes = SeenNodes { unSeenNodes :: Map Serial Occurance }

sharedNodes :: CNode name -> CNodeM [Serial]
sharedNodes node = map fst <$> filter ((== Many) . snd) <$> Map.toAscList <$> 
                     unSeenNodes <$> execStateT (collectNode node) (SeenNodes mempty)
  where collectNode node = do
          let serial = cnodeSerial node
          lookup <- gets $ Map.lookup serial . unSeenNodes 
          case lookup of
            Nothing -> do
              thunk <- readThunk node
              modify $ SeenNodes . Map.insert serial Once . unSeenNodes
              collectPayload $ cthunkPayload thunk
            Just Once -> do
              modify $ SeenNodes . Map.insert serial Many . unSeenNodes
            Just Many -> return ()
        
        collectPayload (Lambda pat node) = collectNode node
        collectPayload (App e args) = collectNode e >> mapM_ collectNode args
        collectPayload (PrimApp _ args) = mapM_ collectNode args
        collectPayload (ConApp _ args) = mapM_ collectNode args
        collectPayload (Case e alts) = collectNode e >> mapM_ collectAlt alts
        collectPayload _ = return ()
        
        collectAlt (Alt _ body) = collectNode body

newtype VarMap = VarMap { unVarMap :: Map Serial (FName VarName) }

type ToSource a = RWST (Set Serial) [Bind VarName] VarMap CNodeM a

ensureVar :: CNode VarName -> ToSource (FNode VarName) -> ToSource (FNode VarName)
ensureVar node f | Just name <- externalName = return $ FVarRef (Given name)
  where externalName = do
          name <- cnodeName node
          (mod, _) <- isOrig_maybe name                    
          guard $ mod /= mAIN
          return name
ensureVar node f = do
  let serial = cnodeSerial node  
  lookup <- gets $ Map.lookup serial . unVarMap
  case lookup of
    Just var -> return $ FVarRef var
    Nothing -> do
      shared <- asks $ Set.member serial
      if not shared then f
        else do
          let insert v = VarMap . Map.insert serial v . unVarMap          
              var = case cnodeName node of
                Nothing -> Generated serial
                Just varName -> Given varName
          modify $ insert var
          proj <- f
          tell [Bind var proj]
          return $ FVarRef var

scope f = do
  (expr, binds) <- censor (const mempty) $ listen f  
  return $ case binds of
    [] -> expr
    _ -> FLet binds expr    

flatten :: CNode VarName -> CNodeM (FNode VarName)
flatten node = do
  shareds <- sharedNodes node 
  (src, []) <- evalRWST (scope $ flattenNode node) (Set.fromAscList shareds) (VarMap mempty)
  return src

flattenNode :: CNode VarName -> ToSource (FNode VarName)
flattenNode node = ensureVar node $ do
  FNode <$> (readThunk node >>= flattenThunk)
  
flattenThunk :: CThunk VarName -> ToSource (FPayload VarName)
flattenThunk = flattenPayload . cthunkPayload
  
flattenPayload :: CPayload VarName -> ToSource (FPayload VarName)
flattenPayload (Lambda pat node) = Lambda pat <$> flattenNode node
flattenPayload (App e args) = liftM2 App (flattenNode e) (mapM flattenNode args)
flattenPayload (BuiltinOp op) = return $ BuiltinOp op
flattenPayload (PrimApp op args) = liftM (PrimApp op) $ mapM flattenNode args
flattenPayload (ConApp c args) = liftM (ConApp c) $ mapM flattenNode args
flattenPayload (Case e alts) = liftM2 Case (flattenNode e) (mapM flattenAlt alts)
  where flattenAlt (Alt pats node) = liftM (Alt pats) $ flattenNode node
flattenPayload (ParamRef x) = return $ ParamRef x
flattenPayload (Literal lit) = return $ Literal lit
