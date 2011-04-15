{-# LANGUAGE NoMonomorphismRestriction #-}
module Vis.Instantiate (clone, instantiate, FormalMap) where

import Vis.Node
import Vis.CNode

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.RWS
import Control.Monad.Writer
import Control.Arrow (first, second, (***))

type NodeMap = Map CNode CNode
type FormalMap = Map VarName CNode

type Cloner a = RWST (FormalMap, Set CNode) () NodeMap CNodeM a

withoutVars :: [VarName] -> Cloner a -> Cloner a
withoutVars vars = local (first removeVars)
  where removeVars formals = foldl (flip Map.delete) formals vars

clone = instantiate mempty

instantiate :: FormalMap -> CNode -> CNodeM CNode
instantiate actuals node = fst <$> (evalRWST (cloneNode node) (actuals, mempty) (mempty))

cloneNode :: CNode -> Cloner CNode
cloneNode node = do
  thunk <- lift $ readThunk node
  if cthunkReentrant thunk
    then return node
    else
      case cthunkPayload thunk of
        ParamRef x -> do
          actual <- asks (Map.lookup x . fst)
          case actual of
            Just actual -> return actual
            Nothing -> cloneNode' thunk
        _ -> cloneNode' thunk
    
  where cloneNode' thunk = do
          cloned <- gets (Map.lookup node)
          case cloned of 
            Just node' -> return node'
            Nothing -> do
              node' <- lift $ mkCNode_ $ (if cthunkReentrant thunk then Nothing else cnodeName node)
              modify (Map.insert node node')              
              payload' <- clonePayload (cthunkPayload thunk)
              lift $ writeThunk node' $ CThunk False payload'
              return node'                    

clonePayload :: CPayload -> Cloner CPayload
clonePayload (Lambda pat node) = Lambda pat <$> cloneNode node
clonePayload p@(Literal lit) = return p
clonePayload (App e args) = App <$> cloneNode e <*> mapM cloneNode args
clonePayload (BuiltinOp op) = return $ BuiltinOp op
clonePayload (PrimApp op args) = PrimApp op <$> mapM cloneNode args
clonePayload (Case expr alts) = Case <$> cloneNode expr <*> mapM cloneAlt alts
clonePayload (ConApp c args) = ConApp c <$> mapM cloneNode args
clonePayload p@(ParamRef x) = return p

cloneAlt (Alt pat body) = Alt pat <$> withoutVars vars (cloneNode body)
  where vars = execWriter $ varsOf pat
        varsOf (PConApp _ pats) = mapM_ varsOf pats
        varsOf (PVar x) = tell [x]
        varsOf (PAsPat x pat) = tell [x] >> varsOf pat
        varsOf _ = return ()
