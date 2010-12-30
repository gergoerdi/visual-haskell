module Vis.Instantiate (instantiate, FormalMap) where

import Vis.Node
import Vis.Monad

import Control.Applicative
import Control.Monad.Error
import Data.STRef
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.RWS

type NodeMap s = Map (Node s) (Node s)
type FormalMap s = Map Name (Node s)

type Cloner s a = RWST (FormalMap s) () (NodeMap s) (Vis s) a

instantiate :: FormalMap s -> Node s -> Vis s (Node s)
instantiate actuals node = fst <$> (evalRWST (cloneNode node) actuals mempty)

cloneNode :: Node s -> Cloner s (Node s)
cloneNode node = do
  payload <- lift $ readPayload node
  case payload of
    VarRef x -> do
      actual <- asks (Map.lookup x)
      case actual of
        Just actual -> return actual
        Nothing -> cloneNode'
    _ -> cloneNode'
    
  where cloneNode' = do
          cloned <- gets (Map.lookup node)
          case cloned of 
            Just node' -> return node'
            Nothing -> do
              payload <- lift $ readPayload node
              node' <- lift $ mkNode_
              modify (Map.insert node node')
              payload' <- clonePayload payload
              lift $ writePayload node' payload'
              return node'
          

clonePayload :: Payload s -> Cloner s (Payload s)
clonePayload Uninitialized = error "Consistency error: cloning an unfilled payload"
clonePayload (IntLit n) = return $ IntLit n
clonePayload (App e f) = App <$> cloneNode e <*> cloneNode f
clonePayload (PartialFunApp f ns) = PartialFunApp f <$> mapM cloneNode ns
clonePayload (PartialConApp c ns) = PartialConApp c <$> mapM cloneNode ns
clonePayload (VarRef x) = return $ VarRef x