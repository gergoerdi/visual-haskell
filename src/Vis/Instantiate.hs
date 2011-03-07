module Vis.Instantiate (instantiate, FormalMap) where

import Vis.Node
import Vis.Monad
import Vis.FromSource

import Control.Applicative
import Data.STRef
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.RWS

type NodeMap s = Map (CNode s) (CNode s)
type FormalMap s = Map Name (CNode s)

type Cloner s a = RWST (FormalMap s) () (NodeMap s) (Vis s) a


withoutVars :: [Name] -> Cloner s a -> Cloner s a
withoutVars vars = local removeVars
  where removeVars formals = foldl (flip Map.delete) formals vars

instantiate :: FormalMap s -> CNode s -> Vis s (CNode s)
instantiate actuals node = fst <$> (evalRWST (cloneNode node) actuals mempty)

cloneNode :: CNode s -> Cloner s (CNode s)
cloneNode node = do
  payload <- lift $ readPayload node
  case payload of
    ParamRef x -> do
      actual <- asks (Map.lookup x)
      case actual of
        Just actual -> return actual
        Nothing -> cloneNode'
    CaseApp _ _ _ -> do
      return node
    _ -> cloneNode'
    
  where cloneNode' = do
          cloned <- gets (Map.lookup node)
          case cloned of 
            Just node' -> return node'
            Nothing -> do
              node' <- lift $ mkCNode_ $ cnodeName node
              modify (Map.insert node node')
              payload <- lift $ readPayload node
              payload' <- clonePayload payload
              lift $ writePayload node' payload'
              return node'
          

clonePayload :: Payload (CNode s) -> Cloner s (Payload (CNode s))
clonePayload Uninitialized = error "Consistency error: cloning an unfilled payload"
clonePayload (IntLit n) = return $ IntLit n
clonePayload (App e f) = App <$> cloneNode e <*> cloneNode f
clonePayload (BuiltinFunApp f args) = BuiltinFunApp f <$> mapM cloneNode args
clonePayload (CaseApp arity alts args) = error "Cloning a CaseApp"
clonePayload (ConApp c args) = ConApp c <$> mapM cloneNode args
clonePayload (ParamRef x) = return $ ParamRef x

cloneAlt (Alt pats body) = Alt pats <$> (withoutVars (concatMap bindsFromPat pats) $ cloneNode body)
