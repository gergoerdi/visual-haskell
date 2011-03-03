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
    _ -> cloneNode'
    
  where cloneNode' = do
          cloned <- gets (Map.lookup node)
          case cloned of 
            Just node' -> return node'
            Nothing -> do
              payload <- lift $ readPayload node
              node' <- lift $ mkCNode_ Nothing
              modify (Map.insert node node')
              payload' <- clonePayload payload
              lift $ writePayload node' payload'
              return node'
          

clonePayload :: Payload (CNode s) -> Cloner s (Payload (CNode s))
clonePayload Uninitialized = error "Consistency error: cloning an unfilled payload"
clonePayload (IntLit n) = return $ IntLit n
clonePayload (App e f) = App <$> cloneNode e <*> cloneNode f
clonePayload (BuiltinFunApp f nodes) = BuiltinFunApp f <$> mapM cloneNode nodes
clonePayload (CaseApp arity alts actuals) = CaseApp arity <$> mapM cloneAlt alts <*> mapM cloneNode actuals
clonePayload (ConApp c nodes) = ConApp c <$> mapM cloneNode nodes
clonePayload (ParamRef x) = return $ ParamRef x

cloneAlt (Alt pats body) = withoutVars (concatMap bindsFromPat pats) $ do 
  Alt pats <$> cloneNode body
