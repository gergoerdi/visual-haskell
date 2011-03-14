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

type NodeMap s name = Map (CNode s name) (CNode s name)
type FormalMap s name = Map name (CNode s name)

type Cloner s name a = RWST (FormalMap s name, Set (CNode s name)) () (NodeMap s name) (CNodeM s) a

withoutVars :: Ord name => [name] -> Cloner s name a -> Cloner s name a
withoutVars vars = local (first removeVars)
  where removeVars formals = foldl (flip Map.delete) formals vars

clone = instantiate mempty

instantiate :: Ord name => FormalMap s name -> CNode s name -> CNodeM s (CNode s name)
instantiate actuals node = fst <$> (evalRWST (cloneNode node) (actuals, mempty) (mempty))

cloneNode :: Ord name => CNode s name -> Cloner s name (CNode s name)
cloneNode (CReentrant node) = do
  reenter <- asks (Set.member node . snd)
  CReentrant <$> (if reenter then return node else local (second $ Set.insert node) (cloneNode node))
cloneNode node = do
  payload <- lift $ readPayload node
  case payload of
    ParamRef x -> do
      actual <- asks (Map.lookup x . fst)
      case actual of
        Just actual -> return actual
        Nothing -> cloneNode'
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
          

clonePayload :: Ord name => CPayload s name -> Cloner s name (CPayload s name)
clonePayload (Lambda pat node) = Lambda pat <$> cloneNode node
clonePayload (Lit n) = return $ Lit n
clonePayload (App e args) = App <$> cloneNode e <*> mapM cloneNode args
clonePayload (BuiltinFunApp f args) = BuiltinFunApp f <$> mapM cloneNode args
clonePayload (Case alts arg) = Case <$> mapM cloneAlt alts <*> cloneNode arg
clonePayload (ConApp c args) = ConApp c <$> mapM cloneNode args
clonePayload (ParamRef x) = return $ ParamRef x

cloneAlt (Alt pat body) = Alt pat <$> withoutVars vars (cloneNode body)
  where vars = execWriter $ varsOf pat
        varsOf (PConApp _ pats) = mapM_ varsOf pats
        varsOf (PVar x) = tell [x]
        varsOf (PAsPat x pat) = tell [x] >> varsOf pat
        varsOf _ = return ()
