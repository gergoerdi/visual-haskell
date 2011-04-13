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

type NodeMap name = Map (CNode name) (CNode name)
type FormalMap name = Map name (CNode name)

type Cloner name a = RWST (FormalMap name, Set (CNode name)) () (NodeMap name) CNodeM a

withoutVars :: Ord name => [name] -> Cloner name a -> Cloner name a
withoutVars vars = local (first removeVars)
  where removeVars formals = foldl (flip Map.delete) formals vars

clone = instantiate mempty

instantiate :: Ord name => FormalMap name -> CNode name -> CNodeM (CNode name)
instantiate actuals node = fst <$> (evalRWST (cloneNode node) (actuals, mempty) (mempty))

cloneNode :: Ord name => CNode name -> Cloner name (CNode name)
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

clonePayload :: Ord name => CPayload name -> Cloner name (CPayload name)
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
