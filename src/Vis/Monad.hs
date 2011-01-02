{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Vis.Monad where

import Vis.Node

import qualified Language.Haskell.Syntax as H
import Control.Applicative
import Control.Monad.Error
import Control.Monad.ST
import Data.STRef
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.RWS
import Data.Maybe

type FunctionMap s = Map Name [Match s]
            
data R s = R { localVars :: Map Name (Node s) }

newtype Vis s a = Vis { unVis :: RWST (R s) () Int (ST s) a }
                deriving (Functor, Applicative, Monad)

runVis f = do (result, s', output) <- runRWST (unVis f) r s
              return result
  where r = R { localVars = mempty }
        s = 0

liftST :: ST s a -> Vis s a
liftST = Vis . lift

readPayload :: Node s -> Vis s (Payload s)
readPayload = liftST . readSTRef . nodePayload

writePayload :: Node s -> Payload s -> Vis s ()
writePayload node = liftST . writeSTRef (nodePayload node)

unsupported x = fail $ unwords ["Unsupported language feature", x]

mkNode_ :: Vis s (Node s)
mkNode_ = mkNode Uninitialized

mkNode :: Payload s -> Vis s (Node s)
mkNode p = Node <$> nextSerial <*> liftST (newSTRef p)

nextSerial :: Vis s Int
nextSerial = Vis (get <* modify succ)

lookupBind :: Name -> Vis s (Maybe (Node s))
lookupBind x = Vis $ asks (Map.lookup x . localVars)

withVars :: [Name] -> Vis s a -> Vis s a
withVars vars f = do
  newBinds <- forM vars $ \var -> do
    node <- mkNode_
    return (var, node)
  let addVars r =  r{ localVars = Map.union (localVars r) (Map.fromList newBinds) }
  Vis $ local addVars $ unVis f

setVar :: Name -> Node s -> Vis s ()
setVar x node = do
  payload <- liftST $ readSTRef $ nodePayload node
  node' <- fromJust <$> lookupBind x
  writePayload node' payload
