{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Vis.CNode (CPayload, CNode(CKnot, cnodeSerial, cnodeName), 
                  CNodeM, runCNodeM, 
                  readPayload, writePayload, 
                  MonadCNode(..), liftST, mkCNode, mkCNode_) 
       where

import Vis.Node

import Data.STRef
import Data.Function (on)
import Control.Applicative
import Control.Monad.ST
import Control.Monad.State

type CPayload s name = Payload name (CNode s name)

-- A node in the cyclic representation
data CNode s name = CNode { cnodeSerial :: Serial, 
                            cnodeName :: Maybe name,
                            cnodePayload :: STRef s (CPayload s name) }
                  | CKnot (CNode s name)
               
instance Eq (CNode s name) where
  (==) = (==) `on` cnodeSerial

instance Ord (CNode s name) where
  compare = compare `on` cnodeSerial
                     

newtype CNodeM s a = CNodeM { unCNodeM :: StateT [Serial] (ST s) a }
                   deriving (Functor, Applicative, Monad)

runCNodeM :: CNodeM s a -> ST s a
runCNodeM f = evalStateT (unCNodeM f) [firstSerial..]

readPayload :: MonadCNode m s => CNode s name -> m (Payload name (CNode s name))
readPayload = liftST . readSTRef . cnodePayload

writePayload :: MonadCNode m s => CNode s name -> Payload name (CNode s name) -> m ()
writePayload node = liftST . writeSTRef (cnodePayload node)

class (Monad m) => MonadCNode m s | m -> s where
  liftCNode :: CNodeM s a -> m a  
  
instance MonadCNode (CNodeM s) s where
  liftCNode = id
  
instance (MonadTrans t, Monad (t m), Monad m, MonadCNode m s) => MonadCNode (t m) s where
  liftCNode = lift . liftCNode

liftST :: MonadCNode m s => ST s a -> m a  
liftST = liftCNode . CNodeM . lift

mkCNode :: MonadCNode m s => Maybe name -> Payload name (CNode s name) -> m (CNode s name)
mkCNode name p = do
  serial <- liftCNode nextSerial
  payload <- liftST (newSTRef p)
  return $ CNode serial name payload

mkCNode_ :: MonadCNode m s => Maybe name -> m (CNode s name)
mkCNode_ name = mkCNode name $ error "Untied knot"

nextSerial :: CNodeM s Serial
nextSerial = CNodeM (gets head <* modify tail)
