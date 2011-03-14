{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Vis.CNode (CPayload, CNode(cnodeSerial, cnodeName), CThunk(..),
                  CNodeM, runCNodeM, 
                  readThunk, writeThunk, 
                  MonadCNode(..), liftST, mkCNode, mkCNode_, mkCNodeReentrant) 
       where

import Vis.Node

import Data.STRef
import Data.Function (on)
import Data.Ord (comparing)
import Control.Applicative
import Control.Monad.ST
import Control.Monad.State

type CPayload s name = Payload name (CNode s name)

-- A node in the cyclic representation
data CNode s name = CNode { cnodeSerial :: Serial, 
                            cnodeName :: Maybe name,
                            cnodeThunk :: STRef s (CThunk s name) }
               
data CThunk s name = CUpdatable (CPayload s name)
                   | CReentrant (CPayload s name)

instance Eq (CNode s name) where
  (==) = (==) `on` cnodeSerial

instance Ord (CNode s name) where
  compare = comparing cnodeSerial
                     

newtype CNodeM s a = CNodeM { unCNodeM :: StateT [Serial] (ST s) a }
                   deriving (Functor, Applicative, Monad)

runCNodeM :: CNodeM s a -> ST s a
runCNodeM f = evalStateT (unCNodeM f) [firstSerial..]

readThunk :: MonadCNode m s => CNode s name -> m (CThunk s name)
readThunk = liftST . readSTRef . cnodeThunk

writeThunk :: MonadCNode m s => CNode s name -> (CThunk s name) -> m ()
writeThunk node = liftST . writeSTRef (cnodeThunk node)

class (Monad m) => MonadCNode m s | m -> s where
  liftCNode :: CNodeM s a -> m a  
  
instance MonadCNode (CNodeM s) s where
  liftCNode = id
  
instance (MonadTrans t, Monad (t m), Monad m, MonadCNode m s) => MonadCNode (t m) s where
  liftCNode = lift . liftCNode

liftST :: MonadCNode m s => ST s a -> m a  
liftST = liftCNode . CNodeM . lift

mkCNodeI name thunk = do
  serial <- liftCNode nextSerial
  payload <- liftST (newSTRef thunk)
  return $ CNode serial name payload

mkCNode :: MonadCNode m s => Maybe name -> CPayload s name -> m (CNode s name)
mkCNode name p = mkCNodeI name (CUpdatable p)

mkCNode_ :: MonadCNode m s => Maybe name -> m (CNode s name)
mkCNode_ name = mkCNodeI name $ error "Untied knot"

mkCNodeReentrant name p = mkCNodeI name (CReentrant p)  

nextSerial :: CNodeM s Serial
nextSerial = CNodeM (gets head <* modify tail)
