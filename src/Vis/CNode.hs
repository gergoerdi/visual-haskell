{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Vis.CNode (CPayload, CNode(cnodeSerial, cnodeName), CThunk(..),
                  CNodeM, runCNodeM, 
                  readThunk, writeThunk, 
                  MonadCNode(..), mkCNode, mkCNode_) 
       where

import Vis.Node

import Data.IORef
import Data.Function (on)
import Data.Ord (comparing)
import Control.Applicative
import Control.Monad.State

-- A node in the cyclic representation
data CNode name = CNode { cnodeSerial :: Serial, 
                          cnodeName :: Maybe name,
                          cnodeThunk :: IORef (CThunk name) }
               
data CThunk name = CThunk { cthunkReentrant :: Bool,
                              cthunkPayload :: CPayload name }

type CPayload name = Payload name (CNode name)

instance Eq (CNode name) where
  (==) = (==) `on` cnodeSerial

instance Ord (CNode name) where
  compare = compare `on` cnodeSerial
                     
newtype CNodeM a = CNodeM { unCNodeM :: StateT [Serial] IO a }
                 deriving (Functor, Applicative, Monad, MonadIO)

runCNodeM :: CNodeM a -> IO a
runCNodeM f = evalStateT (unCNodeM f) [firstSerial..]

class (Monad m) => MonadCNode m where
  liftCNode :: CNodeM a -> m a  
  
instance MonadCNode CNodeM where
  liftCNode = id
  
instance (MonadTrans t, Monad (t m), Monad m, MonadCNode m) => MonadCNode (t m) where
  liftCNode = lift . liftCNode

readThunk :: MonadCNode m => CNode name -> m (CThunk name)
readThunk = liftCNode . liftIO . readIORef . cnodeThunk

writeThunk :: MonadCNode m => CNode name -> CThunk name -> m ()
writeThunk node = liftCNode . liftIO . writeIORef (cnodeThunk node)

mkCNodeI :: MonadCNode m => Maybe name -> CThunk name -> m (CNode name)
mkCNodeI name thunk = do
  serial <- liftCNode nextSerial
  payload <- liftCNode $ liftIO (newIORef thunk)
  return $ CNode serial name payload

mkCNode :: MonadCNode m => Maybe name -> Bool -> CPayload name -> m (CNode name)
mkCNode name reentrant p = mkCNodeI name (CThunk reentrant p)

mkCNode_ :: MonadCNode m => Maybe name -> m (CNode name)
mkCNode_ name = mkCNodeI name $ error "Untied knot"

nextSerial :: CNodeM Serial
nextSerial = CNodeM (gets head <* modify tail)
