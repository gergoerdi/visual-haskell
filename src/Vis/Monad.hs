{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Vis.Monad where

import Vis.Node

import qualified Language.Haskell.Syntax as H
import Control.Applicative
import Control.Monad.Error
import Control.Monad.ST
import Data.STRef
import Control.Monad.State
import Data.Maybe

newtype Vis s a = Vis { unVis :: StateT Serial (ST s) a }
                deriving (Functor, Applicative, Monad)

runVis f = evalStateT (unVis f) firstSerial

readPayload :: MonadCNode m s => CNode s name -> m (Payload name (CNode s name))
readPayload = liftST . readSTRef . cnodePayload

writePayload :: MonadCNode m s => CNode s name -> Payload name (CNode s name) -> m ()
writePayload node = liftST . writeSTRef (cnodePayload node)

unsupported x = fail $ unwords ["Unsupported language feature", x]

class (Monad m) => MonadCNode m s | m -> s where
  liftCNode :: Vis s a -> m a  
  
instance MonadCNode (Vis s) s where
  liftCNode = id
  
instance (MonadTrans t, Monad (t m), Monad m, MonadCNode m s) => MonadCNode (t m) s where
  liftCNode = lift . liftCNode

liftST :: MonadCNode m s => ST s a -> m a  
liftST = liftCNode . Vis . lift

mkCNode :: MonadCNode m s => Maybe name -> Payload name (CNode s name) -> m (CNode s name)
mkCNode name p = do
  serial <- liftCNode nextSerial
  payload <- liftST (newSTRef p)
  return $ CNode serial name payload

mkCNode_ :: MonadCNode m s => Maybe name -> m (CNode s name)
mkCNode_ name = mkCNode name $ error "Untied knot"


nextSerial :: Vis s Serial
nextSerial = Vis (get <* modify succ)
