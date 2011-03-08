{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
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

readPayload :: MonadCNode m s => CNode s -> m (Payload (CNode s))
readPayload = liftST . readSTRef . cnodePayload

writePayload :: MonadCNode m s => CNode s -> Payload (CNode s) -> m ()
writePayload node = liftST . writeSTRef (cnodePayload node)

unsupported x = fail $ unwords ["Unsupported language feature", x]

class (Applicative m, Monad m) => MonadCNode m s | m -> s where
  liftCNode :: Vis s a -> m a  
  
instance MonadCNode (Vis s) s where
  liftCNode = id
  
liftST :: MonadCNode m s => ST s a -> m a  
liftST = liftCNode . Vis . lift

mkCNode :: MonadCNode m s => Maybe Name -> Payload (CNode s) -> m (CNode s)
mkCNode name p = CNode <$> liftCNode nextSerial <*> pure name <*> liftST (newSTRef p)

mkCNode_ :: MonadCNode m s => Maybe Name -> m (CNode s)
mkCNode_ name = mkCNode name Uninitialized


nextSerial :: Vis s Serial
nextSerial = Vis (get <* modify succ)
