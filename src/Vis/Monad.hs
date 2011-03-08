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

liftST :: ST s a -> Vis s a
liftST = Vis . lift

readPayload :: CNode s -> Vis s (Payload (CNode s))
readPayload = liftST . readSTRef . cnodePayload

writePayload :: CNode s -> Payload (CNode s) -> Vis s ()
writePayload node = liftST . writeSTRef (cnodePayload node)

unsupported x = fail $ unwords ["Unsupported language feature", x]

class MonadCNode m s | m -> s where
  mkCNode :: Maybe Name -> Payload (CNode s) -> m (CNode s)
  mkCNode_ :: Maybe Name -> m (CNode s)
  mkCNode_ name = mkCNode name Uninitialized

instance MonadCNode (Vis s) s where
  mkCNode name p = CNode <$> nextSerial <*> pure name <*> liftST (newSTRef p)

nextSerial :: Vis s Serial
nextSerial = Vis (get <* modify succ)
