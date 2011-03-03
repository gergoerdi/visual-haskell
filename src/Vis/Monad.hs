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

type FunctionMap s = Map Name [Alt s]
            
data R s = R { localVars :: Map Name (CNode s) }

newtype Vis s a = Vis { unVis :: RWST (R s) () Serial (ST s) a }
                deriving (Functor, Applicative, Monad)

runVis f = do (result, s', output) <- runRWST (unVis f) r s
              return result
  where r = R { localVars = mempty }
        s = firstSerial

liftST :: ST s a -> Vis s a
liftST = Vis . lift

readPayload :: CNode s -> Vis s (Payload (CNode s))
readPayload = liftST . readSTRef . cnodePayload

writePayload :: CNode s -> Payload (CNode s) -> Vis s ()
writePayload node = liftST . writeSTRef (cnodePayload node)

unsupported x = fail $ unwords ["Unsupported language feature", x]

mkCNode_ :: Maybe Name -> Vis s (CNode s)
mkCNode_ name = mkCNode name Uninitialized

mkCNode :: Maybe Name -> Payload (CNode s) -> Vis s (CNode s)
mkCNode name p = CNode <$> nextSerial <*> pure name <*> liftST (newSTRef p)

nextSerial :: Vis s Serial
nextSerial = Vis (get <* modify succ)

lookupBind :: Name -> Vis s (Maybe (CNode s))
lookupBind x = Vis $ asks (Map.lookup x . localVars)

withVars :: [Name] -> Vis s a -> Vis s a
withVars vars f = do
  newBinds <- forM vars $ \var -> do
    node <- mkCNode_ (Just var)
    return (var, node)
  let addVars r =  r{ localVars = Map.union (localVars r) (Map.fromList newBinds) }
  Vis $ local addVars $ unVis f

setVar :: Name -> CNode s -> Vis s ()
setVar x node = do
  payload <- liftST $ readSTRef $ cnodePayload node
  node' <- fromJust <$> lookupBind x
  writePayload node' payload
