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

data Match s = Match { matchPatterns :: [H.HsPat],
                       matchBody :: Node s }
               
type FunctionMap s = Map Name [Match s]
            
data R s = R { localVars :: Map Name (Node s), 
               functionMap :: FunctionMap s }

newtype Vis s a = Vis { unVis :: RWST (R s) () Int (ST s) a }
                deriving (Functor, Applicative, Monad)

runVis f = do (result, s', output) <- runRWST (unVis f) r s
              return result
  where r = R { localVars = mempty, 
                functionMap = mempty }
        s = 0

liftST :: ST s a -> Vis s a
liftST = Vis . lift

readPayload :: Node s -> Vis s (Payload s)
readPayload = liftST . readSTRef . nodePayload

writePayload :: Node s -> Payload s -> Vis s ()
writePayload node = liftST . writeSTRef (nodePayload node)

unsupported x = fail "Unsupported language feature"

mkNode_ :: Vis s (Node s)
mkNode_ = mkNode Uninitialized

mkNode :: Payload s -> Vis s (Node s)
mkNode p = Node <$> nextSerial <*> liftST (newSTRef p)

nextSerial :: Vis s Int
nextSerial = Vis (get <* modify succ)

lookupBind :: Name -> Vis s (Maybe (Node s))
lookupBind x = Vis $ asks (Map.lookup x . localVars)

lookupMatches :: Name -> Vis s [Match s]
lookupMatches f = Vis $ asks (fromJust . Map.lookup f . functionMap)

withVars :: [Name] -> Vis s a -> Vis s a
withVars vars f = do
  newBinds <- forM vars $ \var -> do
    node <- mkNode_
    return (var, node)
  let addVars r =  r{ localVars = Map.union (localVars r) (Map.fromList newBinds) }
  Vis $ local addVars $ unVis f

withFunctions :: FunctionMap s -> Vis s a -> Vis s a
withFunctions functionMap = Vis . local addFunctions . unVis
  where addFunctions r = r{ functionMap = functionMap } -- TODO
  
