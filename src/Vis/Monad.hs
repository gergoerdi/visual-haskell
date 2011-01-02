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
            
data R s = R { localVars :: Map Name (Node s), 
               functionMap :: FunctionMap s, 
               forwardMap :: Map Name Int}

newtype Vis s a = Vis { unVis :: RWST (R s) () Int (ST s) a }
                deriving (Functor, Applicative, Monad)

runVis f = do (result, s', output) <- runRWST (unVis f) r s
              return result
  where r = R { localVars = mempty, 
                functionMap = mempty, 
                forwardMap = mempty}
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

-- lookupMatches :: Name -> Vis s (Maybe [Match s])
-- lookupMatches f = Vis $ asks (Map.lookup f . functionMap)

-- lookupArity :: Name -> Vis s (Maybe Int)
-- lookupArity f = Vis $ do
--   matches <- asks (Map.lookup f . functionMap)
--   case matches of
--     Just ((Match pats _):_) -> return $ Just $ length pats
--     Nothing -> do
--       forward <- asks (Map.lookup f . forwardMap)
--       case forward of
--         Just arity -> return $ Just arity
--         Nothing -> return Nothing        

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


-- withFunctionForwards :: [(Name, Int)] -> Vis s a -> Vis s a
-- withFunctionForwards forwards = Vis . local addForwards . unVis
--   where addForwards r = r{ forwardMap = Map.union (forwardMap r) (Map.fromList forwards) }

-- withFunctions :: FunctionMap s -> Vis s a -> Vis s a
-- withFunctions functions = Vis . local addFunctions . unVis
--   where addFunctions r = r{ functionMap = Map.union (functionMap r) functions }
  
