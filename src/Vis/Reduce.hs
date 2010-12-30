{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Vis.Reduce (reduce) where

import Vis.Node
import Vis.Monad
import Vis.Instantiate

import qualified Language.Haskell.Syntax as H
import Control.Applicative
import Control.Monad.Error
import Control.Monad.ST
import Data.STRef
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.RWS
import Control.Monad.Writer
import Data.Maybe
import Control.Monad.Trans.Maybe

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

reduce :: Node s -> Vis s ()
reduce node = do
  payload <- readPayload node
  case payload of
    PartialConApp _ _ -> return ()
    IntLit _ -> return ()
    App e f -> do
      reduce e
      payloadFun <- readPayload e
      case payloadFun of
        PartialConApp con ns ->
          writePayload node $ PartialConApp con (snoc ns f)
        PartialFunApp fun ns ->
          writePayload node $ PartialFunApp fun (snoc ns f)
      reduce node
    VarRef x -> do
      writePayload node $ PartialFunApp x []
    PartialFunApp fun actuals -> do
      case fun of
        H.HsSymbol "+" -> do
          when (length actuals == 2) $ do
            mapM_ reduce actuals
            let [left, right] = actuals
            IntLit n <- readPayload left
            IntLit m <- readPayload right
            writePayload node $ IntLit $ n + m
        _ -> do
          matches@((Match pats _):_) <- lookupMatches fun
          when (length pats == length actuals) $ do
            node' <- applyFunction matches actuals
            case node' of
              Nothing -> return ()
              Just node' -> do
                writePayload node =<< readPayload node'
                reduce node

applyFunction :: [Match s] -> [Node s] -> Vis s (Maybe (Node s))
applyFunction matches actuals = do
  matchRes <- firstMatch matches actuals
  case matchRes of
    Nothing -> return Nothing
    Just (formalMap, body) -> do
      Just <$> instantiate formalMap body

firstMatch :: [Match s] -> [Node s] -> Vis s (Maybe (FormalMap s, Node s))
firstMatch matches nodes = do
  getFirst <$> mconcat <$> 
    (forM matches $ \ (Match pats body) -> do              
        matchResult <- match pats nodes
        case matchResult of
          Nothing -> return $ First $ Nothing
          Just formalMap -> return $ First $ Just (formalMap, body))  

match :: [H.HsPat] -> [Node s] -> Vis s (Maybe (FormalMap s))
match pats ns = fmap (Map.fromList) <$> (runMaybeT $ execWriterT $ zipWithM_ collect pats ns)
  where collect :: H.HsPat -> Node s -> WriterT [(Name, Node s)] (MaybeT (Vis s)) ()
        collect (H.HsPVar x) node = tell $ [(x, node)]
        collect H.HsPWildCard node = return ()
        collect (H.HsPParen pat) node = collect pat node
        collect (H.HsPLit (H.HsInt n)) node = do 
          lift $ lift $ reduce node
          IntLit n' <- lift $ lift $ readPayload node
          guard $ n' == n
          return ()
        collect (H.HsPApp (H.UnQual con) pats) node = do
          lift $ lift $ reduce node
          PartialConApp con' nodes <- lift $ lift $ readPayload node
          guard $ con' == con
          zipWithM_ collect pats nodes
        collect (H.HsPAsPat x pat) node = do
          tell $ [(x, node)]
          collect pat node
        
