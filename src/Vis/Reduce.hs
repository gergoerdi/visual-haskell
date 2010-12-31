{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Vis.Reduce (reduce) where

import Vis.Node
import Vis.Monad
import Vis.Instantiate
import Vis.FromSource

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

reduceBuiltin :: BuiltinFun -> Node s -> [Node s] -> Vis s ()
reduceBuiltin IntPlus node actuals = 
  when (length actuals == 2) $ do
    mapM_ reduce actuals
    let [left, right] = actuals
    IntLit n <- readPayload left
    IntLit m <- readPayload right
    writePayload node $ IntLit $ n + m

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
    ParamRef x -> fail $ unwords ["Unfilled parameter:", show x]
    PartialFunApp fun actuals -> do
      case fun of
        BuiltinFun op -> reduceBuiltin op node actuals
        Matches f arity -> do
          when (length actuals == arity) $ do
            matches <- fromJust <$> lookupMatches f
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
        collect (H.HsPVar x) node = tell $ [(Name x, node)]
        collect H.HsPWildCard node = return ()
        collect (H.HsPParen pat) node = collect pat node
        collect (H.HsPLit (H.HsInt n)) node = do 
          lift $ lift $ reduce node
          IntLit n' <- lift $ lift $ readPayload node
          guard $ n' == n
          return ()
        collect (H.HsPApp con pats) node = do          
          con' <- fromName con
          lift $ lift $ reduce node
          PartialConApp conActual nodes <- lift $ lift $ readPayload node
          guard $ conActual == con'
          zipWithM_ collect pats nodes
        collect (H.HsPList pats) node = collect (foldr cons nil pats) node
          where cons x xs = H.HsPApp (H.Special $ H.HsCons) [x, xs]
                nil =  H.HsPApp (H.Special $ H.HsListCon) []
        collect (H.HsPInfixApp left con right) node = collect (H.HsPApp con [left, right]) node
        collect (H.HsPAsPat x pat) node = do
          tell $ [(Name x, node)]
          collect pat node
        
