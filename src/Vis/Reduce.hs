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

import Debug.Trace

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

reduceBuiltinBinaryInt op node left right = do
  reduceFully left
  reduceFully right
  IntLit n <- readPayload left
  IntLit m <- readPayload right
  writePayload node $ IntLit $ n `op` m
  return True  

reduceBuiltin :: BuiltinFun -> Node s -> [Node s] -> Vis s Bool
reduceBuiltin IntPlus node [left, right] = reduceBuiltinBinaryInt (+) node left right
reduceBuiltin IntMinus node [left, right] = reduceBuiltinBinaryInt (+) node left right
reduceBuiltin _ _ _ = return False
                
reduceFully :: Node s -> Vis s ()    
reduceFully node = do
  reduced <- reduce node
  when reduced $ reduceFully node
    
reduce :: Node s -> Vis s Bool
reduce node = do
  payload <- readPayload node
  case payload of
    -- ConApp _ _ -> return False
    -- IntLit _ -> return False
    App f arg -> do
      reduce f
      payloadFun <- readPayload f
      case payloadFun of
        ConApp con args ->
          writePayload node $ ConApp con (snoc args arg)        
        BuiltinFunApp op args ->
          writePayload node $ BuiltinFunApp op (snoc args arg)
        SwitchApp arity matches args ->
          writePayload node $ SwitchApp arity matches (snoc args arg)          
      return True
    ParamRef x -> fail $ unwords ["Unfilled parameter:", show x]
    BuiltinFunApp op args -> reduceBuiltin op node args
    SwitchApp arity matches args | length args == arity -> do
      trace "reduce" $ return ()
      node' <- applySwitch matches args
      case node' of
        Nothing -> return False
        Just node' -> do
          writePayload node =<< readPayload node'
          return True
    _ -> return False

applySwitch :: [Match s] -> [Node s] -> Vis s (Maybe (Node s))
applySwitch matches args = do
  matchRes <- firstMatch matches args
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
          ConApp conActual nodes <- lift $ lift $ readPayload node
          guard $ conActual == con'
          zipWithM_ collect pats nodes
        collect (H.HsPList pats) node = collect (foldr cons nil pats) node
          where cons x xs = H.HsPApp (H.Special $ H.HsCons) [x, xs]
                nil =  H.HsPApp (H.Special $ H.HsListCon) []
        collect (H.HsPInfixApp left con right) node = collect (H.HsPApp con [left, right]) node
        collect (H.HsPAsPat x pat) node = do
          tell $ [(Name x, node)]
          collect pat node
        
