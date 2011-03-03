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

reduceBuiltinBinaryInt op node left right = do
  reduceFully left
  reduceFully right
  IntLit n <- readPayload left
  IntLit m <- readPayload right
  writePayload node $ IntLit $ n `op` m
  return True  

reduceBuiltin :: BuiltinFun -> CNode s -> [CNode s] -> Vis s Bool
reduceBuiltin IntPlus node [left, right] = reduceBuiltinBinaryInt (+) node left right
reduceBuiltin IntMinus node [left, right] = reduceBuiltinBinaryInt (-) node left right
reduceBuiltin _ _ _ = return False
                
reduceFully :: CNode s -> Vis s ()    
reduceFully node = do
  reduced <- reduce node
  when reduced $ reduceFully node
    
reduce :: CNode s -> Vis s Bool
reduce node = do
  payload <- readPayload node
  case payload of
    -- ConApp _ _ -> return False
    -- IntLit _ -> return False
    App f arg -> do
      reduceFully f
      payloadFun <- readPayload f
      case payloadFun of
        ConApp con args ->
          writePayload node $ ConApp con (snoc args arg)        
        BuiltinFunApp op args ->
          writePayload node $ BuiltinFunApp op (snoc args arg)
        CaseApp arity matches args ->
          writePayload node $ CaseApp arity matches (snoc args arg)          
      return True
    ParamRef x -> fail $ unwords ["Unfilled parameter:", show x]
    BuiltinFunApp op args -> reduceBuiltin op node args
    CaseApp arity alts args | length args == arity -> do
      node' <- applyCase alts args
      case node' of
        Nothing -> return False
        Just node' -> do
          writePayload node =<< readPayload node'
          return True
    _ -> return False    

applyCase :: [Alt (CNode s)] -> [CNode s] -> Vis s (Maybe (CNode s))
applyCase alts args = do
  match <- firstMatch alts args
  case match of
    Nothing -> return Nothing
    Just (formalMap, body) -> do
      Just <$> instantiate formalMap body

firstMatch :: [Alt (CNode s)] -> [CNode s] -> Vis s (Maybe (FormalMap s, CNode s))
firstMatch alts nodes = do
  getFirst <$> mconcat <$> 
    (forM alts $ \ (Alt pats body) -> do              
        match <- match pats nodes
        case match of
          Nothing -> return $ First $ Nothing
          Just formalMap -> return $ First $ Just (formalMap, body))  

match :: [H.HsPat] -> [CNode s] -> Vis s (Maybe (FormalMap s))
match pats ns = fmap (Map.fromList) <$> (runMaybeT $ execWriterT $ zipWithM_ collect pats ns)
  where collect :: H.HsPat -> CNode s -> WriterT [(Name, CNode s)] (MaybeT (Vis s)) ()
        collect (H.HsPVar x) node = tell $ [(Name x, node)]
        collect H.HsPWildCard node = return ()
        collect (H.HsPParen pat) node = collect pat node
        collect (H.HsPLit (H.HsInt n)) node = do 
          (lift . lift) $ reduce node
          IntLit n' <- (lift . lift) $ readPayload node
          guard $ n' == n
          return ()
        collect (H.HsPApp con pats) node = do          
          con' <- fromName con
          (lift . lift) $ reduce node
          ConApp conActual nodes <- (lift . lift) $ readPayload node
          guard $ conActual == con'
          zipWithM_ collect pats nodes
        collect (H.HsPTuple pats) node = do
          (lift . lift) $ reduceFully node
          ConApp conActual nodes <- (lift . lift) $ readPayload node
          guard $ conActual == Special (H.HsTupleCon (length pats))
          zipWithM_ collect pats nodes          
        collect (H.HsPList pats) node = collect (foldr cons nil pats) node
          where cons x xs = H.HsPApp (H.Special $ H.HsCons) [x, xs]
                nil =  H.HsPApp (H.Special $ H.HsListCon) []
        collect (H.HsPInfixApp left con right) node = collect (H.HsPApp con [left, right]) node
        collect (H.HsPAsPat x pat) node = do
          tell $ [(Name x, node)]
          collect pat node
        
