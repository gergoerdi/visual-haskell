{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Vis.Reduce (reduceStep) where

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
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import Control.Monad.Trans.Maybe

newtype Reduce s a = Reduce { unReduce :: ReaderT () (Vis s) a }
                   deriving (Monad, Functor, Applicative)
                            
instance MonadCNode (Reduce s) s where                            
  liftCNode = Reduce . lift

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

reduceBuiltinBinaryInt op node left right = do
  reduceWHNF left
  reduceWHNF right
  IntLit n <- readPayload left
  IntLit m <- readPayload right
  writePayload node $ IntLit $ n `op` m
  return True  

reduceBuiltin :: BuiltinFun -> CNode s -> [CNode s] -> Vis s Bool
reduceBuiltin IntPlus node [left, right] = reduceBuiltinBinaryInt (+) node left right
reduceBuiltin IntMinus node [left, right] = reduceBuiltinBinaryInt (-) node left right
reduceBuiltin _ _ _ = return False
                
reduceWHNF :: CNode s -> Vis s ()    
reduceWHNF node = do
  changed <- reduceStep node
  when changed $ reduceWHNF node
    
reduceStep :: CNode s -> Vis s Bool
reduceStep node = do
  payload <- readPayload node
  case payload of    
    Knot node -> do
      node' <- clone node
      reduceStep node'
      overwriteWith node'      
      return True
    App f arg -> do
      reduceWHNF f            
      payloadFun <- readPayload f
      case payloadFun of
        ConApp con args -> writePayload node $ ConApp con (snoc args arg)
        BuiltinFunApp op args -> writePayload node $ BuiltinFunApp op (snoc args arg)
        Lambda pat body -> do
          Just formalMap <- match [pat] [arg]
          overwriteWith =<< instantiate formalMap body
      return True
    ParamRef x -> fail $ unwords ["Unfilled parameter:", show x]
    BuiltinFunApp op args -> reduceBuiltin op node args
    Case alts args -> do
      node' <- applyCase alts args
      case node' of
        Nothing -> return False
        Just node' -> overwriteWith node' >> return True
    _ -> return False    
    
  where overwriteWith node' = writePayload node =<< readPayload node'

applyCase :: [Alt (CNode s)] -> [CNode s] -> Vis s (Maybe (CNode s))
applyCase alts args = do
  match <- firstMatch alts args
  case match of
    Nothing -> return Nothing
    Just (formalMap, body) -> Just <$> instantiate formalMap body

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
          (lift . lift) $ reduceWHNF node
          IntLit n' <- (lift . lift) $ readPayload node
          guard $ n' == n
        collect (H.HsPApp con pats) node = do          
          con' <- fromName con
          (lift . lift) $ reduceWHNF node
          ConApp conActual nodes <- (lift . lift) $ readPayload node
          guard $ conActual == con'
          zipWithM_ collect pats nodes
        collect (H.HsPTuple pats) node = do
          (lift . lift) $ reduceWHNF node
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
        
