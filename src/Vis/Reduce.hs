{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Vis.Reduce (reduceStep) where

import Vis.Node
import Vis.CNode
import Vis.Instantiate

import Control.Applicative
import Control.Monad.Error
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Maybe

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

reduceBuiltinBinaryInt op node left right = do
  Lit (IntLit n) <- reduceWHNF left
  Lit (IntLit m) <- reduceWHNF right
  writePayload node $ Lit $ IntLit $ n `op` m
  return True  

reduceBuiltin :: Ord name => BuiltinFun -> CNode s name -> [CNode s name] -> CNodeM s Bool
reduceBuiltin IntPlus node [left, right] = reduceBuiltinBinaryInt (+) node left right
reduceBuiltin IntMinus node [left, right] = reduceBuiltinBinaryInt (-) node left right
reduceBuiltin _ _ _ = return False
                
reduceWHNF :: Ord name => CNode s name -> CNodeM s (CPayload s name)
reduceWHNF node = do
  changed <- reduceStep node
  if changed then reduceWHNF node else readPayload node
  
    
reduceStep :: Ord name => CNode s name -> CNodeM s Bool
reduceStep node = do
  payload <- readPayload node
  case payload of    
    -- Knot node -> do
    --   node' <- clone node
    --   reduceStep node'
    --   overwriteWith node'      
    --   return True
    -- App f arg -> do
    --   payloadFun <- reduceWHNF f            
    --   case payloadFun of
    --     ConApp con args -> writePayload node $ ConApp con (snoc args arg)
    --     BuiltinFunApp op args -> writePayload node $ BuiltinFunApp op (snoc args arg)
    --     Lambda pat body -> do
    --       Just formalMap <- match [pat] [arg]
    --       overwriteWith =<< instantiate formalMap body
    --   return True
    -- ParamRef x -> fail $ unwords ["Unfilled parameter:", show x]
    BuiltinFunApp op args -> reduceBuiltin op node args
    Case alts arg -> do
      node' <- applyCase alts arg
      case node' of
        Nothing -> return False
        Just node' -> overwriteWith node' >> return True
    _ -> return False    
    
  where overwriteWith node' = writePayload node =<< readPayload node'

applyCase :: Ord name => [Alt name (CNode s name)] -> CNode s name -> CNodeM s (Maybe (CNode s name))
applyCase alts arg = do
  match <- firstMatch alts arg
  case match of
    Nothing -> return Nothing
    Just (formalMap, body) -> Just <$> instantiate formalMap body

firstMatch :: Ord name => [Alt name (CNode s name)] -> CNode s name -> CNodeM s (Maybe (FormalMap s name, CNode s name))
firstMatch alts arg = do
  getFirst <$> mconcat <$> 
    (forM alts $ \ (Alt pat body) -> do              
        match <- match pat arg
        case match of
          Nothing -> return $ First $ Nothing
          Just formalMap -> return $ First $ Just (formalMap, body))  

match :: Ord name => Pat name -> CNode s name -> CNodeM s (Maybe (FormalMap s name))
match pat arg = fmap (Map.fromList) <$> (runMaybeT $ execWriterT $ collect pat arg)
  where collect :: Ord name => Pat name -> CNode s name -> WriterT [(name, CNode s name)] (MaybeT (CNodeM s)) ()
        collect PWildcard node = return ()
        collect (PVar x) node = tell $ [(x, node)]
        collect (PLit lit) node = do
          Lit lit' <- (lift . lift) $ reduceWHNF node
          guard $ lit' == lit
        collect (PAsPat x p) node = tell [(x, node)] >> collect p node
        collect (PConApp con ps) node = do
          ConApp con' args <- (lift . lift) $ reduceWHNF node
          guard $ con' == con
          zipWithM_ collect ps args
