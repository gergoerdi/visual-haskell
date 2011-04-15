{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module Vis.Reduce (reduceStep, reduceWHNF) where

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

import RdrName
import Literal
import PrimOp
import PrelNames
import OccName

import Debug.Trace

reducePrim IntEqOp [left, right] = do 
  Literal (MachInt n) <- reduceWHNF left
  Literal (MachInt m) <- reduceWHNF right
  return $ Just $ fromBool (n == m)

reducePrim IntAddOp [left, right] = do
  Literal (MachInt n) <- reduceWHNF left
  Literal (MachInt m) <- reduceWHNF right
  return $ Just $ Literal $ MachInt (n + m)
  
fromBool b = ConApp (mkOrig gHC_BOOL (mkDataOcc name)) []
  where name = case b of
          True -> "True"
          False -> "False"

readPayload node = cthunkPayload <$> readThunk node
writePayload node = writeThunk node . CThunk False
                      
reduceWHNF :: CNode -> CNodeM CPayload
reduceWHNF node = do
  changed <- reduceStep node
  if changed then reduceWHNF node else readPayload node
  
    
reduceStep :: CNode -> CNodeM Bool
reduceStep node = do
  payload <- readPayload node
  case payload of    
    App f [] -> overwriteWith f >> return True
    App f args -> do
      payloadFun <- reduceWHNF f            
      case payloadFun of
        ConApp con args' -> (writePayload node $ ConApp con (args' ++ args)) >> return True
        -- BuiltinFunApp op args' -> writePayload node $ BuiltinFunApp op (args' ++ args)
        Lambda vars body 
          | length vars <= length args -> do 
            let formalMap = Map.fromList $ zip vars args
            node' <- instantiate formalMap body
            let args' = drop (length vars) args
            overwriteWith =<< if null args'
                                then return node'
                                else (mkCNode (cnodeName node) False $ App node' args')
            return True
          | otherwise -> return False
    ParamRef x -> fail $ unwords ["Unfilled parameter:", showRdrName x]
    -- BuiltinFunApp op args -> reduceBuiltin op node args
    Case expr alts -> do
      node' <- applyCase alts expr
      case node' of
        Nothing -> return False
        Just node' -> overwriteWith node' >> return True
    ConApp _ _ -> return False
    Literal _ -> return False
    Lambda [] node' -> do
      -- reduceStep node'
      overwriteWith node' >> return True
    PrimApp op args -> do
      payload' <- reducePrim op args
      case payload' of
        Just payload' -> (overwriteWith =<< mkCNode Nothing False payload') >> return True
        Nothing -> return False
    _ -> return False    
    
  where overwriteWith node' = writePayload node =<< readPayload node'

applyCase :: [Alt VarName CNode] -> CNode -> CNodeM (Maybe CNode)
applyCase alts arg = do
  match <- firstMatch alts arg
  case match of
    Nothing -> return Nothing
    Just (formalMap, body) -> Just <$> instantiate formalMap body

firstMatch :: [CAlt] -> CNode -> CNodeM (Maybe (FormalMap, CNode))
firstMatch alts arg = do
  getFirst <$> mconcat <$> 
    (forM alts $ \ (Alt pat body) -> do              
        match <- match pat arg
        case match of
          Nothing -> return $ First $ Nothing
          Just formalMap -> return $ First $ Just (formalMap, body))  

match :: Pat VarName -> CNode -> CNodeM (Maybe FormalMap)
match pat arg = fmap (Map.fromList) <$> (runMaybeT $ execWriterT $ collect pat arg)
  where -- collect :: (Ord name) => Pat name -> CNode s name -> WriterT [(name, CNode s name)] (MaybeT (CNodeM s)) ()
        collect PWildcard node = return ()
        collect (PVar x) node = tell $ [(x, node)]
        collect (PLiteral lit) node = do
          Literal lit' <- (lift . lift) $ reduceWHNF node
          guard $ lit' == lit
        collect (PAsPat x p) node = tell [(x, node)] >> collect p node
        collect (PConApp con ps) node = do
          ConApp con' args <- (lift . lift) $ reduceWHNF node
          guard $ con' == con
          zipWithM_ collect ps args
