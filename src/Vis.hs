{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Main where

import Vis.Node
import Vis.Monad
import Vis.FromSource
import Vis.Instantiate
import Vis.Reduce

import qualified Language.Haskell.Syntax as H
import Control.Applicative
import Control.Monad.Error
import Control.Monad.ST
import Data.STRef
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.RWS
import Control.Monad.State (StateT, evalStateT)
import Data.Function (on)
import Data.List
import Data.Maybe

type FunctionMap s = Map Name [Match s]
            
unsupported x = fail "Unsupported language feature"

showNode :: Int -> Node s -> ST s String
showNode depth (Node serial refPayload) | depth < 5 = do
  payload <- readSTRef refPayload
  payloadImage <- showPayload depth payload
  return $ unwords [replicate depth ' ' ++ (show serial), payloadImage]
showNode depth _ = return $ replicate depth ' ' ++ "..."

showPayload :: Int -> Payload s -> ST s String
showPayload depth Uninitialized = return "<unfilled>"
showPayload depth (ParamRef x) = return $ unwords ["ParamRef", show x]
showPayload depth (IntLit n) = return $ unwords ["IntLit", show n]
showPayload depth (App e f) = do
  e' <- showNode (succ depth) e
  f' <- showNode (succ depth) f
  return $ unlines' ["App", e', f']
showPayload depth (PartialFunApp f ns) = do
  ns' <- mapM (showNode $ succ depth) ns
  f' <- showFunction f
  return $ unlines' (unwords ["PartialFunApp", f']:ns')
showPayload depth (PartialConApp c ns) = do
  ns' <- mapM (showNode $ succ depth) ns
  return $ unlines' (unwords ["PartialConApp", show c]:ns')
    
showFunction (BuiltinFun builtin) = return $ show builtin    
showFunction (Matches f arity) = return $ concat [show f, '#':show arity]
    
unlines' = intercalate "\n"
    
-- printNode node = runST $ showNode 0 node

test = do
  sum <- fromExpr $ 
          H.HsApp 
            (H.HsApp 
             (H.HsVar $ H.UnQual $ H.HsSymbol "+")
             (H.HsLit (H.HsInt 35)))
            (H.HsLit (H.HsInt 7))
  
  let f = H.UnQual $ H.HsIdent "f"
      x = H.UnQual $ H.HsIdent "x"
      xs = H.UnQual $ H.HsIdent "xs"
      -- _map = H.UnQual $ H.HsIdent "map"
      cons = H.Special $ H.HsCons
      nil = H.Special $ H.HsListCon
  -- node <- let ones = H.HsIdent "ones"
  --             expr = H.HsApp
  --                      (H.HsApp (H.HsCon cons) (H.HsVar x))
  --                      (H.HsVar $ H.UnQual ones)
  --         in fromExpr $
  --           H.HsLet [H.HsPatBind undefined (H.HsPVar ones) (H.HsUnGuardedRhs expr) []] (H.HsVar $ H.UnQual ones)
            
  -- let formals1 = [H.HsPWildCard, H.HsPList []]
  -- nodeMap1 <- fromExpr $ H.HsCon $ H.UnQual $ H.HsSymbol "[]"
  
  -- let formals2 = [H.HsPApp cons [H.HsPVar $ H.HsIdent "x", H.HsPVar $ H.HsIdent "xs"], H.HsPVar $ H.HsIdent "f"]
  -- nodeMap2 <- fromExpr $
  --               H.HsApp 
  --                (H.HsApp (H.HsCon cons)
  --                 (H.HsApp (H.HsVar f) (H.HsVar x)))
  --                (H.HsApp
  --                 (H.HsApp
  --                  (H.HsVar _map)
  --                  (H.HsVar f))
  --                 (H.HsVar xs))
                                  
  let plus = H.UnQual $ H.HsSymbol "+"
      
  withFunctionForwards [(Name $ H.HsIdent "length", 1)] $ do
    matches <- do
      let lengthFormals1 = [H.HsPInfixApp (H.HsPVar $ H.HsIdent "x") cons (H.HsPVar $ H.HsIdent "xs")]
      lengthNode1 <- fromExpr $ H.HsInfixApp 
                      (H.HsLit (H.HsInt 1)) 
                      (H.HsQVarOp plus) 
                      (H.HsApp (H.HsVar $ H.UnQual $ H.HsIdent "length") (H.HsVar xs))
  
      let lengthFormals2 = [H.HsPList []]
      lengthNode2 <- fromExpr $ H.HsLit (H.HsInt 0)
      return [Match lengthFormals1 lengthNode1, Match lengthFormals2 lengthNode2]
                
    lst <- fromExpr $ 
            H.HsApp 
              (H.HsVar $ H.UnQual $ H.HsIdent "length")
              (H.HsList $ map (H.HsLit . H.HsInt) [1..5])
    img <- liftST $ showNode 0 lst
    withFunctions (Map.fromList [(Name $ H.HsIdent "length", matches)]) $ reduce lst
    img' <- liftST $ showNode 0 lst  
    return $ unlines [img, img']

toList = foldr cons nil
  where cons x xs = H.HsApp (H.HsApp (H.HsCon $ H.Special $ H.HsCons) x) xs
        nil =  H.HsCon $ H.Special $ H.HsListCon

main = putStrLn $ runST $ runVis test
