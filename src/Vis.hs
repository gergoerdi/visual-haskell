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
showPayload depth (VarRef x) = return $ unwords ["VarRef", show x]
showPayload depth (IntLit n) = return $ unwords ["IntLit", show n]
showPayload depth (App e f) = do
  e' <- showNode (succ depth) e
  f' <- showNode (succ depth) f
  return $ unlines' ["App", e', f']
showPayload depth (PartialFunApp f ns) = do
  ns' <- mapM (showNode $ succ depth) ns
  return $ unlines' (unwords ["PartialFunApp", show f]:ns')
showPayload depth (PartialConApp c ns) = do
  ns' <- mapM (showNode $ succ depth) ns
  return $ unlines' (unwords ["PartialConApp", show c]:ns')
    
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
      cons = H.UnQual $ H.HsSymbol ":"
      nil = H.UnQual $ H.HsSymbol "[]"
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
      
  matches <- do
    let lengthFormals1 = [H.HsPApp cons [H.HsPVar $ H.HsIdent "x", H.HsPVar $ H.HsIdent "xs"]]
    lengthNode1 <- fromExpr $ 
                    H.HsApp
                      (H.HsApp (H.HsVar plus) (H.HsLit (H.HsInt 1)))
                      (H.HsApp (H.HsVar $ H.UnQual $ H.HsIdent "length") (H.HsVar xs))
  
    let lengthFormals2 = [H.HsPApp nil []]
    lengthNode2 <- fromExpr $ H.HsLit (H.HsInt 0)
    return [Match lengthFormals1 lengthNode1, Match lengthFormals2 lengthNode2]
                
  lst <- fromExpr $ 
          H.HsApp 
            (H.HsVar $ H.UnQual $ H.HsIdent "length")
            (toList $ map (H.HsLit . H.HsInt) [1..5])
  img <- liftST $ showNode 0 lst
  withFunctions (Map.fromList [(H.HsIdent "length", matches)]) $ reduce lst
  img' <- liftST $ showNode 0 lst  
  
  -- return $ unlines [img1, img2]
  -- -- return img

  -- img1' <- liftST $ showNode 0 $ sum
  -- reduce sum
  -- img2' <- liftST $ showNode 0 $ sum
  return $ unlines [img, img']

toList = foldr (\ e f -> H.HsApp (H.HsApp cons e) f) nil
  where cons = H.HsCon $ H.UnQual $ H.HsSymbol ":"
        nil =  H.HsCon $ H.UnQual $ H.HsSymbol "[]"

main = putStrLn $ runST $ runVis test
