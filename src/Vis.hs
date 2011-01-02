{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Main where

import Vis.Node
import Vis.Monad
import Vis.FromSource
import Vis.Instantiate
import Vis.Reduce

import qualified Language.Haskell.Syntax as H
import Language.Haskell.Pretty
import Language.Haskell.Parser
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
showPayload depth (BuiltinFunApp op args) = do
  imgs <- mapM (showNode $ succ depth) args
  return $ unlines' (unwords ["BuiltinFunApp", show op]:imgs)
showPayload depth (SwitchApp arity matches args) = do  
  patimgs <- forM matches $ \(Match pats node) -> do
    img <- showNode (succ (succ (succ depth))) node
    return $ unlines' [replicate (succ (succ depth)) ' ' ++ unwords (map show pats) ++ " ->", img]    
  imgs <- mapM (showNode $ succ depth) args
  return $ unlines' (unwords ["SwitchApp", show arity]:(patimgs ++ imgs))
showPayload depth (ConApp c ns) = do
  ns' <- mapM (showNode $ succ depth) ns
  return $ unlines' (unwords ["PartialConApp", show c]:ns')
    
unlines' = intercalate "\n"
    
test = do
  let src = unlines $
            ["length (x:xs) = 1 + length xs",
             "length [] = 0",
             "",
             "length' xs = case xs of",
             "  (x:xs) -> 1 + length' xs",
             "  [] -> 0",
             "",
             "map f [] = []",
             "map f (x:xs) = (f x):(map f xs)",
             "",
             "",
             "ones = 1:ones",
             "",
             "steppers k = let inc x = x + k",
             "                 dec x = x - k",
             "             in (inc, dec)",
             "",             
             "fst (x, y) = x",
             "snd (x, y) = y",
             "",
             "take 0 _ = []",
             "take _ [] = []",
             "take n (x:xs) = x:take (n-1) xs",
             "",
             -- "main = take (length' [1,2,3,4,5]) ones",
             "main = let xy = steppers 3",
             "           inc = fst xy",
             "           dec = snd xy",
             "       in dec 4",
             ""
             ]  
      ParseOk mod = parseModule src
      H.HsModule _ _ _ _ decls = mod  
  withVars (concatMap bindsFromDecl decls) $ do
    forM_ decls $ \decl -> do
      (x, node) <- fromDecl decl      
      setVar x node
    Just main <- lookupBind (Name $ H.HsIdent "main")
    unlines <$> replicateM 4 (reduce main >> liftST (showNode 0 main) )
  
toList = foldr cons nil
  where cons x xs = H.HsApp (H.HsApp (H.HsCon $ H.Special $ H.HsCons) x) xs
        nil =  H.HsCon $ H.Special $ H.HsListCon

main = putStrLn $ runST $ runVis test
