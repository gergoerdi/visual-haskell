module Main where

import Vis.Node
import Vis.Monad
import Vis.FromSource
import Vis.Instantiate
import Vis.Reduce

import qualified Language.Haskell.Syntax as H
import Language.Haskell.Parser
import Language.Haskell.Pretty
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.STRef

projectNode :: Int -> Node s -> ST s H.HsExp
projectNode depth (Node serial refPayload) | depth < 5 = do
  payload <- readSTRef refPayload
  projectPayload depth payload
projectNode _ _ =  return $ H.HsWildCard

toApp = foldl1 H.HsApp 

projectName (Name n) = H.UnQual n
projectName (Special s) = H.Special s

noLoc = error "No location"

projectPayload :: Int -> Payload s -> ST s H.HsExp
projectPayload depth Uninitialized = return $ H.HsWildCard
projectPayload depth (ParamRef x) = return $ H.HsVar $ projectName x
projectPayload depth (IntLit n) = return $ H.HsLit $ H.HsInt n  
projectPayload depth (App e f) = do
  liftM2 H.HsApp (projectNode (succ depth) e) (projectNode (succ depth) f)
projectPayload depth (BuiltinFunApp op args) = do  
  projectArgs <- mapM (projectNode (succ depth)) args
  let fun = case op of
        IntPlus -> H.HsVar (H.UnQual $ H.HsSymbol "+")
        IntMinus -> H.HsVar (H.UnQual $ H.HsSymbol "-")
  return $ toApp $ fun:projectArgs
projectPayload depth (ConApp c args) = do
  projectArgs <- mapM (projectNode (succ depth)) args
  let con = H.HsCon $ projectName c
  return $ toApp $ con:projectArgs
projectPayload depth (SwitchApp arity alts args) = do
  projectAlts <- forM alts $ \ (Match pats node) -> do
    body <- projectNode (succ depth) node
    return $ H.HsAlt noLoc (H.HsPTuple pats) (H.HsUnGuardedAlt body) []
  projectArgs <- mapM (projectNode (succ depth)) args  
  let missing = replicate (arity - length args) H.HsWildCard
      expr = H.HsTuple $ projectArgs ++ missing
  return $ H.HsCase expr projectAlts

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
             "main = take (length' [1,2,3,4,5]) ones",
             -- "main = let xy = steppers 3",
             -- "           inc = fst xy",
             -- "           dec = snd xy",
             -- "       in dec 4",
             ""
             ]  
      ParseOk mod = parseModule src
      H.HsModule _ _ _ _ decls = mod  
  withVars (concatMap bindsFromDecl decls) $ do
    forM_ decls $ \decl -> do
      (x, node) <- fromDecl decl      
      setVar x node
    Just main <- lookupBind (Name $ H.HsIdent "main")
    unlines <$> replicateM 4 (reduce main >> (liftM prettyPrint $ liftST (projectNode 0 main)))
  
main = putStrLn $ runST $ runVis test
