module Main where

import Vis.Node
import Vis.Monad
import Vis.FromSource
import Vis.Flatten
import Vis.ToSource
import Vis.Instantiate
import Vis.Reduce

import qualified Language.Haskell.Syntax as H
import Language.Haskell.Parser
import Language.Haskell.Infix
import Language.Haskell.Pretty
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.STRef

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
             "main = take (length [1,2,3,4,5]) ones",
             -- "main = let xy = steppers 3",
             -- "           inc = fst xy",
             -- "           dec = snd xy",
             -- "       in dec 4",
             -- "main = let nats = 1:map (\\x -> x + 1) nats in nats",
             ""
             ]  
      ParseOk mod = parseModule src
      H.HsModule _ _ _ _ decls' = mod  
      decls = infixer emptyInfixMap decls'
  withDecls decls $ do
    forM_ decls $ \decl -> do
      (x, node) <- fromDecl decl      
      setVar x node
    Just main <- lookupBind (Name $ H.HsIdent "main")
    unlines <$> (replicateM steps $ do      
      result <- liftM prettyPrint $ liftST (liftM toSource $ flatten main)
      reduce main
      return result)
  where steps = 1

main = putStrLn $ runST $ runVis test
