{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Vis.FromSource where
-- module Vis.FromSource (fromName, fromExpr) where

import Vis.Node
import Vis.Monad

import qualified Language.Haskell.Syntax as H
import Control.Applicative
import Control.Monad.Error
import Control.Monad.ST
import Data.STRef
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Writer (tell, execWriter)
import Data.Function (on)
import Data.Maybe
import Language.Haskell.Pretty

builtinFromName (Name (H.HsSymbol "+")) = Just IntPlus
builtinFromName _ = Nothing

bindsFromDecl :: H.HsDecl -> [Name]
bindsFromDecl (H.HsPatBind _ pat _ _) = bindsFromPat pat
bindsFromDecl (H.HsFunBind ms) = [fst $ bindFromMatches ms]

withDecls :: [H.HsDecl] -> Vis s a -> Vis s a
withDecls decls = withVars $ concatMap bindsFromDecl decls

bindFromMatches :: [H.HsMatch] -> (Name, Int)
bindFromMatches ((H.HsMatch _ f pats _ _):_) = (Name f, arity)
  where arity = length pats

bindsFromPat = execWriter . collect
  where collect (H.HsPVar x) = tell [Name x]
        collect (H.HsPInfixApp left _ right) = collect left >> collect right
        collect (H.HsPApp _ pats) = mapM_ collect pats
        collect (H.HsPTuple pats) = mapM_ collect pats
        collect (H.HsPList pats) = mapM_ collect pats
        collect (H.HsPParen pat) = collect pat
        collect (H.HsPAsPat x pat) = tell [Name x] >> collect pat
        collect _ = return ()

fromDecl :: H.HsDecl -> Vis s (Name, (Node s))
fromDecl (H.HsPatBind _ (H.HsPVar x) (H.HsUnGuardedRhs expr) []) = do
  node <- fromExpr expr
  return (Name x, node)  
fromDecl (H.HsFunBind ms) = do
  let (f, arity) = bindFromMatches ms      
  matches <- forM ms $ \(H.HsMatch _ _ pats (H.HsUnGuardedRhs expr) []) ->
    (Match pats <$> fromExpr expr)
  node <- mkNode $ SwitchApp arity matches []
  return (f, node)

fromExpr :: H.HsExp -> Vis s (Node s)
fromExpr (H.HsParen expr) = fromExpr expr
fromExpr (H.HsLit (H.HsInt n)) = mkNode (IntLit n)
fromExpr (H.HsApp f x) = mkNode =<< (App <$> fromExpr f <*> fromExpr x)
fromExpr (H.HsLambda _ pats expr) = do
  node <- fromExpr expr  
  mkNode $ SwitchApp (length pats) [Match pats node] []
fromExpr (H.HsVar x) = do
  x' <- fromName x
  case builtinFromName x' of
    Just builtin -> mkNode $ BuiltinFunApp builtin []
    Nothing -> do
      bind <- lookupBind x'
      case bind of
        Just node -> return node
        Nothing -> mkNode $ ParamRef x'
fromExpr (H.HsLet decls body) = do
  withDecls decls $ do
    forM_ decls $ \decl -> do
      (x, node) <- fromDecl decl      
      setVar x node
    fromExpr body            
fromExpr (H.HsCon c) = mkNode =<< (ConApp <$> fromName c <*> return [])
fromExpr (H.HsList es) = fromExpr $ foldr cons nil es
  where cons x xs = H.HsApp (H.HsApp (H.HsCon $ H.Special $ H.HsCons) x) xs
        nil =  H.HsCon $ H.Special $ H.HsListCon
fromExpr (H.HsTuple es) = mkNode =<< (ConApp (Special (H.HsTupleCon (length es))) <$> mapM fromExpr es)
fromExpr (H.HsInfixApp left op right) = fromExpr $ H.HsApp (H.HsApp f left) right
  where f = case op of
          H.HsQVarOp var -> H.HsVar var
          H.HsQConOp con -> H.HsCon con
fromExpr (H.HsCase expr alts) = do          
  matches <- forM alts $ \alt -> case alt of
    H.HsAlt _ pat (H.HsUnGuardedAlt expr) [] -> Match [pat] <$> fromExpr expr
    _ -> unsupported $ prettyPrint alt
  node <- fromExpr expr
  mkNode $ SwitchApp 1 matches [node]
  
fromExpr e = unsupported $ prettyPrint e

fromName (H.UnQual name) = return $ Name name
fromName (H.Special special) = return $ Special special
fromName n = unsupported "qualified name"
