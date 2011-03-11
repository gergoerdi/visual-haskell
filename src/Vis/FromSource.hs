{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
module Vis.FromSource where
-- module Vis.FromSource (fromName, fromExpr) where

import Vis.Node
import Vis.Monad

import qualified Language.Haskell.Syntax as H
import Control.Applicative
import Control.Monad.Error
import Control.Monad.ST
import Data.STRef
import Control.Monad.Writer (tell, execWriter)
import Control.Monad.Reader
import Control.Monad.RWS
import Data.Function (on)
import Data.Maybe
import Language.Haskell.Pretty

import Data.Map (Map)
import qualified Data.Map as Map

newtype FromSource s a = FromSource { unFromSource :: RWST (Map Name (CNode s)) () Serial (Vis s) a }
                       deriving (Functor, Applicative, Monad, MonadState Serial)

runFromSource :: FromSource s a -> Vis s a
runFromSource f = fst <$> evalRWST (unFromSource f) Map.empty firstSerial

instance MonadCNode (FromSource s) s where
  liftCNode = FromSource . lift


lookupBind :: Name -> FromSource s (Maybe (CNode s))
lookupBind x = FromSource $ asks (Map.lookup x)

withVars :: [Name] -> FromSource s a -> FromSource s a
withVars vars f = do
  newBinds <- forM vars $ \var -> do
    node <- mkCNode_ (Just var)
    return (var, node)
  FromSource $ local (flip Map.union $ Map.fromList newBinds) $ unFromSource f

setVar :: Name -> CNode s -> FromSource s ()
setVar x node = do
  payload <- liftST $ readSTRef $ cnodePayload node
  node' <- fromJust <$> lookupBind x
  writePayload node' payload


builtinFromName (Name (H.HsSymbol "+")) = Just IntPlus
builtinFromName (Name (H.HsSymbol "-")) = Just IntMinus
builtinFromName _ = Nothing

bindsFromDecl :: H.HsDecl -> [Name]
bindsFromDecl (H.HsPatBind _ pat _ _) = bindsFromPat pat
bindsFromDecl (H.HsFunBind ms) = [fst $ bindFromMatches ms]

withDecls :: [H.HsDecl] -> FromSource s a -> FromSource s a
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

fromDecl :: H.HsDecl -> FromSource s (Name, (CNode s))
fromDecl (H.HsPatBind _ (H.HsPVar x) (H.HsUnGuardedRhs expr) []) = do
  CNode serial name payload <- fromExpr expr
  let node' = CNode serial (name `mplus` Just (Name x)) payload
  return (Name x, node')  
fromDecl (H.HsFunBind ms) = do
  let (f, arity) = bindFromMatches ms      
  alts <- forM ms $ \(H.HsMatch _ _ pats (H.HsUnGuardedRhs expr) []) ->
    (Alt pats <$> fromExpr expr)  
  vars <- forM [1..arity] $ \_-> do
    s <- gets unSerial
    modify succ
    return $ H.HsIdent $ "%v" ++ show s
  varRefs <- forM vars $ \var -> 
    mkCNode Nothing $ ParamRef $ Name var
  node <- mkCNode Nothing $ Case alts varRefs
  node' <- toLambda (map H.HsPVar vars) node
  return (f, node'{cnodeName = Just f})

toLambda :: [Pat] -> CNode s -> FromSource s (CNode s)
toLambda pats node = do
  foldM `flip` node `flip` (reverse pats) $ \node pat ->
    mkCNode Nothing $ Lambda pat node

fromExpr :: H.HsExp -> FromSource s (CNode s)
fromExpr (H.HsParen expr) = fromExpr expr
fromExpr (H.HsLit (H.HsInt n)) = mkCNode Nothing (IntLit n)
fromExpr (H.HsApp f x) = mkCNode Nothing =<< (App <$> fromExpr f <*> fromExpr x)
fromExpr (H.HsLambda _ pats expr) = toLambda pats =<< fromExpr expr
fromExpr (H.HsVar x) = do
  x' <- fromName x
  case builtinFromName x' of
    Just builtin -> mkCNode Nothing $ BuiltinFunApp builtin []
    Nothing -> do
      bind <- lookupBind x'
      case bind of
        Just node -> mkCNode Nothing $ Knot node
        Nothing -> mkCNode (Just x') $ ParamRef x'
fromExpr (H.HsLet decls body) = do
  withDecls decls $ do
    forM_ decls $ \decl -> do
      (x, node) <- fromDecl decl      
      setVar x node
    fromExpr body            
fromExpr (H.HsCon c) = mkCNode Nothing =<< (ConApp <$> fromName c <*> return [])
fromExpr (H.HsList es) = fromExpr $ foldr cons nil es
  where cons x xs = H.HsApp (H.HsApp (H.HsCon $ H.Special $ H.HsCons) x) xs
        nil =  H.HsCon $ H.Special $ H.HsListCon
fromExpr (H.HsTuple es) = mkCNode Nothing =<< (ConApp (Special (H.HsTupleCon (length es))) <$> mapM fromExpr es)
fromExpr (H.HsInfixApp left op right) = fromExpr $ H.HsApp (H.HsApp f left) right
  where f = case op of
          H.HsQVarOp var -> H.HsVar var
          H.HsQConOp con -> H.HsCon con
fromExpr (H.HsCase expr cases) = do          
  alts <- forM cases $ \alt -> case alt of
    H.HsAlt _ pat (H.HsUnGuardedAlt expr) [] -> Alt [pat] <$> fromExpr expr
    _ -> unsupported $ prettyPrint alt
  node <- fromExpr expr
  mkCNode Nothing $ Case alts [node]
  
fromExpr e = unsupported $ prettyPrint e

fromName (H.UnQual name) = return $ Name name
fromName (H.Special special) = return $ Special special
fromName n = unsupported "qualified name"
