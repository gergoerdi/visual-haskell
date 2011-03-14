module Vis.FromSTG where

import Vis.Node
import Vis.CNode

import StgSyn
import CoreSyn (AltCon(..))
import DataCon
import Name
import Literal

import Control.Applicative
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type FromSTG s a = ReaderT (Map Name (CNode s Name)) (CNodeM s) a

runFromSTG = runReaderT `flip` Map.empty

withVars vars f = do
  newBinds <- forM vars $ \var -> do
    node <- mkCNode_ (Just var)
    return (var, node)
  local (Map.union `flip` Map.fromList newBinds) f

withBindings :: [(Name, StgRhs)] -> FromSTG s a -> FromSTG s a
withBindings bindings f = withVars (map fst bindings) $ do
  forM_ bindings $ \(var, rhs) -> setVar var =<< fromRhs rhs
  f

withBinding :: StgBinding -> ([Name] -> FromSTG s a) -> FromSTG s a
withBinding binding f = let bindings = bindingList binding
                            vars = map fst bindings
                        in withBindings bindings $ f vars
    
lookupBind x = asks $ Map.lookup x

-- setVar :: Name -> CNode s -> FromSTG s ()
setVar x node = do
  payload <- readThunk node
  node' <- fromJust <$> lookupBind x
  writeThunk node' payload

fromExpr :: StgExpr -> FromSTG s (CNode s Name)
fromExpr (StgSCC _ e) = fromExpr e
fromExpr (StgTick _ _ e) = fromExpr e
fromExpr (StgLit lit) = mkCNode Nothing False $ Lit $ fromLit lit
fromExpr (StgLam _ vars body) = error "TODO: StgLam"
fromExpr (StgApp f args) = mkCNode Nothing False =<< App <$> fromVar f <*> mapM fromArg args
fromExpr (StgConApp con args) = mkCNode Nothing False =<< (ConApp (dataConName con) <$> mapM fromArg args)
fromExpr (StgOpApp op args _) = error "TODO: StgOpApp"
fromExpr (StgCase e _ _ _ _ _ (a:as)) = do
  alts <- mapM fromAlt (as ++ [a]) -- we rotate a:as to make sure the wildcard case (if any) is the last one
  node <- fromExpr e
  mkCNode Nothing False $ Case alts node
fromExpr (StgLet binding body) = withBinding binding $ \vars -> fromExpr body
fromExpr (StgLetNoEscape _ _ binding e) = fromExpr (StgLet binding e)

fromVar v = do
  let x = getName v
  lookup <- lookupBind x
  case lookup of
    Just node -> return node
    Nothing -> mkCNode Nothing False $ ParamRef x
  
  where x = getName v

fromAlt :: StgAlt -> FromSTG s (Alt Name (CNode s Name))
fromAlt (con, vars, _, e) = Alt pat <$> fromExpr e
  where pat = case con of
          DEFAULT -> PWildcard
          LitAlt l -> PLit $ fromLit l
          DataAlt c -> PConApp (getName c) $ map (PVar . getName) vars

fromArg :: StgArg -> FromSTG s (CNode s Name)
fromArg (StgVarArg v) = fromVar v
fromArg (StgLitArg lit) = mkCNode Nothing False $ Lit $ fromLit lit

fromLit (MachInt n) = IntLit n
fromLit (MachInt64 n) = IntLit n
fromLit (MachWord n) = IntLit n
fromLit (MachWord64 n) = IntLit n 
fromLit (MachChar c) = CharLit c

fromRhs (StgRhsCon _ con args) = fromExpr (StgConApp con args)
fromRhs (StgRhsClosure _ _ _ update _ vars expr) = 
  mkCNode Nothing reentrant =<< (Lambda (map getName vars) <$> fromExpr expr)
  where reentrant = case update of
          ReEntrant -> True
          _ -> False

bindingList (StgNonRec name rhs) = [(getName name, rhs)]
bindingList (StgRec binds) = map (\(name, rhs) -> (getName name, rhs)) binds

-- pprUpdate ReEntrant = text "r"
-- pprUpdate Updatable = text "u"
-- pprUpdate SingleEntry = text "s"

instance (Show Name) where
  show = show . occNameString . nameOccName

fromBindings :: [StgBinding] -> FromSTG s [CNode s Name]
fromBindings bs = do
  let bindings = concatMap bindingList bs
      vars = map fst bindings
  withBindings bindings $ mapM (liftM fromJust . lookupBind) vars
