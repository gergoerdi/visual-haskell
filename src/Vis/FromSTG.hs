module Vis.FromSTG where

import Vis.Node
import Vis.Monad
import Vis.GHC.CompileToSTG

import Vis.Flatten
-- import Vis.ToSource
import Control.Monad.ST (runST)

import StgSyn
import CoreSyn (AltCon(..))
import DataCon
import Outputable
import Var 
import Name
import Literal

import Control.Applicative
import Control.Monad.RWS
import Data.Map (Map)
import qualified Data.Map as Map

type FromSource s a = RWST (Map Name (CNode Name s)) () Serial (Vis s) a

fromExpr :: StgExpr -> FromSource s (CNode s Name)
fromExpr (StgSCC _ e) = fromExpr e
fromExpr (StgTick _ _ e) = fromExpr e
fromExpr (StgLit lit) = mkCNode Nothing $ Lit $ fromLit lit
fromExpr (StgLam _ vars body) = error "TODO: StgLam"
fromExpr (StgApp f args) = error "StgApp"
fromExpr (StgConApp con args) = mkCNode Nothing =<< (ConApp (dataConName con) <$> mapM fromArg args)
fromExpr (StgOpApp op args _) = error "StgOpApp"
fromExpr (StgCase e _ _ _ _ _ alts) = error "StgCase"
fromExpr (StgLet binding e) = error "StgLet"
fromExpr (StgLetNoEscape _ _ binding e) = fromExpr (StgLet binding e)

fromArg :: StgArg -> FromSource s (CNode s Name)
fromArg (StgVarArg x) = error "StgVarArg"
fromArg (StgLitArg lit) = mkCNode Nothing $ Lit $ fromLit lit

fromLit (MachInt n) = IntLit n
fromLit (MachInt64 n) = IntLit n
fromLit (MachWord n) = IntLit n
fromLit (MachWord64 n) = IntLit n 
fromLit (MachChar c) = CharLit c

fromRhs (StgRhsCon _ con args) = fromExpr (StgConApp con args)
fromRhs (StgRhsClosure _ _ _ update _ vars expr) = fromExpr expr -- TODO

bindingList (StgNonRec name rhs) = [(name, rhs)]
bindingList (StgRec binds) = binds

-- pprList ppr xs = brackets (hcat $ punctuate comma $ map ppr xs)

-- pprBind name rhs = text (show name) <+> text "=" <+> pprRhs rhs

-- pprBinding (StgNonRec name rhs) = pprBind name rhs
-- pprBinding (StgRec binds) = vcat $ map (uncurry pprBind) binds

-- pprRhs (StgRhsCon _ con args) = text "StgRhsCon"
-- pprRhs (StgRhsClosure _ _ occs update _ vars expr) = text "λ" <> pprUpdate update <+> pprList (text . show) vars <+> text "->" <+> pprExpr expr

-- pprUpdate ReEntrant = text "r"
-- pprUpdate Updatable = text "u"
-- pprUpdate SingleEntry = text "s"

-- pprAlt (con, vars, _, e) = pprAltCon con <+> pprList (text . show) vars <+> text "->" <+> pprExpr e

-- pprAltCon (DataAlt con) = text (show con)

-- pprArg (StgVarArg v) = text (show v)
-- pprArg (StgLitArg lit) = text "lit"
-- pprArg (StgTypeArg _) = text "type"

instance (Show Name) where
  show = show . occNameString . nameOccName

main :: IO ()
main = do
  stg <- toStg "../test/Hello.hs"
  printDump $ pprStgBindingsWithSRTs stg    
  
  putStrLn $ unlines $ runST $ do
    cnodes <- runVis $ liftM fst $ evalRWST `flip` Map.empty `flip` firstSerial $ do
        let bindings = concatMap bindingList $ map fst stg
        forM bindings $ \(x, rhs) -> do
          fromRhs rhs                  
    fnodes <- mapM flatten cnodes
    return $ map show fnodes
    
