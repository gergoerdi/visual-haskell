module Vis.GHC.CompileToSTG (toStg, Stg) where

import GHC
import HscTypes
import HscMain
import SrcLoc
import HeaderInfo
import Finder
import Module
import StringBuffer
import System.Directory
import GHC.Paths (libdir)
import GHC (runGhc, getSessionDynFlags, setSessionDynFlags)
import DynFlags (thisPackage)
import CorePrep (corePrepPgm)
import TyCon (isDataTyCon)
import CoreToStg (coreToStg)
import SimplStg (stg2stg)
import Var (Id)
import StgSyn (GenStgBinding)
import TcRnTypes (TcGblEnv)

import System.Environment (getArgs)

parseAndTypecheck :: (GhcMonad m) => ModSummary -> m DesugaredModule
parseAndTypecheck mod = parseModule mod >>= typecheckModule >>= desugarModule

type Stg id = [(GenStgBinding id id, [(id, [id])])]

compileToStg :: (GhcMonad m) => ModSummary -> m (Stg Id)
compileToStg mod = do
  core <- parseAndTypecheck mod
  let guts = coreModule core
  guts' <- hscSimplify guts
  (_iface, _changed, _details, cgguts) <- hscNormalIface guts' Nothing
  
  liftIO $ do
    let dflags = ms_hspp_opts mod
        coreBinds = cg_binds cgguts
        tycons = cg_tycons cgguts
        data_tycons = filter isDataTyCon tycons      
        this_mod = cg_module cgguts    
    prepd_binds <- corePrepPgm dflags coreBinds data_tycons
    stg_binds <- coreToStg (modulePackageId this_mod) prepd_binds    
    (stg_binds', _ccs) <- stg2stg dflags this_mod stg_binds
    return stg_binds'
  
toStg :: FilePath -> IO [Stg Id]
toStg filename = runGhc (Just libdir) $ do
  args <- liftIO getArgs
  dflags <- getSessionDynFlags
  (dflags', lfilenames, _) <- parseDynamicFlags dflags (map noLoc args)
  let filenames = map unLoc lfilenames
  _ <- setSessionDynFlags dflags'

  targets <- mapM (guessTarget `flip` Nothing) filenames
  setTargets targets
  _ <- load LoadAllTargets
  mods <- depanal [] False
  mapM compileToStg mods
