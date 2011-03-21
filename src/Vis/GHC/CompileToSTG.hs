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

getModSummary' :: (GhcMonad m) => HscEnv -> FilePath -> m ModSummary
getModSummary' hsc_env filename = do
  let dflags = hsc_dflags hsc_env
      
  buf <- liftIO $ hGetStringBuffer filename
  src_timestamp <- liftIO $ getModificationTime filename  
  
  (src_imps, imps, L _ mod_name) <- getImports dflags buf filename filename
  
  let loc = ModLocation{ ml_hs_file   = Nothing,
                         ml_hi_file   = error "hi_file",
                         ml_obj_file  = error "obj_file" }
  mod <- liftIO $ addHomeModuleToFinder hsc_env mod_name loc
  
  return ModSummary {
    ms_mod = mod,
    ms_hsc_src = HsSrcFile,
    ms_hspp_file = filename,
    ms_hspp_opts = dflags,
    ms_hspp_buf = Just buf,
    ms_location = error "ms_location",
    ms_hs_date = src_timestamp,
    ms_obj_date = Nothing,
    ms_imps = [],
    ms_srcimps = src_imps
    }

parseAndTypecheck :: (GhcMonad m) => ModSummary -> m TcGblEnv
parseAndTypecheck mod_summary = do 
  rdr_module <- hscParse mod_summary
  hscTypecheck mod_summary rdr_module

type Stg id = [(GenStgBinding id id, [(id, [id])])]

compileToStg :: (GhcMonad m) => HscEnv -> ModSummary -> m (Stg Id)
compileToStg hsc_env mod_summary = do
  tcr <- parseAndTypecheck mod_summary
  guts <- hscDesugar mod_summary tcr
  guts' <- hscSimplify guts
  (_iface, _changed, _details, cgguts) <- hscNormalIface guts' Nothing
  
  liftIO $ do
    let dflags = hsc_dflags hsc_env
        coreBinds = cg_binds cgguts
        tycons = cg_tycons cgguts
        data_tycons = filter isDataTyCon tycons      
        this_mod = cg_module cgguts    
    prepd_binds <- corePrepPgm dflags coreBinds data_tycons
    stg_binds <- coreToStg (thisPackage dflags) prepd_binds    
    (stg_binds', _ccs) <- stg2stg dflags this_mod stg_binds
    return stg_binds'
  
toStg :: FilePath -> IO [Stg Id]
toStg filename = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags dflags

  target <- guessTarget filename Nothing
  setTargets [target]
  _ <- load LoadAllTargets
  mods <- depanal [] False

  withSession $ \hsc_env ->
    mapM (compileToStg hsc_env) mods
