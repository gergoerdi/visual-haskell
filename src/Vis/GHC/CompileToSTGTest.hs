-- module Main where

import Vis.GHC.CompileToSTG
import Vis.GHC.SimpleSTG

import Outputable
import StgSyn (pprStgBindings)
import HscTypes
import Module
import Binary
import Name
import UniqFM
import FastMutInt
import Unique

import Data.Array
import Data.IORef
import IO
import System.Environment (getArgs)
import Control.Monad
import System.FilePath (replaceExtension)
import System.Directory (canonicalizePath)
import Data.Word
import Control.Arrow

withOpenFile fn mode f = do
  h <- liftIO $ openFile fn mode
  f h
  liftIO $ hClose h

withOutput fn f = do
  liftIO $ putStrLn . unwords $ ["Creating", fn]
  withOpenFile fn WriteMode f

fileName mod ext = replaceExtension (ml_hi_file . ms_location $ mod) ('.':ext)
                   
writeStg mod stg = withOutput (fileName mod "stg") $ \h -> do
    let fnSrc = ml_hs_file . ms_location $ mod
    case fnSrc of        
      Just fnSrc -> do
        fnSrc' <- canonicalizePath fnSrc
        hPutStrLn h $ unwords ["--", "Compiled from", fnSrc']
      Nothing -> return ()
    printForUser h neverQualify $ pprStgBindings $ map fst stg    

-- Based on GHC's BinIface
writeStgb mod stg = withOutput (fileName mod "stgb") $ \h -> do      
  bh <- openBinIO h
  -- bh <- openBinMem initBinMemSize       
       
  -- Remember where the dictionary pointer will go
  fsDict_p_p <- tellBin bh
  put_ bh fsDict_p_p	-- Placeholder for ptr to dictionary
      
  -- Remember where the symbol table pointer will go
  nameDict_p_p <- tellBin bh
  put_ bh nameDict_p_p

  nameDict <- mkFastDict
  fsDict <- mkFastDict            
  ud <- newWriteState (putSymbol nameDict) (putSymbol fsDict)
  bh <- return $ setUserData bh ud
            
  mapM_ (put_ bh) $ map (simplifyBinding . fst) stg
      
  -- Write the symtab pointer at the front of the file
  nameDict_p <- tellBin bh	        -- This is where the symtab will start
  putAt bh nameDict_p_p nameDict_p	-- Fill in the placeholder
  seekBin bh nameDict_p		-- Seek back to the end of the file      
      
  -- Write the symbol table itself
  putSymbolTable bh nameDict

  -- NB. write the dictionary after the symbol table, because
  -- writing the symbol table may create more dictionary entries.

  -- Write the dictionary pointer at the fornt of the file
  fsDict_p <- tellBin bh	        -- This is where the dictionary will start
  putAt bh fsDict_p_p fsDict_p	-- Fill in the placeholder
  seekBin bh fsDict_p		-- Seek back to the end of the file

  -- Write the dictionary itself
  fsDict_next <- readFastMutInt $ fsd_next fsDict
  fsDict_map <- readIORef $ fsd_map fsDict
  putDictionary bh fsDict_next fsDict_map  
  
  where initBinMemSize :: Int
        initBinMemSize = 1024 * 1024

main :: IO ()
main = do    
  args <- getArgs
  stgs <- toStg args
  forM_ stgs $ \(mod, stg) -> do
    writeStg mod stg
    writeStgb mod stg

data FastDict a = FastDict { fsd_next :: !FastMutInt,
                             fsd_map :: !(IORef (UniqFM (Int, a))) }
                      
mkFastDict :: IO (FastDict a)
mkFastDict = do
  next <- newFastMutInt
  writeFastMutInt next 0
  map <- newIORef emptyUFM
  return $ FastDict { fsd_next = next, fsd_map = map }

putSymbol :: (Uniquable a) => FastDict a -> BinHandle -> a -> IO ()
putSymbol FastDict{ fsd_next = fsd_next, fsd_map = fsd_map } bh sym = do
  map <- readIORef fsd_map
  case lookupUFM map sym of
    Just (idx, _) -> put_ bh (fromIntegral idx :: Word32)
    Nothing -> do
      idx <- readFastMutInt fsd_next
      put_ bh (fromIntegral idx :: Word32)
      writeFastMutInt fsd_next (succ idx)
      writeIORef fsd_map $! addToUFM map sym (idx, sym)
            
putSymbolTable :: BinHandle -> FastDict Name -> IO ()
putSymbolTable bh FastDict{ fsd_next = fsd_next, fsd_map = fsd_map} = do
  idx <- readFastMutInt fsd_next
  put_ bh idx
  map <- readIORef fsd_map
  let names = elems (array (0, idx - 1) (eltsUFM map))
  forM_ names $ \name -> do
    let mmod = nameModule_maybe name
    put_ bh (fmap (modulePackageId &&& moduleName) mmod, nameOccName name)
