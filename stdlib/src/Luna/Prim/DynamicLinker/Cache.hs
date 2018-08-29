{-# LANGUAGE CPP #-}

module Luna.Prim.DynamicLinker.Cache where

import Prologue

import qualified Data.HashMap.Strict as HashMap

import Control.Concurrent      (MVar, newMVar, modifyMVar, readMVar,
                                takeMVar, swapMVar)
import Data.Hashable           (Hashable)
import Data.HashMap.Strict     (HashMap)
import Foreign.Ptr             (FunPtr, Ptr, castFunPtr)
import Luna.Prim.DynamicLinker (Handle, loadLibrary, closeLibrary, loadSymbol)

#if ! mingw32_HOST_OS
import qualified System.Posix.DynamicLinker as Unix
#endif

newtype HandleKey = HandleKey (Ptr ())
    deriving (Eq, Generic, Ord, Show)
makeLenses ''HandleKey

instance Hashable HandleKey

#if mingw32_HOST_OS
toKey :: Handle -> HandleKey
toKey = HandleKey
#else
toKey :: Handle -> HandleKey
toKey = HandleKey . Unix.packDL
#endif

data Cache = Cache
    { _moduleMap :: HashMap Text Handle
    , _symbolMap :: HashMap (HandleKey, Text) (FunPtr ())
    }
    deriving (Generic, Show)
makeLenses ''Cache

newtype CacheCtx = CacheCtx (MVar Cache)
    deriving (Eq, Generic)

instance Default Cache where
    def = Cache HashMap.empty HashMap.empty

create :: IO CacheCtx
create = CacheCtx <$> newMVar (Cache HashMap.empty HashMap.empty)

loadLibraryCached :: CacheCtx -> Text -> IO Handle
loadLibraryCached (CacheCtx cacheCtx) moduleName = do
    let lookupModule = HashMap.lookup moduleName . _moduleMap
    cachedHandle <- lookupModule <$> readMVar cacheCtx
    case cachedHandle of
        Just handle -> pure handle
        Nothing -> do
            -- Note: loadLibrary can be really slow,
            -- we don't want to run it within MVar
            newHandle <- loadLibrary $ convert moduleName
            -- In the meantime module could have been added to cache by
            -- other thread. We do not want to leak handles - get rid of
            -- our (now duplicate) one.
            modifyMVar cacheCtx $ \cache ->
                case lookupModule cache of
                    Just handleThatAppearedInCache -> do
                        closeLibrary newHandle
                        pure (cache, handleThatAppearedInCache)
                    Nothing -> do
                        let updateMap = HashMap.insert moduleName newHandle
                        pure (over moduleMap updateMap cache, newHandle)


lookupSymbolCached :: CacheCtx -> (Handle, Text) -> IO (FunPtr a)
lookupSymbolCached (CacheCtx cacheCtx) (handle, symbol) = do
    let key = (toKey handle, symbol)
    let lookupSymbol = HashMap.lookup key . _symbolMap
    cachedPtr <- lookupSymbol <$> readMVar cacheCtx
    ret <- case cachedPtr of
        Just ptr -> pure ptr
        Nothing -> do
            newPtr <- loadSymbol handle (convert symbol)
            modifyMVar cacheCtx $ \cache -> do
                let updateMap = HashMap.insert key newPtr
                pure (over symbolMap updateMap cache, newPtr)
    pure $ castFunPtr ret

-- Releases handles to modules allocated through this cache.
-- The cache context is not usable anymore.
finalize :: CacheCtx -> IO ()
finalize (CacheCtx ctxInside) = do
    Cache cache _ <- swapMVar ctxInside def
    let handles = HashMap.elems cache
    sequence_ (closeLibrary <$> handles)

