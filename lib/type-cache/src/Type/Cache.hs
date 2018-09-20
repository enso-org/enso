{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Type.Cache where

import Prologue

import Data.Proxy
import Language.Haskell.TH
import Language.Haskell.TH.Builder

mkHelperName :: Name -> Name
mkHelperName = ("_TYPE_CACHE_HELPER_" <>)

mkCacheName :: Name -> Name
mkCacheName = (<> "_CACHE")

cache_phase1 :: Name -> Q [Dec]
cache_phase1 name = return [decl] where
    helperName = var $ mkHelperName name
    decl = ValD helperName
         (NormalB (cons' 'Proxy -:: app (cons' ''Proxy) (cons' name))) []


cache_phase2 :: Name -> Q [Dec]
cache_phase2 name = do
    let helperName = mkHelperName name
        cacheName  = mkCacheName  name
    info <- recover (fail "Run cache_phase1 before running cache_phase2.")
                    (reify helperName)
    case info of
        VarI vname (AppT (ConT p) t) _ ->
            if (nameBase vname == nameBase helperName)
            && (p == ''Proxy) then return [TySynD cacheName [] t]
            else internalError
        _ -> internalError

    where internalError = fail "Internal error. Please report it as a bug."

