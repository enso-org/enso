{-# LANGUAGE OverloadedStrings #-}

module OCI.Pass.TH where

import qualified OCI.Pass.Class as Pass

import Prologue
import Language.Haskell.TH
import Language.Haskell.TH.Builder
import Type.Cache

mkLayoutName :: Name -> Name
mkLayoutName = ("PassStateLayout_" <>)

cachePassConfig_phase1 :: Name -> Q [Dec]
cachePassConfig_phase1 name = (layoutDecl:) <$> cache_phase1 layoutName where
    layoutName = mkLayoutName name
    layoutType = ConT ''Pass.ComputePassStateLayout
    layoutDecl = TySynD layoutName [] $ app layoutType (ConT name)

cachePassConfig_phase2 :: Name -> Q [Dec]
cachePassConfig_phase2 name = (passInst:) <$> cache_phase2 layoutName where
    layoutName  = mkLayoutName name
    layoutCache = mkCacheName layoutName
    passInst   = TySynInstD ''Pass.PassStateLayout
               $ TySynEqn [ConT name] (ConT layoutCache)
