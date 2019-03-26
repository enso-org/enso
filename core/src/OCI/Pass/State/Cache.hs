{-# LANGUAGE OverloadedStrings #-}

module OCI.Pass.State.Cache where

-- import Prologue

-- import qualified OCI.Pass.State.Runtime as Pass
-- import qualified Type.Cache             as Type

-- import Language.Haskell.TH
-- import Language.Haskell.TH.Builder



-- mkLayoutName :: Name -> Name
-- mkLayoutName = ("PassStateLayout_" <>)

-- cache_phase1 :: Name -> Q [Dec]
-- cache_phase1 name = (layoutDecl:) <$> Type.cache_phase1 layoutName where
--     layoutName = mkLayoutName name
--     layoutType = ConT ''Pass.ComputeStateLayout
--     layoutDecl = TySynD layoutName [] $ app layoutType (ConT name)

-- cache_phase2 :: Name -> Q [Dec]
-- cache_phase2 name = (passInst:) <$> Type.cache_phase2 layoutName where
--     layoutName  = mkLayoutName name
--     layoutCache = Type.mkCacheName layoutName
--     passInst    = TySynInstD ''Pass.StateLayout
--                 $ TySynEqn [ConT name] (ConT layoutCache)

