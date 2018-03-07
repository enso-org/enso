{-# LANGUAGE TemplateHaskell #-}
module Foreign.Storable1.Deriving (deriveStorable1) where

import Prologue

import Foreign.Storable            (Storable)
import Foreign.Storable1           (Storable1)
import Language.Haskell.TH.Lib     hiding (clause)
import Language.Haskell.TH.Builder
import Language.Haskell.TH         hiding (clause)

import qualified Foreign.Storable    as Storable
import qualified Foreign.Storable1   as Storable1
import qualified Language.Haskell.TH as TH


--------------------------------
-- === Main instance code === --
--------------------------------

deriveStorable1 :: Name -> Q [TH.Dec]
deriveStorable1 ty = do
    TypeInfo tyConName _ _ <- getTypeInfo ty
    let decs = concat [genSizeOf, genAlignment, genPeek, genPoke]
        inst = classInstance ''Storable1 tyConName mempty decs
    return [inst]



-------------------------------
-- === Method generators === --
-------------------------------

genClause :: Name -> TH.Clause
genClause n = clause mempty (var n) mempty

genFun :: Name -> Name -> [TH.Dec]
genFun n1 n2 = [FunD n1 [genClause n2], inlineF n1]

genSizeOf :: [TH.Dec]
genSizeOf = genFun 'Storable1.sizeOf 'Storable.sizeOf

genAlignment :: [TH.Dec]
genAlignment = genFun 'Storable1.alignment 'Storable.alignment

genPeek :: [TH.Dec]
genPeek = genFun 'Storable1.peek 'Storable.peek

genPoke :: [TH.Dec]
genPoke = genFun 'Storable1.poke 'Storable.poke
