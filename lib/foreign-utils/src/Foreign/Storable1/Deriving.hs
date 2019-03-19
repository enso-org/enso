{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Foreign.Storable1.Deriving (derive, derive') where

import Prologue

import Foreign.Storable1           (Storable1)
import Language.Haskell.TH         hiding (clause)
import Language.Haskell.TH.Builder

import qualified Foreign.Storable    as Storable
import qualified Foreign.Storable1   as Storable1
import qualified Language.Haskell.TH as TH


--------------------------------
-- === Main instance code === --
--------------------------------

-- FIXME[WD->PM]: IRREFUTABLE PATTERN!
derive :: Name -> Q [TH.Dec]
derive ty = do
    TH.TyConI tyCon <- TH.reify ty
    derive' tyCon

derive' :: Dec -> Q [TH.Dec]
derive' dec = do
    sizeOf    <- genSizeOf
    alignment <- genAlignment
    let TypeInfo tyConName tyVars _ = getTypeInfo dec
        decs = concat [sizeOf, alignment, genPeek, genPoke]
    case tyVars of
        [] -> fail "[Storable1.derive] Kind of type needs to be: * -> *"
        _  -> pure [classInstance ''Storable1 tyConName (unsafeInit tyVars) decs]



-------------------------------
-- === Method generators === --
-------------------------------

genClause :: Name -> TH.Clause
genClause n = clause mempty (var n) mempty

genFun :: Name -> Name -> [TH.Dec]
genFun n1 n2 = [FunD n1 [genClause n2], inlineF n1]

genLazyClause :: Name -> Q TH.Clause
genLazyClause _ = do
    a <- newName "a"
    let pat  = TH.TildeP $ TH.VarP a
        exp  = app (var 'Storable.sizeOf) (var a)
        lam  = TH.LamE [pat] exp
    pure $ TH.Clause mempty (TH.NormalB lam) mempty

genLazyFun :: Name -> Name -> Q [TH.Dec]
genLazyFun n1 n2 = do
    lazyClause <- genLazyClause n2
    pure [FunD n1 [lazyClause], inlineF n1]

genAlignment, genSizeOf :: Q [TH.Dec]
genSizeOf    = genLazyFun 'Storable1.sizeOf    'Storable.sizeOf
genAlignment = genLazyFun 'Storable1.alignment 'Storable.alignment

genPoke, genPeek :: [TH.Dec]
genPeek = genFun 'Storable1.peek 'Storable.peek
genPoke = genFun 'Storable1.poke 'Storable.poke

