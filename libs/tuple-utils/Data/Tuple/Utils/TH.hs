{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Tuple.Utils.TH where

import Language.Haskell.TH.Builder
import Language.Haskell.TH as TH
import Prologue hiding (cons)

_MAX_TUPLE_SIZE :: Int
_MAX_TUPLE_SIZE = 20

tup els = cons (convert $ "T" <> show (length els)) $ field' <$> els


-- >> type T2 = (,)
-- >> pattern T2 :: t1 -> t2 -> (t1,t2)
-- >> pattern T2 {t1,t2} = (t1,t2)
genTupleSyn :: Q [Dec]
genTupleSyn = return decs where
    genDec i = [tpSyn, sig, syn] where
        synNameS = "T" <> show i
        synName  = convert synNameS
        tpSyn    = TySynD synName [] $ tupleCons i []
        sig      = PatSynSigD synName (ForallT tvvs [] (ForallT [] [] (foldr AppT (tuple tvs) (AppT ArrowT <$> tvs))))
        syn      = PatSynD synName (RecordPatSyn ns') ImplBidir (tuple pvs :: TH.Pat)
        nsStr    = unsafeGenNamesTN i
        nsStr'   = (\n -> "_" <> synNameS <> "_" <> n) <$> nsStr
        ns       = convert <$> nsStr
        ns'      = convert <$> nsStr'
        tvvs     = var <$> ns
        tvs      = var <$> ns
        pvs      = var <$> ns'
    decs = concat $ genDec <$> [0.._MAX_TUPLE_SIZE]


genStrictTupDecls :: Q [Dec]
genStrictTupDecls = return decs where
    genDec i = [DataD [] name tvvs Nothing [con] [DerivClause Nothing [cons' ''Show]]] where
        con     = NormalC name $ (Bang NoSourceUnpackedness SourceStrict,) <$> tvs
        ns      = unsafeGenNamesTN i
        tvvs    = var <$> ns
        tvs     = var <$> ns
        nameStr = "T" <> show i
        name    = convert nameStr
    decs = concat $ genDec <$> [0.._MAX_TUPLE_SIZE]

-- >> data T2 = T2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show)
genStrictIntTupDecls :: Q [Dec]
genStrictIntTupDecls = return decs where
    genDec i = [DataD [] name [] Nothing [con] [DerivClause Nothing [cons' ''Show]]] where
        con     = NormalC name $ replicate i (Bang SourceUnpack SourceStrict, cons' ''Int)
        nameStr = "T" <> show i
        name    = convert nameStr
    decs = concat $ genDec <$> [0.._MAX_TUPLE_SIZE]

-- >> FromNat 3 = T3
genIntTupleFromNat :: Q [Dec]
genIntTupleFromNat = return [fam] where
    name   = mkName "FromNat"
    header = TypeFamilyHead name ["n"] NoSig Nothing
    line i = TySynEqn [convert i] (cons' . convert $ "T" <> show i)
    fam    = ClosedTypeFamilyD header (line <$> [0.._MAX_TUPLE_SIZE])


-- >> FromList '[t1, t2] = T2 t1 t2
genFromList :: Q [Dec]
genFromList = return [fam] where
    name   = mkName "FromList"
    header = TypeFamilyHead name ["t"] NoSig Nothing
    line i = TySynEqn [foldr app PromotedNilT (app PromotedConsT <$> vs)] (tup vs) where
        vs = var <$> unsafeGenNamesTN i
    fam    = ClosedTypeFamilyD header (line <$> [0.._MAX_TUPLE_SIZE])

-- >> type instance GetElemAt 0 (T2 t1 t2) = t1
genGetElemAt :: Q [Dec]
genGetElemAt = return decls where
    genDecl elIdx tupLen = TySynInstD "GetElemAt" (TySynEqn [convert elIdx, tup vs] (vs !! elIdx)) where
        vs = var <$> unsafeGenNamesTN tupLen
    genDeclsForTup i = (\a -> genDecl a i) <$> [0..(i - 1)]
    decls = concat $ genDeclsForTup <$> [0.._MAX_TUPLE_SIZE]

-- >> type instance SetElemAt 0 v (T2 t1 t2) = T2 v t2
genSetElemAt :: Q [Dec]
genSetElemAt = return decls where
    genDecl elIdx tupLen = TySynInstD "SetElemAt" (TySynEqn [convert elIdx, "v", tup vs] (tup $ replaceLst elIdx "v" vs)) where
        vs = var <$> unsafeGenNamesTN tupLen
    genDeclsForTup i = (\a -> genDecl a i) <$> [0..(i - 1)]
    decls = concat $ genDeclsForTup <$> [0.._MAX_TUPLE_SIZE]

genElemGetters :: Q [Dec]
genElemGetters = return decls where
    genDecl elIdx tupLen = decl where
        ns     = unsafeGenNamesTN tupLen
        tvs    = var <$> ns
        pvs    = var <$> ns
        evs    = var <$> ns
        header = apps (cons' "ElemGetter") [convert elIdx, tup tvs]
        fun    = FunD "getElemAt" [ TH.Clause [tup $ BangP <$> pvs] (NormalB (evs !! elIdx)) []]
        prag   = PragmaD (InlineP "getElemAt" Inline FunLike AllPhases)
        decl   = InstanceD Nothing [] header [fun, prag]
    genDeclsForTup i = (\a -> genDecl a i) <$> [0..(i - 1)]
    decls = concat $ genDeclsForTup <$> [0.._MAX_TUPLE_SIZE]

genElemSetters :: Q [Dec]
genElemSetters = return decls where
    genDecl elIdx tupLen = decl where
        ns     = unsafeGenNamesTN tupLen
        tvs    = var <$> ns
        pvs    = var <$> ns
        evs    = var <$> ns
        header = apps (cons' "ElemSetter") [convert elIdx, tup tvs]
        fun    = FunD "setElemAt" [ TH.Clause ["v", tup $ BangP <$> pvs] (NormalB (tup $ replaceLst elIdx "v" evs)) []]
        prag   = PragmaD (InlineP "setElemAt" Inline FunLike AllPhases)
        decl   = InstanceD Nothing [] header [fun, prag]
    genDeclsForTup i = (\a -> genDecl a i) <$> [0..(i - 1)]
    decls = concat $ genDeclsForTup <$> [0.._MAX_TUPLE_SIZE]


genIntTupleElemGetters :: Q [Dec]
genIntTupleElemGetters = return decls where
    genDecl elIdx tupLen = decl where
        name   = convert $ "T" <> show tupLen
        ns     = unsafeGenNamesTN tupLen
        pvs    = var <$> ns
        evs    = var <$> ns
        header = apps (cons' "ElemGetter") [convert elIdx, cons' name]
        fun    = FunD "getElemAt" [ TH.Clause [tup $ BangP <$> pvs] (NormalB (evs !! elIdx)) []]
        prag   = PragmaD (InlineP "getElemAt" Inline FunLike AllPhases)
        decl   = InstanceD Nothing [] header [fun, prag]
    genDeclsForTup i = (\a -> genDecl a i) <$> [0..(i - 1)]
    decls = concat $ genDeclsForTup <$> [0.._MAX_TUPLE_SIZE]


genIntTupleElemSetters :: Q [Dec]
genIntTupleElemSetters = return decls where
    genDecl elIdx tupLen = decl where
        name   = convert $ "T" <> show tupLen
        ns     = unsafeGenNamesTN tupLen
        pvs    = var <$> ns
        evs    = var <$> ns
        header = apps (cons' "ElemSetter") [convert elIdx, cons' name]
        fun    = FunD "setElemAt" [ TH.Clause ["v", tup $ BangP <$> pvs] (NormalB (tup $ replaceLst elIdx "v" evs)) []]
        prag   = PragmaD (InlineP "setElemAt" Inline FunLike AllPhases)
        decl   = InstanceD Nothing [] header [fun, prag]
    genDeclsForTup i = (\a -> genDecl a i) <$> [0..(i - 1)]
    decls = concat $ genDeclsForTup <$> [0.._MAX_TUPLE_SIZE]



replaceLst i v lst = take i lst <> [v] <> drop (i+1) lst
