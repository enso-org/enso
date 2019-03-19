{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoStrict                  #-}
{-# LANGUAGE NoStrictData              #-}
{-# LANGUAGE OverloadedStrings         #-}

module Data.Tuple.Utils.TH where

import Prologue

import Language.Haskell.TH as TH

import Language.Haskell.TH.Builder

_MAX_TUPLE_SIZE :: Int
_MAX_TUPLE_SIZE = 30

tup :: Convertible (Cons a) a => [a] -> a
tup els = cons (convert $ "T" <> show (length els)) $ field' <$> els


-- >> type T2 = (,)
-- >> pattern T2 :: t1 -> t2 -> (t1,t2)
-- >> pattern T2 {t1,t2} = (t1,t2)
genTupleSyn :: Q [Dec]
genTupleSyn = pure decs where
    genDec i = [tpSyn, sig, syn] where
        synNameS = "T" <> show i
        synName  = convert synNameS
        tpSyn    = TySynD synName [] $ tupleCons i []
        sig      = PatSynSigD synName
                 $ ForallT tvvs []
                 $ ForallT [] []
                 $ foldr AppT (tuple tvs) (AppT ArrowT <$> tvs)
        syn      = PatSynD synName (RecordPatSyn ns') ImplBidir (tuple pvs)
        nsStr    = unsafeGenNamesTN i
        nsStr'   = (\n -> "_" <> synNameS <> "_" <> n) <$> nsStr
        ns       = convert <$> nsStr
        ns'      = convert <$> nsStr'
        tvvs     = var <$> ns
        tvs      = var <$> ns
        pvs      = var <$> ns'
    decs = concat $ genDec <$> [0.._MAX_TUPLE_SIZE]


genStrictTupDecls :: Q [Dec]
genStrictTupDecls = pure decs where
    genDec i = [decl] where
        con     = NormalC name
                $ (Bang NoSourceUnpackedness SourceStrict,) <$> tvs
        ns      = unsafeGenNamesTN i
        tvvs    = var <$> ns
        tvs     = var <$> ns
        nameStr = "T" <> show i
        name    = convert nameStr
        decl    = DataD [] name tvvs Nothing [con]
                  [DerivClause Nothing [cons' ''Show]]
    decs = concat $ genDec <$> [0.._MAX_TUPLE_SIZE]

-- >> data T2 = T2 {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Show)
genStrictIntTupDecls :: Q [Dec]
genStrictIntTupDecls = pure decs where
    genDec i = [decl] where
        con     = NormalC name
                $ replicate i (Bang SourceUnpack SourceStrict, cons' ''Int)
        nameStr = "T" <> show i
        name    = convert nameStr
        decl    = DataD [] name [] Nothing [con]
                  [DerivClause Nothing [cons' ''Show]]
    decs = concat $ genDec <$> [0.._MAX_TUPLE_SIZE]

-- >> FromNat 3 = T3
genIntTupleFromNat :: Q [Dec]
genIntTupleFromNat = pure [fam] where
    name   = mkName "FromNat"
    header = TypeFamilyHead name ["n"] NoSig Nothing
    line i = TySynEqn [convert i] (cons' . convert $ "T" <> show i)
    fam    = ClosedTypeFamilyD header (line <$> [0.._MAX_TUPLE_SIZE])


-- >> FromList '[t1, t2] = T2 t1 t2
genFromList :: Q [Dec]
genFromList = pure [fam] where
    name   = mkName "FromList"
    header = TypeFamilyHead name ["t"] NoSig Nothing
    line i = TySynEqn [foldr app PromotedNilT (app PromotedConsT <$> vs)]
             (tup vs)
             where vs = var <$> unsafeGenNamesTN i
    fam    = ClosedTypeFamilyD header (line <$> [0.._MAX_TUPLE_SIZE])

-- >> ToList T2 t1 t2 = '[t1, t2]
genToList :: Q [Dec]
genToList = pure [fam] where
    name   = mkName "ToList"
    header = TypeFamilyHead name ["t"] NoSig Nothing
    line i = TySynEqn [tup vs]
             (foldr app PromotedNilT (app PromotedConsT <$> vs))
             where vs = var <$> unsafeGenNamesTN i
    fam    = ClosedTypeFamilyD header (line <$> [0.._MAX_TUPLE_SIZE])

-- >> type instance GetElemAt 0 (T2 t1 t2) = t1
genGetElemAt :: Q [Dec]
genGetElemAt = pure decls where
    genDecl elIdx tupLen = TySynInstD "GetElemAt"
        (TySynEqn [convert elIdx, tup vs] (vs !! elIdx)) where
            vs = var <$> unsafeGenNamesTN tupLen
    genDeclsForTup i = (`genDecl` i) <$> [0..(i - 1)]
    decls = concat $ genDeclsForTup <$> [0.._MAX_TUPLE_SIZE]

-- >> type instance Head (T3 t1 t2 t3) = t1
genHead :: Q [Dec]
genHead = pure decls where
    genDecl tupLen = TySynInstD "Head"
        (TySynEqn [tup vs] (vs !! 0)) where
            vs = var <$> unsafeGenNamesTN tupLen
    decls = genDecl <$> [1.._MAX_TUPLE_SIZE]

-- >> type instance Tail (T3 t1 t2 t3) = T2 t2 t3
genTail :: Q [Dec]
genTail = pure decls where
    genDecl tupLen = TySynInstD "Tail"
        (TySynEqn [tup vs] (tup $ unsafeTail vs)) where
            vs = var <$> unsafeGenNamesTN tupLen
    decls = genDecl <$> [1.._MAX_TUPLE_SIZE]

-- >> instance HeadGetter (T3 t1 t2 t3) where
-- >>     head (T3 !t1 !t2 !t3) = t1 ; {-# INLINE head #-}
genHeadGetter :: Q [Dec]
genHeadGetter = pure decls where
    genDecl tupLen = decl where
        ns     = unsafeGenNamesTN tupLen
        tvs    = var <$> ns
        pvs    = var <$> ns
        evs    = var <$> ns
        header = apps (cons' "HeadGetter") [tup tvs]
        fun    = FunD "head" [ TH.Clause [tup $ BangP <$> pvs]
                 (NormalB (evs !! 0)) []]
        prag   = PragmaD (InlineP "head" Inline FunLike AllPhases)
        decl   = InstanceD Nothing [] header [fun, prag]
    decls = genDecl <$> [1.._MAX_TUPLE_SIZE]

-- >> instance TailGetter (T3 t1 t2 t3) where
-- >>     tail (T3 !t1 !t2 !t3) = T2 t2 t3 ; {-# INLINE tail #-}
genTailGetter :: Q [Dec]
genTailGetter = pure decls where
    genDecl tupLen = decl where
        ns     = unsafeGenNamesTN tupLen
        tvs    = var <$> ns
        pvs    = var <$> ns
        evs    = var <$> ns
        header = apps (cons' "TailGetter") [tup tvs]
        fun    = FunD "tail" [ TH.Clause [tup $ BangP <$> pvs]
                 (NormalB (tup $ unsafeTail evs)) []]
        prag   = PragmaD (InlineP "tail" Inline FunLike AllPhases)
        decl   = InstanceD Nothing [] header [fun, prag]
    decls = genDecl <$> [1.._MAX_TUPLE_SIZE]

-- >> type instance SetElemAt 0 v (T2 t1 t2) = T2 v t2
genSetElemAt :: Q [Dec]
genSetElemAt = pure decls where
    genDecl elIdx tupLen = TySynInstD "SetElemAt"
        (TySynEqn [convert elIdx, "v", tup vs] (tup $ replaceLst elIdx "v" vs))
        where vs = var <$> unsafeGenNamesTN tupLen
    genDeclsForTup i = (\a -> genDecl a i) <$> [0..(i - 1)]
    decls = concat $ genDeclsForTup <$> [0.._MAX_TUPLE_SIZE]

-- >> instance IxElemGetter 0 (T3 t1 t2 t3) where
-- >>     getElemAt (T3 !t1 !t2 !t3) = t1 ; {-# INLINE getElemAt #-}
genIxElemGetters :: Q [Dec]
genIxElemGetters = pure decls where
    genDecl elIdx tupLen = decl where
        ns     = unsafeGenNamesTN tupLen
        tvs    = var <$> ns
        pvs    = var <$> ns
        evs    = var <$> ns
        header = apps (cons' "IxElemGetter") [convert elIdx, tup tvs]
        fun    = FunD "getElemAt" [ TH.Clause [tup $ BangP <$> pvs]
                 (NormalB (evs !! elIdx)) []]
        prag   = PragmaD (InlineP "getElemAt" Inline FunLike AllPhases)
        decl   = InstanceD Nothing [] header [fun, prag]
    genDeclsForTup i = (`genDecl` i) <$> [0..(i - 1)]
    decls = concat $ genDeclsForTup <$> [0.._MAX_TUPLE_SIZE]

genIxElemSetters :: Q [Dec]
genIxElemSetters = pure decls where
    genDecl elIdx tupLen = decl where
        ns     = unsafeGenNamesTN tupLen
        tvs    = var <$> ns
        pvs    = var <$> ns
        evs    = var <$> ns
        header = apps (cons' "IxElemSetter") [convert elIdx, tup tvs]
        fun    = FunD "setElemAt" [ TH.Clause ["v", tup $ BangP <$> pvs]
                 (NormalB (tup $ replaceLst elIdx "v" evs)) []]
        prag   = PragmaD (InlineP "setElemAt" Inline FunLike AllPhases)
        decl   = InstanceD Nothing [] header [fun, prag]
    genDeclsForTup i = (`genDecl` i) <$> [0..(i - 1)]
    decls = concat $ genDeclsForTup <$> [0.._MAX_TUPLE_SIZE]

genDefaultInstances :: Q [Dec]
genDefaultInstances = pure decls where
    genDecl tupLen = decl where
        ns     = unsafeGenNamesTN tupLen
        tvs    = var <$> ns
        header = apps (cons' "Default") [tup tvs]
        fun    = FunD "def" [ TH.Clause []
                 (NormalB (tup $ replicate tupLen "def")) []]
        prag   = PragmaD (InlineP "def" Inline FunLike AllPhases)
        ctx    = app (cons' "Default") <$> tvs
        decl   = InstanceD Nothing ctx header [fun, prag]
    decls = genDecl <$> [0.._MAX_TUPLE_SIZE]

genMemptyInstances :: Q [Dec]
genMemptyInstances = pure decls where
    genDecl tupLen = decl where
        ns     = unsafeGenNamesTN tupLen
        tvs    = var <$> ns
        header = apps (cons' "Mempty") [tup tvs]
        fun    = FunD "mempty" [ TH.Clause []
                 (NormalB (tup $ replicate tupLen "mempty")) []]
        prag   = PragmaD (InlineP "mempty" Inline FunLike AllPhases)
        ctx    = app (cons' "Mempty") <$> tvs
        decl   = InstanceD Nothing ctx header [fun, prag]
    decls = genDecl <$> [0.._MAX_TUPLE_SIZE]

-- >> instance IxElemSetter 0 (T3 t1 t2 t3) where
-- >>     setElemAt v (T3 !t1 !t2 !t3) = T3 v t2 t3 ; {-# INLINE setElemAt #-}

genIntTupleIxElemGetters :: Q [Dec]
genIntTupleIxElemGetters = pure decls where
    genDecl elIdx tupLen = decl where
        name   = convert $ "T" <> show tupLen
        ns     = unsafeGenNamesTN tupLen
        pvs    = var <$> ns
        evs    = var <$> ns
        header = apps (cons' "IxElemGetter") [convert elIdx, cons' name]
        fun    = FunD "getElemAt" [ TH.Clause [tup $ BangP <$> pvs]
                 (NormalB (evs !! elIdx)) []]
        prag   = PragmaD (InlineP "getElemAt" Inline FunLike AllPhases)
        decl   = InstanceD Nothing [] header [fun, prag]
    genDeclsForTup i = (`genDecl` i) <$> [0..(i - 1)]
    decls = concat $ genDeclsForTup <$> [0.._MAX_TUPLE_SIZE]


genIntTupleIxElemSetters :: Q [Dec]
genIntTupleIxElemSetters = pure decls where
    genDecl elIdx tupLen = decl where
        name   = convert $ "T" <> show tupLen
        ns     = unsafeGenNamesTN tupLen
        pvs    = var <$> ns
        evs    = var <$> ns
        header = apps (cons' "IxElemSetter") [convert elIdx, cons' name]
        fun    = FunD "setElemAt" [ TH.Clause ["v", tup $ BangP <$> pvs]
                 (NormalB (tup $ replaceLst elIdx "v" evs)) []]
        prag   = PragmaD (InlineP "setElemAt" Inline FunLike AllPhases)
        decl   = InstanceD Nothing [] header [fun, prag]
    genDeclsForTup i = (`genDecl` i) <$> [0..(i - 1)]
    decls = concat $ genDeclsForTup <$> [0.._MAX_TUPLE_SIZE]


-- >> type instance Prepended t (T2 t1 t2) = T3 t t1 t2
genPrepended :: Q [Dec]
genPrepended = pure decls where
    genDecl tupLen = TySynInstD "Prepended"
        (TySynEqn ["t", tup vs] (tup $ "t" : vs)) where
            vs = var <$> unsafeGenNamesTN tupLen
    decls = genDecl <$> [0..(_MAX_TUPLE_SIZE - 1)]

-- >> instance Prependable t (T3 t1 t2 t3) where
-- >>     prepend t (T3 !t1 !t2 !t3) = T4 t t1 t2 t3 ; {-# INLINE prepend #-}
genPrependable :: Q [Dec]
genPrependable = pure decls where
    genDecl tupLen = decl where
        ns     = unsafeGenNamesTN tupLen
        tvs    = var <$> ns
        pvs    = var <$> ns
        evs    = var <$> ns
        header = apps (cons' "Prependable") ["t", tup tvs]
        fun    = FunD "prepend" [ TH.Clause ["t", tup $ BangP <$> pvs]
                 (NormalB (tup $ "t" : evs)) []]
        prag   = PragmaD (InlineP "prepend" Inline FunLike AllPhases)
        decl   = InstanceD Nothing [] header [fun, prag]
    decls = genDecl <$> [0.. (_MAX_TUPLE_SIZE - 1)]

replaceLst :: Int -> a -> [a] -> [a]
replaceLst i v lst = take i lst <> [v] <> drop (i+1) lst

