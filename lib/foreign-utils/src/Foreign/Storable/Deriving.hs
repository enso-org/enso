{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Foreign.Storable.Deriving (derive, derive', deriveNoContext, align) where

import Prologue

import Data.Bifunctor              (first)
import Foreign.Storable            (Storable)
import Language.Haskell.TH         hiding (clause)
import Language.Haskell.TH.Builder

import qualified Data.List.NonEmpty  as NonEmpty
import qualified Foreign.Storable    as Storable
import qualified Language.Haskell.TH as TH


--------------------------------------
-- === TH info extracting utils === --
--------------------------------------

extractFieldTypes :: TH.Con -> [TH.Type]
extractFieldTypes = \case
    TH.NormalC _ bangTypes     -> view _2 <$> bangTypes
    TH.RecC    _ nameBangTypes -> view _3 <$> nameBangTypes
    TH.InfixC  bangT1 _ bangT2 -> view _2 <$> [bangT1, bangT2]
    _ -> error "Storable.derive: GADTs and existentials not supported yet"

-------------------------------------
-- === TH convenience wrappers === --
-------------------------------------

sizeOfType :: TH.Type -> TH.Exp
sizeOfType = app (var 'Storable.sizeOf) . (var 'undefined -::)

sizeOfInt :: TH.Exp
sizeOfInt = sizeOfType $ cons' ''Int

undefinedAsInt :: TH.Exp
undefinedAsInt = var 'undefined -:: cons' ''Int

conFieldSizes :: TH.Con -> [TH.Exp]
conFieldSizes = fmap sizeOfType . extractFieldTypes

sizeOfCon :: TH.Con -> TH.Exp
sizeOfCon con
    | conArity con > 0 = unsafeFoldl1 plus $ conFieldSizes con
    | otherwise        = intLit 0

align :: TH.Exp
align = app (var 'Storable.alignment) undefinedAsInt

whereClause :: Name -> TH.Exp -> TH.Dec
whereClause n e = ValD (var n) (NormalB e) mempty

wildCardLazyClause :: TH.Exp -> TH.Clause
wildCardLazyClause expr = clause [TildeP WildP] expr mempty



--------------------------------
-- === Main instance code === --
--------------------------------

-- FIXME[WD->PM]: IRREFUTABLE PATTERN!
-- | Generate the `Storable` instance for a type.
--   The constraint is that all of the fields of
--   the type's constructor must be Ints/Pointers.
derive :: Name -> Q [TH.Dec]
derive ty = do
    TH.TyConI tyCon <- TH.reify ty
    derive' tyCon

deriveNoContext :: Name -> Q [TH.Dec]
deriveNoContext ty = do
    TH.TyConI tyCon <- TH.reify ty
    deriveCondCtx False tyCon

derive' :: Dec -> Q [TH.Dec]
derive' = deriveCondCtx True

deriveCondCtx :: Bool -> Dec -> Q [TH.Dec]
deriveCondCtx genConstraint dec = do
    let TypeInfo tyConName tyVars cs = getTypeInfo dec
    decs <- sequence [pure $ genSizeOf cs, pure genAlignment, genPeek cs, genPoke cs]
    let ctx = generateConstraint ''Storable cs
    let inst = classInstanceWithCtx (if genConstraint then ctx else []) ''Storable tyConName tyVars decs
    pure [inst]


-----------------------------------
-- === Constraints generator === --
-----------------------------------

generateConstraintForField :: TH.Name -> TH.Type -> TH.Type
generateConstraintForField className tp = app (cons' className) tp

generateConstraint :: TH.Name -> [TH.Con] -> TH.Cxt
generateConstraint className conses = constraint where
    constraint = generateConstraintForField className <$> allFields
    allFields  = concatMap extractFieldTypes conses

-------------------------------
-- === Method generators === --
-------------------------------

-- | Generate the offsets for a constructor (also pures the names of the variables in wheres).
--   Example:
--   > data T = Cons x y
--   >
--   > [...] where off0 = 0
--   >             off1 = sizeOf (undefined :: Int)
--   >             off2 = off1 + sizeOf (undefined :: x)
--   >             off3 = off2 + sizeOf (undefined :: y)
genOffsets :: Bool -> TH.Con -> Q (NonEmpty Name, NonEmpty TH.Dec)
genOffsets isSingleCons con = do
    let fSizes  = conFieldSizes con
        arity   = length fSizes
        name i  = newName $ "off" <> show i

    name0     <- name (0 :: Integer)
    namesList <- mapM name $ take arity ([1..] :: [Integer])
    let names = name0 :| namesList
    case names of
        n :| [] -> pure (names, whereClause n (intLit 0 -:: cons' ''Int) :| [])
        (n0 :| (n1:ns)) -> do
            let off0D   = whereClause n0 $ intLit 0 -:: cons' ''Int
                off1D   = whereClause n1 $ app (var 'Storable.sizeOf) undefinedAsInt
                headers = zip3 ns ((if isSingleCons then n0 else n1):ns) fSizes

                mkDecl :: (Name, Name, TH.Exp) -> Dec
                mkDecl (declName, refName, fSize) =
                    whereClause declName (plus (var refName) fSize) -- >> where declName = refName + size

                offDecls = mkDecl <$> headers
                -- if a type has one constructor, we need to omit the offset (and name) of the tag
                addIfMulti :: a -> [a] -> [a]
                addIfMulti e es = if isSingleCons then es else e:es

                clauses    = off0D :| addIfMulti off1D offDecls
                finalNames = n0    :| addIfMulti n1 ns

            pure (finalNames, clauses)

-- | Generate the `sizeOf` method of the `Storable` class.
--   It will pure the largest possible size of a given data type.
--   The mechanism is much like unions in C.
genSizeOf :: [TH.Con] -> TH.Dec
genSizeOf conss = FunD 'Storable.sizeOf [wildCardLazyClause expr]
    where expr = case conss of
            []  -> intLit 0
            [c] -> sizeOfCon c
            cs  -> genSizeOfExpr cs

genSizeOfExpr :: [TH.Con] -> TH.Exp
genSizeOfExpr cs = plus maxConSize sizeOfInt
    where conSizes   = ListE $ sizeOfCon <$> cs
          maxConSize = app2 (var 'maximumDef) (intLit 0) conSizes

-- | Generate the `alignment` method of the `Storable` class.
--   It will always be the size of `Int`.
genAlignment :: TH.Dec
genAlignment = FunD 'Storable.alignment [genAlignmentClause]

genAlignmentClause :: TH.Clause
genAlignmentClause = wildCardLazyClause $ app (var 'Storable.sizeOf) undefinedAsInt

-- | Generate the `peek` method of the `Storable` class.
--   It will behave differently for single- and multi-constructor types,
--   as well as for no-argument constructors. For details, please refer to
--   the docs for the `genPoke` method, where the memory layout is described
--   in detail.
genPeek :: [TH.Con] -> Q TH.Dec
genPeek cs = funD 'Storable.peek [genPeekClause cs]

-- | Generate the `case` expression that given a tag of the constructor
--   will perform the appropriate number of pokes.
genPeekCaseMatch :: Bool -> Name -> Integer -> TH.Con -> Q TH.Match
genPeekCaseMatch isSingleCons ptr idx con = do
    (off0 :| offNames, whereCs) <- genOffsets isSingleCons con
    let (cName, arity)   = conNameArity con
        peekByteOffPtr   = app (var 'Storable.peekByteOff) (var ptr)
        peekByte off     = app peekByteOffPtr $ var off
        appPeekByte t x  = op '(<*>) t $ peekByte x
        mkFirstCon off   = op '(<$>) (ConE cName) (peekByte off)
        -- This piece of logic is tricky due to different handling of (arity == 0, nCons > 1) configs
        -- * if the constructor has no arguments and we have no offsets, it's a data A = A case,
        --   so the peek is a simple `pure A`
        -- * if the cons. has no args, but is part of complex type, like data A = A | B [..],
        --   (we have offsets) we handle it like any other complex type's constructor
        -- * if the cons. has args, but is the only constructor in a type, like data A = A Int,
        --   we want it to start reading the values from 0 (off0)
        -- * if the cons. has no offsets, but its arity is not 0, it means we have data A = A Int
        --   single cons, single field. We then do just one peek and wrap it with constructor.
        -- * otherwise (data A = A Int | B Int Int, etc) we need to skip the first offset, because
        --   it was used
        (firstCon, offs) = case offNames of
                allOs@(off1:os) -> first mkFirstCon $ if isSingleCons then (off0, allOs) else (off1, os)
                _               -> if arity == 0 then (app (var 'pure) (ConE cName), [])
                                                 else (mkFirstCon off0, [])
        body             = NormalB $ foldl appPeekByte firstCon offs
        pat              = LitP $ IntegerL idx
        whereCs'         = if isSingleCons then NonEmpty.toList whereCs
                                           else NonEmpty.tail whereCs
    pure $ TH.Match pat body whereCs'

-- | Generate a catch-all branch of the case to account for
--   non-exhaustive patterns warnings.
genPeekCatchAllMatch :: [TH.Con] -> Q TH.Match
genPeekCatchAllMatch cs = do
    t <- newName "t"
    let cNames    = show . conName <$> cs
        cNamesFmt = foldl (\a b -> a <> ", " <> b) "" cNames
        errorMsg  = "[peek] Unrecognized constructor (not one of"
                 <> cNamesFmt <> "), got tag: "
        errorLit  = TH.LitE $ TH.StringL errorMsg
        showT     = app (var 'show) (var t)
        wholeMsg  = op '(<>) errorLit showT
        body      = app (var 'error) wholeMsg

    pure $ TH.Match (TH.VarP t) (TH.NormalB body) mempty

-- | Generate the `peek` clause for a single-constructor types.
--   In this case the fields are stored raw, without the tag.
genPeekSingleCons :: Name -> TH.Con -> Q TH.Clause
genPeekSingleCons ptr con = do
    TH.Match _ body whereCs <- genPeekCaseMatch True ptr 0 con
    let pat      = if noArgCon con then TH.WildP else var ptr
        whereCs' = if noArgCon con then []       else whereCs
    case body of
        TH.NormalB e  -> pure $ clause [pat] e whereCs'
        TH.GuardedB _ -> fail "[genPeekSingleCons] Guarded bodies not supported"

-- Generate the `peek` method for multi-constructor data types.
genPeekMultiCons :: Name -> Name -> [TH.Con] -> Q TH.Clause
genPeekMultiCons ptr tag cs = do
    peekCases <- zipWithM (genPeekCaseMatch False ptr) [0..] cs
    catchAll  <- genPeekCatchAllMatch cs
    let peekTag      = app (app (var 'Storable.peekByteOff) (var ptr)) (intLit 0)
        peekTagTyped = peekTag -:: app (cons' ''IO) (cons' ''Int)
        bind         = BindS (var tag) peekTagTyped
        cases        = CaseE (var tag) $ peekCases <> [catchAll]
        doE          = DoE [bind, NoBindS cases]
    pure $ clause [var ptr] doE mempty

-- | Generate the clause for the `peek` method,
--   deciding between single- and multi-constructor implementations.
genPeekClause :: [TH.Con] -> Q TH.Clause
genPeekClause cs = do
    ptr <- newName "ptr"
    tag <- newName "tag"
    case cs of
        []  -> fail "[genPeekClause] Phantom types not supported"
        [c] -> genPeekSingleCons ptr c
        cs  -> genPeekMultiCons ptr tag cs

-- | Generate a `poke` method of the `Storable` class.
--   Behaves differently for single- and multi-constructor types
--   as well as for constructors with no arguments.
--
--   When the type has a single constructor, we will just store
--   the elements in the memory one after the other.
--   Example: `data Bar = Bar Int Int Int` will be stored as:
--                  ----------------
--   Bar 1 10 100:  | 1 | 10 | 100 |
--                  ----------------
--
--   In the multi-constructor case we need to add a tag that will
--   encode the constructor that this value was created with.
--   Example: `data Foo = A Int | B Int | C Int` will be stored as:
--         ----------       ----------          -----------
--   A 12: | 0 | 12 | B 32: | 1 | 32 |  C 132 : | 2 | 132 |
--         ----------       ----------          -----------
genPoke :: [TH.Con] -> Q TH.Dec
genPoke conss = funD 'Storable.poke $ case conss of
    []  -> error "[genPoke] Phantom types not supported"
    [c] -> [genPokeClauseSingle c]
    cs  -> zipWith genPokeClauseMulti [0 ..] cs

-- | Generate the pattern for the `poke` method.
--   like: `poke ptr (SomeCons consArgs)`.
genPokePat :: Name -> Name -> [Name] -> [TH.Pat]
genPokePat ptr cName patVarNames =
    [var ptr, cons cName $ var <$> patVarNames]

-- | A wrapper utility for generating the poking expressions.
genPokeExpr :: Name -> NonEmpty Name -> [Name] -> TH.Exp -> TH.Exp
genPokeExpr ptr (off :| offNames) varNames firstExpr = body
    where pokeByteOffPtr = app (var 'Storable.pokeByteOff) (var ptr)
          pokeByte a     = app2 pokeByteOffPtr (var a)
          nextPoke t     = app2 (var '(>>)) t .: pokeByte
          firstPoke      = pokeByte off firstExpr
          varxps         = var <$> varNames
          body           = foldl (uncurry . nextPoke) firstPoke
                         $ zip offNames varxps

-- | Generate a `poke` clause ignoring its params and pureing unit.
genEmptyPoke :: Q TH.Clause
genEmptyPoke = pure $ clause [WildP, WildP] pureUnit mempty
    where pureUnit = app (var 'pure) (TH.ConE '())

-- | Generate a `poke` clause for single-constructor data types.
--   In this case we don't add the tag to the stored memory,
--   as it is unambiguous.
genPokeClauseSingle :: TH.Con -> Q TH.Clause
genPokeClauseSingle con = do
    let (cName, nParams) = conNameArity con
    ptr         <- newName "ptr"
    patVarNames <- newNames nParams
    -- if the constructor has no params, we will generate `poke _ _ = pure ()`
    case patVarNames of
        [] -> genEmptyPoke
        (firstP : restP) -> do
            (offNames, whereCs) <- genOffsets True con
            let pat  = genPokePat  ptr cName patVarNames
                body = genPokeExpr ptr offNames restP (var firstP)
            pure $ clause pat body (NonEmpty.toList whereCs)

-- | Generate a `poke` clause for multi-constructor data types.
--   In this case we add a tag to the stored memory, so that
--   when poking we know which constructor to choose.
genPokeClauseMulti :: Integer -> TH.Con -> Q TH.Clause
genPokeClauseMulti idx con = do
    let (cName, nParams) = conNameArity con
    ptr         <- newName "ptr"
    patVarNames <- newNames nParams
    (offNames, whereCs) <- genOffsets False con
    let pat            = genPokePat ptr cName patVarNames
        idxAsInt       = convert idx -:: cons' ''Int
        body           = genPokeExpr ptr offNames patVarNames idxAsInt
    pure $ clause pat body (NonEmpty.toList whereCs)

