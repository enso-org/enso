{-# LANGUAGE TemplateHaskell #-}
module Foreign.Storable.Deriving (deriveStorable) where

import Prologue

import Control.Lens                (view, _2, _3)
import Foreign.Storable            (Storable)
import GHC.Num
import Language.Haskell.TH         hiding (clause)
import Language.Haskell.TH.Builder
import Language.Haskell.TH.Lib     hiding (clause)

import qualified Data.List.NonEmpty  as NonEmpty
import qualified Foreign.Storable    as Storable
import qualified Language.Haskell.TH as TH


--------------------------------------
-- === TH info extracting utils === --
--------------------------------------

concretizeType :: TH.Type -> TH.Type
concretizeType = \case
    ConT n   -> ConT n
    VarT _   -> ConT ''Int
    AppT l r -> AppT (concretizeType l) (concretizeType r)
    _        -> error "***error*** deriveStorable: only reasonably complex types supported"

-- | Instantiate all the free type variables to Int for a consturctor
extractConcreteTypes :: TH.Con -> [TH.Type]
extractConcreteTypes = \case
    NormalC n bts -> (concretizeType . view _2) <$> bts
    RecC    n bts -> (concretizeType . view _3) <$> bts
    _ -> error "***error*** deriveStorable: type not yet supported"



-------------------------------------
-- === TH convenience wrappers === --
-------------------------------------

sizeOfType :: TH.Type -> TH.Exp
sizeOfType = app (var 'Storable.sizeOf) . (var 'undefined -::)

sizeOfInt :: TH.Exp
sizeOfInt = sizeOfType $ cons' ''Int

op :: Name -> TH.Exp -> TH.Exp -> TH.Exp
op = app2 . var

plus, mul :: TH.Exp -> TH.Exp -> TH.Exp
plus = op '(+)
mul  = op '(*)

intLit :: Integer -> TH.Exp
intLit = LitE . IntegerL

undefinedAsInt :: TH.Exp
undefinedAsInt = var 'undefined -:: cons' ''Int

conFieldSizes :: TH.Con -> [TH.Exp]
conFieldSizes = fmap sizeOfType . extractConcreteTypes

sizeOfCon :: TH.Con -> TH.Exp
sizeOfCon con
    | conArity con > 0 = unsafeFoldl1 plus $ conFieldSizes con
    | otherwise        = intLit 0

align :: TH.Exp
align = app (var 'Storable.alignment) undefinedAsInt

whereClause :: Name -> TH.Exp -> TH.Dec
whereClause n e = ValD (var n) (NormalB e) mempty



--------------------------------
-- === Main instance code === --
--------------------------------

deriveStorable :: Name -> Q [TH.Dec]
deriveStorable ty = do
    TypeInfo tyConName tyVars cs <- getTypeInfo ty
    decs <- sequence [return $ genSizeOf cs, return genAlignment, genPeek cs, genPoke cs]
    let inst = classInstance ''Storable tyConName tyVars decs
    return [inst]


-------------------------------
-- === Method generators === --
-------------------------------

-- | Generate the offsets for a constructor (also returns the names of the variables in wheres).
--   Example:
--   > data T = Cons x y
--   >
--   > [...] where off0 = 0
--   >             off1 = sizeOf (undefined :: Int)
--   >             off2 = off1 + sizeOf (undefined :: x)
--   >             off3 = off2 + sizeOf (undefined :: y)
genOffsets :: TH.Con -> Q (NonEmpty Name, NonEmpty TH.Dec)
genOffsets con = do
    let fSizes  = conFieldSizes con
        arity   = length fSizes
        name i  = newName $ "off" <> show i

    name0     <- name 0
    namesList <- mapM name $ take arity [1..]
    let names = name0 :| namesList
    case names of
        n :| [] -> return (names, whereClause n (intLit 0) :| [])
        names@(n1 :| (n2:ns)) -> do
            let off0D   = whereClause n1 $ intLit 0
                off1D   = whereClause n2 $ app (var 'Storable.sizeOf) undefinedAsInt
                headers = zip3 ns (n2:ns) fSizes

                mkDecl :: (Name, Name, TH.Exp) -> Dec
                mkDecl (declName, refName, fSize) =
                    whereClause declName (plus (var refName) fSize) -- >> where declName = refName + size

                clauses = off0D :| (off1D : fmap mkDecl headers)

            return (names, clauses)

genSizeOf :: [TH.Con] -> TH.Dec
genSizeOf conss  = FunD 'Storable.sizeOf $ case conss of
    []  -> error "[genSizeOf] Phantom types not supported"
    [c] -> [clause [WildP] sizeOfInt mempty]
    cs  -> [genSizeOfClause cs]

genSizeOfClause :: [TH.Con] -> TH.Clause
genSizeOfClause cs = do
    let conSizes   = ListE $ sizeOfCon <$> cs
        maxConSize = app2 (var 'maximumDef) (intLit 0) conSizes
        maxConSizePlusOne = plus maxConSize sizeOfInt
    clause [WildP] maxConSizePlusOne mempty

genAlignment :: TH.Dec
genAlignment = FunD 'Storable.alignment [genAlignmentClause]

genAlignmentClause :: TH.Clause
genAlignmentClause = clause [WildP] (app (var 'Storable.sizeOf) undefinedAsInt) mempty

genPeek :: [TH.Con] -> Q TH.Dec
genPeek cs = funD 'Storable.peek [genPeekClause cs]

genPeekCaseMatch :: Bool -> Name -> Integer -> TH.Con -> Q TH.Match
genPeekCaseMatch single ptr idx con = do
    (off0 :| offNames, whereCs) <- genOffsets con
    let (cName, arity)   = conNameArity con
        peekByteOffPtr   = app (var 'Storable.peekByteOff) (var ptr)
        peekByte off     = app peekByteOffPtr $ var off
        appPeekByte t x  = op '(<*>) t $ peekByte x
        -- No-field constructors are a special case of just the constructor being returned
        (firstCon, offs) = case offNames of
                (off1:os) -> (op '(<$>) (ConE cName) (peekByte $ if single then off0 else off1), os)
                _         -> (app (var 'return) (ConE cName), [])
        body             = NormalB $ foldl appPeekByte firstCon offs
        pat              = LitP $ IntegerL idx
    return $ Match pat body (NonEmpty.toList whereCs)

genPeekCatchAllMatch :: TH.Match
genPeekCatchAllMatch = TH.Match TH.WildP (TH.NormalB body) mempty
    where body = app (var 'error) (TH.LitE $ TH.StringL "[peek] Unrecognized constructor")

genPeekSingleCons :: Name -> TH.Con -> Q TH.Clause
genPeekSingleCons ptr con = do
    TH.Match _ body whereCs <- genPeekCaseMatch True ptr 0 con
    case body of
        TH.NormalB e  -> return $ clause [var ptr] e whereCs
        TH.GuardedB _ -> fail "[genPeekSingleCons] Guarded bodies not supported"

genPeekMultiCons :: Name -> Name -> [TH.Con] -> Q TH.Clause
genPeekMultiCons ptr tag cs = do
    peekCases <- zipWithM (genPeekCaseMatch False ptr) [0..] cs
    let peekTag      = app (app (var 'Storable.peekByteOff) (var ptr)) (intLit 0)
        peekTagTyped = peekTag -:: app (cons' ''IO) (cons' ''Int)
        bind         = BindS (var tag) peekTagTyped
        cases        = CaseE (var tag) $ peekCases <> [genPeekCatchAllMatch]
        doE          = DoE [bind, NoBindS cases]
    return $ clause [var ptr] doE mempty

genPeekClause :: [TH.Con] -> Q TH.Clause
genPeekClause cs = do
    ptr       <- newName "ptr"
    tag       <- newName "tag"
    case cs of
        []  -> fail "[genPeekClause] Phantom types not supported"
        [c] -> genPeekSingleCons ptr c
        cs  -> genPeekMultiCons ptr tag cs

genPoke :: [TH.Con] -> Q TH.Dec
genPoke conss = funD 'Storable.poke $ case conss of
    []  -> error "[genPoke] Phantom types not supported"
    [c] -> [genPokeClauseSingle c]
    cs  -> zipWith genPokeClauseMulti [0 ..] cs

nonEmptyParamNames :: Int -> Q (NonEmpty Name)
nonEmptyParamNames n = newNames n >>= \case
    (firstPat:restPats) -> return $ firstPat :| restPats
    -- TODO[piotrMocz] this could work, with some love
    _ -> fail "[genPokeClause] No-field constructors not supported"

genPokePat :: Name -> Name -> [Name] -> [TH.Pat]
genPokePat ptr cName patVarNames =
    [var ptr, cons cName $ var <$> patVarNames]

genPokeExpr :: Name -> NonEmpty Name -> [Name] -> TH.Exp -> TH.Exp
genPokeExpr ptr (off :| offNames) varNames firstExpr = body
    where pokeByteOffPtr = app (var 'Storable.pokeByteOff) (var ptr)
          pokeByte a     = app2 pokeByteOffPtr (var a)
          nextPoke t     = app2 (var '(>>)) t .: pokeByte
          firstPoke      = pokeByte off firstExpr
          varxps         = var <$> varNames
          body           = foldl (uncurry . nextPoke) firstPoke
                         $ zip offNames varxps

genPokeClauseSingle :: TH.Con -> Q TH.Clause
genPokeClauseSingle con = do
    let (cName, nParams) = conNameArity con
    ptr <- newName "ptr"
    patVarNames@(firstP :| restP) <- nonEmptyParamNames nParams
    (offNames, whereCs) <- genOffsets con
    let pat  = genPokePat  ptr cName $ NonEmpty.toList patVarNames
        body = genPokeExpr ptr offNames restP (var firstP)
    return $ clause pat body (NonEmpty.toList whereCs)

genPokeClauseMulti :: Integer -> TH.Con -> Q TH.Clause
genPokeClauseMulti idx con = do
    let (cName, nParams) = conNameArity con
    ptr         <- newName "ptr"
    patVarNames <- newNames nParams
    (offNames, whereCs) <- genOffsets con
    let pat            = genPokePat ptr cName patVarNames
        idxAsInt       = convert idx -:: cons' ''Int
        body           = genPokeExpr ptr offNames patVarNames idxAsInt
    return $ clause pat body (NonEmpty.toList whereCs)
