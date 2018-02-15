{-# LANGUAGE TemplateHaskell #-}
module Foreign.Storable.Deriving where

import Prologue ((.:))
import Data.Convert
import Foreign.Storable
import Foreign.Storable.TH()
import Language.Haskell.TH.Lib hiding (clause)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH hiding (clause)
import Prelude
import Control.Monad
import GHC.Num
import System.IO.Unsafe
import Control.Monad.IO.Class

import Data.List (maximum)


import Language.Haskell.TH.Builder


thd :: (a, b, c) -> c
thd (_, _, x) = x

--------------------------------------
-- === TH info extracting utils === --
--------------------------------------

unpackCon :: TH.Con -> (Name, [Type])
unpackCon = \case
    NormalC n fs  -> (n, map snd fs)
    RecC    n fs  -> (n, map (\(_, _, x) -> x) fs)
    InfixC a n b  -> (n, [snd a, snd b])
    ForallC _ _ c -> unpackCon c
    _             -> error "***error*** deriveStorable: GADT constructors not supported"

-- | Extract the name and number of params from the consturctor
conInfo :: Num a => TH.Con -> (Name, a)
conInfo c = let (n, fs) = unpackCon c in (n, fromIntegral $ length fs)

-- | Extract the number of params from the constructor
conArity :: Num a => TH.Con -> a
conArity = snd . conInfo

concretizeType :: Type -> Type
concretizeType = \case
    ConT n   -> ConT n
    VarT _   -> ConT ''Int
    AppT l r -> AppT (concretizeType l) (concretizeType r)
    _        -> error "***error*** deriveStorable: only reasonably complex types supported"

-- | Instantiate all the free type variables to Int for a consturctor
extractConcreteTypes :: TH.Con -> [Type]
extractConcreteTypes = \case
    NormalC n bts -> map (concretizeType . snd) bts
    RecC    n bts -> map (concretizeType . thd) bts
    _ -> error "***error*** deriveStorable: type not yet supported"

-------------------------------------
-- === TH convenience wrappers === --
-------------------------------------

sizeOfType :: Type -> Exp
sizeOfType = app (var 'sizeOf) . ((var 'undefined) -::)

sizeOfInt :: Exp
sizeOfInt = sizeOfType $ var ''Int

op :: Name -> Exp -> Exp -> Exp
op = app2 . var

plus, mul :: Exp -> Exp -> Exp
plus = op '(+)
mul  = op '(*)

intLit :: Integer -> Exp
intLit = LitE . IntegerL

undefinedAsInt :: Exp
undefinedAsInt = (var 'undefined) -:: (var ''Int)

conFieldSizes :: TH.Con -> [Exp]
conFieldSizes = map sizeOfType . extractConcreteTypes

sizeOfCon :: TH.Con -> Exp
sizeOfCon con
    | conArity con > 0 = foldl1 plus $ conFieldSizes con
    | otherwise        = intLit 0

align :: Exp
align = app (var 'alignment) $ undefinedAsInt

whereClause :: Name -> Exp -> Dec
whereClause n e = ValD (var n) (NormalB e) []

--------------------------------
-- === Main instance code === --
--------------------------------

deriveStorable :: Name -> Q [TH.Dec]
deriveStorable ty = do
    (TyConI tyCon) <- reify ty
    let (tyConName, tyVars, cs) = case tyCon of
            DataD    _ nm tyVars _ cs _ -> (nm, tyVars, cs)
            NewtypeD _ nm tyVars _ c  _ -> (nm, tyVars, [c])
            _ -> error "***error*** deriveStorable: type may not be a type synonym."
        instanceType              = appT (conT ''Storable) (foldl apply (conT tyConName) tyVars)
        apply t (PlainTV name)    = appT t (varT name)
        apply t (KindedTV name _) = appT t (varT name)

    instanceCxt <- mapM (apply $ conT ''Storable) tyVars
    sequence [instanceD (return []) instanceType [return $ genSizeOf cs, return genAlignment, genPeek cs, genPoke cs]]

-------------------------------
-- === Method generators === --
-------------------------------

-- | Generate the offsets for a constructor (also returns the names of the variables in wheres).
-- Example:
-- > data T = Cons x y
-- >
-- > [...] where off0 = 0
-- >             off1 = sizeOf (undefined :: Int)
-- >             off2 = off1 + sizeOf (undefined :: x)
-- >             off3 = off2 + sizeOf (undefined :: y)
genOffsets :: TH.Con -> Q ([Name], [Dec])
genOffsets con = do
    let fSizes  = conFieldSizes con
        arity   = length fSizes
        name i  = newName $ "off" ++ show i

    -- this needs to bind, because it would generate new names every time
    names <- mapM name $ take (arity + 1) [0..]

    let off0D   = whereClause (head names) $ intLit 0
    if arity == 0 then return (names, [off0D]) else do
        let off1D   = whereClause (names !! 1) $ app (var 'sizeOf) undefinedAsInt
            headers = zip3 (drop 2 names) (tail names) fSizes

            mkDecl :: (Name, Name, Exp) -> Dec
            mkDecl (declName, refName, fSize) =
                whereClause declName (plus (var refName) fSize) --  where declName = refName + size

            clauses = (off0D : off1D : (map mkDecl headers))

        return (names, clauses)


genSizeOf :: [TH.Con] -> TH.Dec
genSizeOf cs = FunD 'sizeOf [genSizeOfClause cs]

genSizeOfClause :: [TH.Con] -> TH.Clause
genSizeOfClause cs = do
    let conSizes   = ListE $ map sizeOfCon cs
        maxConSize = app (var 'maximum) conSizes
        maxConSizePlusOne = plus maxConSize sizeOfInt
    clause [WildP] maxConSizePlusOne []

genAlignment :: TH.Dec
genAlignment = FunD 'alignment [genAlignmentClause]

genAlignmentClause :: TH.Clause
genAlignmentClause = clause [WildP] (app (var 'sizeOf) undefinedAsInt) []

genPeek :: [TH.Con] -> Q TH.Dec
genPeek cs = funD 'peek [genPeekClause cs]

genPeekCaseMatch :: Name -> Integer -> TH.Con -> Q Match
genPeekCaseMatch ptr idx con = do
    (_:offNames, whereCs) <- genOffsets con
    let (cName, arity)   = conInfo con
        peekByteOffPtr   = app (var 'peekByteOff) (var ptr)
        peekByte off     = app peekByteOffPtr $ var off
        appPeekByte t x  = op '(<*>) t $ peekByte x
        -- no-field constructors are a special case of just the constructor being returned
        firstCon         = if arity > 0 then op '(<$>) (ConE cName) (peekByte $ head offNames)
                                        else app (var 'return) (ConE cName)
        offs             = if arity > 0 then tail offNames else []
        body             = NormalB $ foldl appPeekByte firstCon offs
        pat              = LitP $ IntegerL idx
    return $ Match pat body whereCs

genPeekClause :: [TH.Con] -> Q TH.Clause
genPeekClause cs = do
    ptr       <- newName "ptr"
    tag       <- newName "tag"
    peekCases <- mapM (uncurry $ genPeekCaseMatch ptr) $ zip [0..] cs
    let peekTag      = app (app (var 'peekByteOff) (var ptr)) (intLit 0)
        peekTagTyped = peekTag -:: (app (var ''IO) (var ''Int))
        bind         = BindS (var tag) peekTagTyped
        cases        = CaseE (var tag) peekCases
        doBlock      = DoE [bind, NoBindS cases]
    return $ clause [var ptr] doBlock []

genPoke :: [TH.Con] -> DecQ
genPoke = funD 'poke . map (uncurry genPokeClause) . zip [0..]

genPokeClause :: Integer -> TH.Con -> Q TH.Clause
genPokeClause idx con = do
    let (cName, nParams) = conInfo con
    ptr         <- newName "ptr"
    patVarNames <- newNames nParams
    (off:offNames, whereClauses) <- genOffsets con
    let pat            = [var ptr, cons cName $ var <$> patVarNames]
        pokeByteOffPtr = app (var 'pokeByteOff) (var ptr)
        pokeByte a     = app2 pokeByteOffPtr (var a)
        nextPoke t     = app2 (var '(>>)) t .: pokeByte
        idxAsInt       = convert idx -:: cons ''Int [] -- FIXME: it's strange. It seems like we do a lot of it, check if necessary
        firstPoke      = pokeByte off idxAsInt
        varxps         = var <$> patVarNames
        body           = foldl (uncurry . nextPoke) firstPoke
                       $ zip offNames varxps
    return $ clause pat body whereClauses
