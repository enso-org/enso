{-# LANGUAGE TemplateHaskell #-}
module Foreign.Storable.Deriving where

import Foreign.Storable
import Foreign.Storable.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH as TH
import Prelude
import Control.Monad
import GHC.Num
import System.IO.Unsafe
import Control.Monad.IO.Class

import Data.List (maximum)


thd :: (a, b, c) -> c
thd (_, _, x) = x

------------------------------
-- TH info extracting utils --
------------------------------

unpackCon :: Num a => TH.Con -> (Name, [Type])
unpackCon (NormalC n fs)  = (n, map snd fs)
unpackCon (RecC    n fs)  = (n, map (\(_, _, x) -> x) fs)
unpackCon (InfixC a n b)  = (n, [snd a, snd b])
unpackCon (ForallC _ _ c) = unpackCon c
unpackCon _               = error "***error*** deriveStorable: GADT constructors not supported"

-- | extract the name and number of params from the consturctor
conInfo :: Num a => TH.Con -> (Name, a)
conInfo c = let (n, fs) = unpackCon c in (n, fromIntegral $ length fs)

-- | extract the number of params from the constructor
conArity :: Num a => TH.Con -> a
conArity = snd . conInfo

concretizeType :: Type -> Type
concretizeType (ConT n)   = (ConT n)
concretizeType (VarT _)   = ConT ''Int
concretizeType (AppT l r) = AppT (concretizeType l) (concretizeType r)
concretizeType _          = error "***error*** deriveStorable: only reasonably complex types supported"

-- | instantiate all the free type variables to Int for a consturctor
extractConcreteTypes :: TH.Con -> [Type]
extractConcreteTypes (NormalC n bts) = map (concretizeType . snd) bts
extractConcreteTypes (RecC    n bts) = map (concretizeType . thd) bts
extractConcreteTypes _ = error "***error*** deriveStorable: type not yet supported"

-----------------------------
-- TH convenience wrappers --
-----------------------------

sizeOfType :: Type -> ExpQ
sizeOfType tp = let undefinedAsT = sigE (varE 'undefined) (return tp)
                in  appE (varE 'sizeOf) undefinedAsT

nNewNames :: Int -> Q [Name]
nNewNames n = mapM (\x -> newName [x]) $ take n ['a'..]

op :: Name -> ExpQ -> ExpQ -> ExpQ
op name lhs rhs = infixE (Just lhs) (varE name) (Just rhs)

plus, mul :: ExpQ -> ExpQ -> ExpQ
plus = op '(+)
mul  = op '(*)

intLit :: Integer -> ExpQ
intLit = litE . integerL

undefinedAsInt :: ExpQ
undefinedAsInt = sigE (varE 'undefined) (conT ''Int)

conFieldSizes :: TH.Con -> [ExpQ]
conFieldSizes = map sizeOfType . extractConcreteTypes

sizeOfCon :: TH.Con -> ExpQ
sizeOfCon con
    | conArity con > 0 = foldl1 plus $ conFieldSizes con
    | otherwise        = intLit 0

align :: ExpQ
align = appE (varE 'alignment) $ undefinedAsInt

whereClause :: Name -> ExpQ -> DecQ
whereClause n e = valD (varP n) (normalB e) []

------------------------
-- Main instance code --
------------------------

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
    sequence [instanceD (return []) instanceType [genSizeOf cs, genAlignment, genPeek cs, genPoke cs]]

------------------------
-- Method generators  --
------------------------

-- | generate the offsets for a constructor (also returns the names of the variables in wheres).
-- Example:
-- ```
-- data T = Cons x y
--
-- [...] where off0 = 0
--             off1 = sizeOf (undefined :: Int)
--             off2 = off1 + sizeOf (undefined :: x)
--             off3 = off2 + sizeOf (undefined :: y)
-- ```
genOffsets :: TH.Con -> Q ([Name], [DecQ])
genOffsets con = do
    let fSizes  = conFieldSizes con
        arity   = length fSizes
        name i  = newName $ "off" ++ show i

    -- this needs to bind, because it would generate new names every time
    names <- mapM name $ take (arity + 1) [0..]

    let off0D   = whereClause (head names) $ intLit 0
    if arity == 0 then return (names, [off0D]) else do
        let off1D   = whereClause (names !! 1) $ appE (varE 'sizeOf) undefinedAsInt
            headers = zip3 (drop 2 names) (tail names) fSizes

            mkDecl :: (Name, Name, ExpQ) -> DecQ
            mkDecl (declName, refName, fSize) =
                whereClause declName (plus (varE refName) fSize) --  where declName = refName + size

            clauses = (off0D : off1D : (map mkDecl headers))

        return (names, clauses)


genSizeOf :: [TH.Con] -> Q TH.Dec
genSizeOf cs = funD 'sizeOf [genSizeOfClause cs]

genSizeOfClause :: [TH.Con] -> ClauseQ
genSizeOfClause cs = do
    let sizeOfInt  = appE (varE 'sizeOf) undefinedAsInt
        conSizes   = listE $ map sizeOfCon cs
        maxConSize = appE (varE 'maximum) conSizes
        maxConSizePlusOne = plus maxConSize sizeOfInt
        body       = normalB $ maxConSizePlusOne
    clause [wildP] body []

genAlignment :: DecQ
genAlignment = funD 'alignment [genAlignmentClause]

genAlignmentClause :: ClauseQ
genAlignmentClause = let body = normalB $ appE (varE 'sizeOf) undefinedAsInt
                     in  clause [wildP] body []

genPeek :: [TH.Con] -> DecQ
genPeek cs = funD 'peek [genPeekClause cs]

genPeekCaseMatch :: Name -> (Integer, TH.Con) -> MatchQ
genPeekCaseMatch ptr (idx, con) = do
    -- we can be dead sure the offsets/wheres contain at least 2 elems
    -- BUT we need to handle no-param type constructors, huh  TODO[piotrMocz]
    (_:offNames, whereCs) <- genOffsets con
    let (cName, arity) = conInfo con
        peekByteOffPtr   = appE (varE 'peekByteOff) (varE ptr)
        peekByte off     = appE peekByteOffPtr $ varE off
        appPeekByte t x  = op '(<*>) t $ peekByte x
        -- no-field constructors are a special case of just the constructor being returned
        firstCon         = if arity > 0 then op '(<$>) (conE cName) (peekByte $ head offNames)
                                        else appE (varE 'return) (conE cName)
        offs             = if arity > 0 then tail offNames else []
        body = normalB $ foldl appPeekByte firstCon offs
        pat  = litP $ integerL idx
    match pat body whereCs

genPeekClause :: [TH.Con] -> ClauseQ
genPeekClause cs = do
    ptr <- newName "ptr"
    tag <- newName "tag"
    let v            = varP ptr
        peekTag      = appE (appE (varE 'peekByteOff) (varE ptr)) (intLit 0)
        peekTagTyped = sigE peekTag (appT (conT ''IO) (conT ''Int))
        bind         = bindS (varP tag) peekTagTyped
        cases        = caseE (varE tag) $ map (genPeekCaseMatch ptr) $ zip [0..] cs
        doBlock      = doE [bind, noBindS cases]
    clause [v] (normalB doBlock) []

genPoke :: [TH.Con] -> DecQ
genPoke = funD 'poke . map genPokeClause . zip [0..]

genPokeClause :: (Integer, TH.Con) -> ClauseQ
genPokeClause (idx, con) = do
    let (cName, nParams) = conInfo con
    ptr         <- newName "ptr"
    patVarNames <- nNewNames nParams
    (off:offNames, whereClauses) <- genOffsets con
    let pat                  = [varP ptr, conP cName $ map varP patVarNames]
        pokeByteOffPtr       = appE (varE 'pokeByteOff) (varE ptr)
        pokeByte o           = appE (appE pokeByteOffPtr $ varE o)
        appPokeByte t (o, p) = op '(>>) t $ pokeByte o p
        idxAsInt             = sigE (intLit idx) (conT ''Int)
        firstPoke            = pokeByte off idxAsInt
        varExps              = map varE patVarNames
        body                 = normalB $ foldl appPokeByte firstPoke
                                       $ zip offNames varExps
    clause pat body whereClauses
