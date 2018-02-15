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

------------------------------
-- TH info extracting utils --
------------------------------

unpackCon :: Num a => TH.Con -> (Name, [Type])
unpackCon = \case
    NormalC n fs  -> (n, map snd fs)
    RecC    n fs  -> (n, map (\(_, _, x) -> x) fs)
    InfixC a n b  -> (n, [snd a, snd b])
    ForallC _ _ c -> unpackCon c
    _             -> error "***error*** deriveStorable: GADT constructors not supported"

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
sizeOfType tp = let undefinedAsT = sigE (return $ var 'undefined) (return tp)
                in  appQ (return $ var 'sizeOf) undefinedAsT

opWrong :: Name -> ExpQ -> ExpQ -> ExpQ
opWrong name lhs rhs = infixE (Just lhs) (return $ var name) (Just rhs)


op :: Name -> Exp -> Exp -> Exp
op = app2 . var

plus, mul :: ExpQ -> ExpQ -> ExpQ
plus = opWrong '(+)
mul  = opWrong '(*)

intLit :: Integer -> ExpQ
intLit = litE . integerL

undefinedAsInt :: ExpQ
undefinedAsInt = sigE (return $ var 'undefined) (conT ''Int)

conFieldSizes :: TH.Con -> [ExpQ]
conFieldSizes = map sizeOfType . extractConcreteTypes

sizeOfCon :: TH.Con -> ExpQ
sizeOfCon con
    | conArity con > 0 = foldl1 plus $ conFieldSizes con
    | otherwise        = intLit 0

align :: ExpQ
align = appQ (return $ var 'alignment) $ undefinedAsInt

whereClause :: Name -> ExpQ -> DecQ
whereClause n e = valD (return $ var n) (normalB e) []

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
        let off1D   = whereClause (names !! 1) $ appQ (return $ var 'sizeOf) undefinedAsInt
            headers = zip3 (drop 2 names) (tail names) fSizes

            mkDecl :: (Name, Name, ExpQ) -> DecQ
            mkDecl (declName, refName, fSize) =
                whereClause declName (plus (return $ var refName) fSize) --  where declName = refName + size

            clauses = (off0D : off1D : (map mkDecl headers))

        return (names, clauses)


genSizeOf :: [TH.Con] -> Q TH.Dec
genSizeOf cs = funD 'sizeOf [genSizeOfClause cs]

genSizeOfClause :: [TH.Con] -> ClauseQ
genSizeOfClause cs = do
    let sizeOfInt  = appQ (return $ var 'sizeOf) undefinedAsInt
        conSizes   = listE $ map sizeOfCon cs
        maxConSize = appQ (return $ var 'maximum) conSizes
        maxConSizePlusOne = plus maxConSize sizeOfInt
        body       = normalB $ maxConSizePlusOne
    TH.clause [wildP] body []

genAlignment :: DecQ
genAlignment = funD 'alignment [genAlignmentClause]

genAlignmentClause :: ClauseQ
genAlignmentClause = let body = normalB $ appQ (return $ var 'sizeOf) undefinedAsInt
                     in  TH.clause [wildP] body []

genPeek :: [TH.Con] -> DecQ
genPeek cs = funD 'peek [genPeekClause cs]

genPeekCaseMatch :: Name -> (Integer, TH.Con) -> MatchQ
genPeekCaseMatch ptr (idx, con) = do
    -- FIXME[WD -> PM]: use proper TODO comments syntax
    -- we can be dead sure the offsets/wheres contain at least 2 elems
    -- BUT we need to handle no-param type constructors, huh  TODO[piotrMocz]
    (_:offNames, whereCs) <- genOffsets con
    let (cName, arity) = conInfo con
        peekByteOffPtr   = appQ (return $ var 'peekByteOff) (return $ var ptr)
        peekByte off     = appQ peekByteOffPtr $ return $ var off
        appPeekByte t x  = opWrong '(<*>) t $ peekByte x
        -- no-field constructors are a special case of just the constructor being returned
        firstCon         = if arity > 0 then opWrong '(<$>) (conE cName) (peekByte $ head offNames)
                                        else appQ (return $ var 'return) (conE cName)
        offs             = if arity > 0 then tail offNames else []
        body = normalB $ foldl appPeekByte firstCon offs
        pat  = litP $ integerL idx
    match pat body whereCs

genPeekClause :: [TH.Con] -> ClauseQ
genPeekClause cs = do
    ptr <- newName "ptr"
    tag <- newName "tag"
    let v            = return $ var ptr
        peekTag      = appQ (appQ (return $ var 'peekByteOff) (return $ var ptr)) (intLit 0)
        peekTagTyped = sigE peekTag (appT (conT ''IO) (conT ''Int))
        bind         = bindS (return $ var tag) peekTagTyped
        cases        = caseE (return $ var tag) $ map (genPeekCaseMatch ptr) $ zip [0..] cs
        doBlock      = doE [bind, noBindS cases]
    TH.clause [v] (normalB doBlock) []

genPoke :: [TH.Con] -> DecQ
genPoke = funD 'poke . map (uncurry genPokeClause) . zip [0..]

genPokeClause :: Integer -> TH.Con -> ClauseQ
genPokeClause idx con = do
    let (cName, nParams) = conInfo con
    ptr         <- newName "ptr"
    patVarNames <- newNames nParams
    (off:offNames, whereClauses') <- genOffsets con
    whereClauses <- sequence whereClauses' -- FIXME: make it pure!
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
