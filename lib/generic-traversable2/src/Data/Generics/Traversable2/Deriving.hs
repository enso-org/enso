{-# LANGUAGE CPP             #-}
{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Generics.Traversable2.Deriving where

import Prologue hiding (Traversable, Type, traverse)

import Language.Haskell.TH              as TH

import Data.Generics.Traversable2.Class ( Traversable, TraverseElem, traverse
                                        , traverseElem)
import Data.List                        ( nub )


err :: String -> a
err s = error $ "Data.Generics.Traversable.TH: " <> s

reifyDataInfo :: Name -> Q (Name, [TH.Type], [(Name, Int, [Type])])
reifyDataInfo name = reify name >>= \case
    TyConI d -> pure $ getDataInfo d
    _        -> err  $ "can't be used on anything but a type constructor "
                    <> "of an algebraic data type"

getDataInfo :: Dec -> (Name, [TH.Type], [(Name, Int, [Type])])
getDataInfo decl = case decl of
    DataD        _ n ps _ cs _ -> (n, map (TH.VarT . varName) ps, map conA cs)
    NewtypeD     _ n ps _ c  _ -> (n, map (TH.VarT . varName) ps, [conA c])
    DataInstD    _ n ps _ cs _ -> (n, ps, map conA cs)
    NewtypeInstD _ n ps _ c  _ -> (n, ps, [conA c])
    _ -> err ("not a data type declaration: " <> show decl)

-- | Return a lambda expression which implements 'traverse' for the given
-- data type.
-- gtraverseExpr :: Name -> Q Exp
-- gtraverseExpr typeName = makeGtraverseExpr =<< reifyDataInfo typeName

makeGtraverseExpr :: Name -> (Name, [TH.Type], [(Name, Int, [Type])]) -> Q Exp
makeGtraverseExpr tp (_name, _params, constructors) = do
    x <- newName "x"
    let f   = 'traverseElem
        lam = lamE [varP x] $ caseE (varE x) matches
        mkMatch (c, n, _) = do
            args <- replicateM n (newName "arg")
            let applyF e arg = varE '(<*>) `appE` e `appE`
                               ((appTypeE (varE f) (varT tp)) `appE` varE arg)
                body = foldl applyF [| $(varE 'pure) $(conE c) |] args
            match (conP c $ map (bangP . varP) args) (normalB body) []
        matches = map mkMatch constructors
    lam

-- | Example usage:
--
-- >data MyType = MyType
-- >
-- >deriveGTraversable ''MyType
--
-- It tries to create the necessary instance constraints, but is not very
-- smart about it For tricky types, it may fail or produce an
-- overconstrained instance. In that case, write the instance declaration
-- yourself and use 'gtraverseExpr' to derive the implementation:
--
-- >data MyType a = MyType
-- >
-- >instance Traversable (MyType a) where
-- >  traverse = $(gtraverseExpr ''MyType)
class    Derive a    where derive :: a -> Q [Dec]
instance Derive Name where derive = deriveByName
instance Derive Dec  where derive = deriveByDec

-- class    Derive1 a    where derive1 :: a -> Q [Dec]
-- instance Derive1 Name where derive1 = deriveByName1
-- instance Derive1 Dec  where derive1 = deriveByDec1

deriveByName :: Name -> Q [Dec]
deriveByName name = deriveGTraversableByInfo =<< reifyDataInfo name

deriveByDec :: Dec -> Q [Dec]
deriveByDec dec = deriveGTraversableByInfo $ getDataInfo dec

-- deriveByName1 :: Name -> Q [Dec]
-- deriveByName1 name = deriveGTraversableByInfo1 =<< reifyDataInfo name

-- deriveByDec1 :: Dec -> Q [Dec]
-- deriveByDec1 dec = deriveGTraversableByInfo1 $ getDataInfo dec

deriveGTraversableByInfo :: (Name, [TH.Type], [(Name, Int, [Type])]) -> Q [Dec]
deriveGTraversableByInfo info@(typeName, typeParams, constructors) = do
    tp <- newName "t"
    let ctx         = ''TraverseElem
        appliedType = foldl AppT (ConT typeName) typeParams
        body        = funD 'traverse [ clause [] (normalB $ makeGtraverseExpr tp info) [] ]
        pragma      = pragInlD 'traverse Inline FunLike AllPhases
        instHead    = conT ''Traversable `appT` varT tp `appT` pure appliedType
        inst        = instanceD context instHead [body, pragma]
        context     = sequence userContext
        types       = nub [ t | (_,_,ts) <- constructors, t <- ts ]
        userContext = [ conT ctx `appT` varT tp `appT` pure t | t <- types ]
    sequence [inst]

-- deriveGTraversableByInfo1 :: (Name, [TH.Type], [(Name, Int, [Type])]) -> Q [Dec]
-- deriveGTraversableByInfo1 info@(typeName, typeParams, constructors) = do
--     ctx <- newName "c"
--     let appliedType = foldl AppT (ConT typeName) typeParams
--         body        = funD 'traverse1 [ clause [] (normalB $ makeGtraverseExpr info) [] ]
--         pragma      = pragInlD 'traverse1 Inline FunLike AllPhases
--         instHead    = conT ''Traversable1 `appT` varT ctx `appT` pure appliedType
--         inst        = instanceD context instHead [body, pragma]
--         context     = sequence userContext
--         types       = nub [ t | (_,_,ts) <- constructors, t <- ts ]
--         userContext = [ varT ctx `appT` pure t | t <- types ]
--     sequence [inst]

conA :: Con -> (Name, Int, [Type])
conA (NormalC c xs)   = (c, length xs, map snd xs)
conA (InfixC x1 c x2) = conA (NormalC c [x1, x2])
conA (ForallC _ _ c)  = conA c
conA (RecC c xs)      = (c, length xs, map (\(_,_,t)->t) xs)
conA _                = err "GADTs are not supported yet"

varName :: TyVarBndr -> Name
varName (PlainTV n)    = n
varName (KindedTV n _) = n

