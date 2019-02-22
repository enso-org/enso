module Data.Convert2.TH where

import Prelude

import Language.Haskell.TH 

maxKind :: Int
maxKind = 5

appsT :: Foldable f => Type -> f Type -> Type
appsT = foldl (AppT)

showIfNot0 :: Int -> String
showIfNot0 i = if i == 0 then "" else show i

ixedName :: String -> Int -> String
ixedName s i = s <> showIfNot0 i

ixedName' :: String -> Int -> String
ixedName' s i = ixedName s i <> "'"

src, tgt :: Name
src  = mkName "src"
tgt = mkName "tgt"

type IString = Int -> String

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- > class Convertible1 src tgt where convert1 :: ∀ b. src b -> tgt
genConvertibleClass :: Bool -> IString -> IString -> Int -> Dec
genConvertibleClass inv sname sfname i = cls where
    name   = mkName $ sname  i
    fname  = mkName $ sfname i
    vnames = mkName . ("t" <>) . show <$> [1 .. i]
    binds  = ForallT (PlainTV <$> vnames) []
    (arg,result) = (if inv then swap else id)
                 $ (appsT (VarT src) (VarT <$> vnames), VarT tgt)
    body  = SigD fname (binds $ AppT (AppT ArrowT arg) result)
    cls   = ClassD [] name [PlainTV tgt, PlainTV src] [] [body]

-- > instance {-# OVERLAPPABLE #-} Convertible2 src tgt
-- >       => Convertible1 (src t) tgt where
-- >    convert1 = convert2
-- >    {-# INLINE convert1 #-}
genHigherKindDefInstance :: IString -> IString -> Int -> Dec
genHigherKindDefInstance sname sfname i = inst where
    name    = mkName $ sname  i
    fname   = mkName $ sfname i
    nameH   = mkName $ sname  (i + 1)
    fnameH  = mkName $ sfname (i + 1)
    overlap = Just Overlappable
    ctx     = [AppT (AppT (ConT nameH) (VarT tgt)) (VarT src)]
    body    = ValD (VarP fname) (NormalB (VarE fnameH)) []
    inline  = PragmaD (InlineP fname Inline FunLike AllPhases)
    arg     = appsT (VarT src) [VarT (mkName "t")]
    inst    = InstanceD overlap ctx (AppT (AppT (ConT name) (VarT tgt)) arg) 
              [body, inline]

-- > instance TypeError (IdConversionErr src) 
-- >       => Convertible1 src (src t1) where
-- >     convert1 = impossible
-- >     {-# INLINE convert1 #-}
genIdConversionErrorInstance :: IString -> IString -> Int -> Dec
genIdConversionErrorInstance sname sfname i = inst where
    name    = mkName $ sname  i
    fname   = mkName $ sfname i
    vnames  = mkName . ("t" <>) . show <$> [1 .. i]
    err     = AppT (ConT (mkName "TypeError")) 
            $ AppT (ConT (mkName "IdConversionErr")) 
            $ VarT src
    overlap = Nothing
    ctx     = [err]
    body    = ValD (VarP fname) (NormalB (VarE $ mkName "impossible")) []
    inline  = PragmaD (InlineP fname Inline FunLike AllPhases)
    arg     = appsT (VarT src) (VarT <$> vnames)
    inst    = InstanceD overlap ctx (AppT (AppT (ConT name) arg) (VarT src)) 
              [body, inline]

genConvertibleClasses :: Bool -> IString -> IString -> Q [Dec]
genConvertibleClasses inv sname sfname = pure $ genConvertibleClass inv sname sfname <$> [0 .. maxKind]

genHigherKindDefInstances :: IString -> IString -> Q [Dec]
genHigherKindDefInstances sname sfname = pure $ genHigherKindDefInstance sname sfname <$> [0 .. maxKind - 1]

genIdConversionErrorInstances :: IString -> IString -> Q [Dec]
genIdConversionErrorInstances sname sfname = pure $ genIdConversionErrorInstance sname sfname <$> [0 .. maxKind]