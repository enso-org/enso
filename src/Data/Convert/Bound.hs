{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Convert.Bound where

import Data.Convert.Base
import Control.Applicative
import Language.Haskell.TH hiding (Type, Safety, Safe, Unsafe)
import Data.Monoid
import Data.Word
import GHC.Int
import Prelude hiding (Bounded, maxBound, minBound)
import qualified Prelude as Prelude




-- === The runtime used safe conversion ===

-- TODO [wd]: We can optimize it further. Right now we are converting everything to Rational and comparing.
--            It's because sometimes we are converting between types of different bounds like Char and Int8
--            - neither can we safely convert Char to Int 8 nor vice versa.
--            We can though make typeclasses that will find the smallest super-type for a given type pair
--            and convert both values to that super-type
boundedConversion :: (Bounded b, Convertible a Rational, Convertible (Bounds b) (Bounds Rational))
                  => (a -> b) -> (a -> Either BoundError b)
boundedConversion (func :: a -> b) inp = if (convert inp) `boundedBy` (convert (bounds :: Bounds b) :: Bounds Rational)
    then Right $ func inp
    else Left BoundError




-- === Errors ===

data BoundError = BoundError deriving (Show)


-- === Conversion ===

data Conversion = Conversion (Q Exp) Type Type

class Conversions a b where
    conversions :: Q Exp -> a -> b -> [Conversion]

-- utils

genConversion :: Conversion -> Q Dec
genConversion (Conversion qexp (Type name bounds) (Type name' bounds')) = do
    exp' <- qexp :: Q Exp
    let convf name' fmod = [ValD (VarP $ mkName name') (NormalB $ fmod exp') []]
    return $ if bounds `isSubBound` bounds'
#if __GLASGOW_HASKELL__ >= 800
        then InstanceD Nothing [] (AppT (AppT (ConT $ mkName "Convertible") (ConT name)) (ConT name')) $ convf "convert" id
        else InstanceD Nothing [] (AppT (AppT (AppT (ConT $ mkName "MaybeConvertible") (ConT name)) (ConT $ mkName "BoundError")) (ConT name')) $ convf "tryConvert" $ AppE (VarE $ mkName "boundedConversion")
#else
        then InstanceD [] (AppT (AppT (ConT $ mkName "Convertible") (ConT name)) (ConT name')) $ convf "convert" id
        else InstanceD [] (AppT (AppT (AppT (ConT $ mkName "MaybeConvertible") (ConT name)) (ConT $ mkName "BoundError")) (ConT name')) $ convf "tryConvert" $ AppE (VarE $ mkName "boundedConversion")
#endif

genConversions :: [Conversion] -> Q [Dec]
genConversions = mapM genConversion

-- instances

instance Conversions [Type] [Type] where conversions f a b = Conversion f <$> a <*> b
instance Conversions Type   [Type] where conversions f a b = Conversion f a <$> b
instance Conversions [Type] Type   where conversions f a b = flip (Conversion f) b <$> a
instance Conversions Type   Type   where conversions f a b = [Conversion f a b]

-- === Type ===

data Type = Type { name    :: Name
                 , tbounds :: (Bounds Integer)
                 } deriving (Show, Eq)

-- === Layout ===
data Layout = IntLayout Sign Integer
            | InfiniteLayout
            deriving (Show, Eq)

data Sign = Signed
          | Unsigned
          deriving (Show, Eq)

layoutBounds :: Layout -> Bounds Integer
layoutBounds = \case
    InfiniteLayout -> infiniteBounds
    IntLayout s i  -> Bounds (Value (- base)) (Value $ base - 1)
                      where base = case s of
                                   Signed   -> 2 ^ (i - 1)
                                   Unsigned -> 2 ^ i

-- === Value ===

data Value a = MinusInfinity
             | Value a
             | Infinity
             deriving (Show, Functor, Eq)

-- instances

instance Num a => Num (Value a) where
    fromInteger = Value . fromInteger

instance Convertible a b => Convertible (Value a) (Value b) where
    convert (Value a) = Value $ convert a
    convert MinusInfinity = MinusInfinity
    convert Infinity = Infinity

instance Ord a => Ord (Value a) where
    compare MinusInfinity MinusInfinity = EQ
    compare MinusInfinity _             = LT
    compare _             MinusInfinity = GT
    compare Infinity      Infinity      = EQ
    compare Infinity      _             = GT
    compare _             Infinity      = LT
    compare (Value a)     (Value a')    = compare a a'




-- === Bounds ===

data Bounds a = Bounds (Value a) (Value a) deriving (Show, Functor, Eq)

class Bounded a where
    bounds :: Bounds a

-- utils

infiniteBounds :: Bounds a
infiniteBounds = Bounds MinusInfinity Infinity

isSubBound :: Ord a => Bounds a -> Bounds a -> Bool
isSubBound (Bounds min' max') (Bounds min'' max'') = min' >= min'' && max' <= max''

boundedBy :: Ord a => a -> Bounds a -> Bool
boundedBy (Value -> a) (Bounds min' max') = a >= min' && a <= max'

-- instances
instance {-# OVERLAPPABLE #-} Convertible a b => Convertible (Bounds a) (Bounds b) where
    convert (Bounds a b) = Bounds (convert a) (convert b)

instance {-# OVERLAPPABLE #-} (Prelude.Bounded a)
      => Bounded a where
    bounds = Bounds (Value Prelude.minBound) (Value Prelude.maxBound)

instance {-# OVERLAPPABLE #-} Bounded Float    where bounds = infiniteBounds
instance {-# OVERLAPPABLE #-} Bounded Double   where bounds = infiniteBounds
instance {-# OVERLAPPABLE #-} Bounded Rational where bounds = infiniteBounds
instance {-# OVERLAPPABLE #-} Bounded Integer  where bounds = infiniteBounds

intTypes, wordTypes, floatTypes, integralTypes :: [Type]
intTypes   = [ Type ''Int      $ layoutBounds $ IntLayout Signed 30 -- according to: https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Int.html#t:Int
             , Type ''Integer  $ layoutBounds $ InfiniteLayout
             , Type ''Int8     $ layoutBounds $ IntLayout Signed 8
             , Type ''Int16    $ layoutBounds $ IntLayout Signed 16
             , Type ''Int32    $ layoutBounds $ IntLayout Signed 32
             , Type ''Int64    $ layoutBounds $ IntLayout Signed 64
             ]

wordTypes  = [ Type ''Word     $ layoutBounds $ IntLayout Signed 30 -- according to: https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Int.html#t:Int
             , Type ''Word8    $ layoutBounds $ IntLayout Signed 8
             , Type ''Word16   $ layoutBounds $ IntLayout Signed 16
             , Type ''Word32   $ layoutBounds $ IntLayout Signed 32
             , Type ''Word64   $ layoutBounds $ IntLayout Signed 64
             ]

floatTypes = [ Type ''Float    $ layoutBounds $ InfiniteLayout
             , Type ''Double   $ layoutBounds $ InfiniteLayout
             , Type ''Rational $ layoutBounds $ InfiniteLayout
             ]

integralTypes = intTypes <> wordTypes

charType :: Type
charType = Type ''Char $ Bounds 0 1114111 -- max char enum is 1114111

numConversions :: [Conversion]
numConversions =  conversions [| fromIntegral            |] integralTypes (integralTypes <> floatTypes)
               <> conversions [| truncate                |] floatTypes    integralTypes
               <> conversions [| realToFrac              |] floatTypes    floatTypes
               <> conversions [| fromIntegral . fromEnum |] charType      (integralTypes <> floatTypes)
               <> conversions [| toEnum . fromIntegral   |] integralTypes charType
