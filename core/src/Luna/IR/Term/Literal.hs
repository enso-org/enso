{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Luna.IR.Term.Literal where

import           Prologue hiding (String)
import qualified Prologue as P

import qualified Data.Char                             as Char
import qualified Data.Graph.Component.Node.Class       as Term
import qualified Data.Mutable.Storable.SmallAutoVector as SmallVector
import qualified Luna.IR.Term.Format                   as Format
import qualified OCI.IR.Term.Definition                as Term

import OCI.IR.Term.Class            (Terms)
import OCI.IR.Term.Definition       (LinksTo)

-- FIXME: Storable.derive doesnt support Nat literals
type V16 = SmallVector.UnmanagedSmallVector 16


--------------------
-- === Number === --
--------------------

-- === Definition === --

Term.define [d|
 data Value
    = Number    { base     :: Word8
                , intPart  :: V16  Word8
                , fracPart :: V16  Word8  }
    | RawString { val      :: V16  Char   }
    | FmtString { segments :: LinksTo Terms }
 |]



-- === API === --

prettyShow :: MonadIO m => Term.Constructor Number a -> m P.String
prettyShow (Number base intPart fracPart) = do
    intPartS  <- showVec intPart
    fracPartS <- showVec fracPart
    let frac = if fracPartS /= "" then "." <> fracPartS else mempty
    pure . pfx $ intPartS <> frac
    where showVec :: MonadIO m => V16 Word8 -> m P.String
          showVec     = fmap (concat . fmap showDigit) . SmallVector.toList
          showDigit d = if d < 10
              then show d
              else [Char.chr $ Char.ord 'a' + convert d - 10]
          pfx  = (<>) $ case base of
              2  -> "0b"
              8  -> "0o"
              10 -> ""
              16 -> "0x"
              _  -> error "unsupported base"

isInteger :: MonadIO m => Term.Constructor Number a -> m Bool
isInteger (Number _ _ fracPart) = null <$> SmallVector.toList fracPart
{-# INLINE isInteger #-}

toInteger :: MonadIO m => Term.Constructor Number a -> m Integer
toInteger (Number base' digits' _) = do
    digits <- fromIntegral <<$>> SmallVector.toList digits'
    let base = fromIntegral base'
    pure $ sum $ zipWith (\digit pow -> digit * base ^ pow)
                           (reverse digits)
                           ([0..] :: [Integer])

toDouble :: MonadIO m => Term.Constructor Number a -> m Double
toDouble (Number base' int' frac') = do
    let base = fromIntegral base'
    int  <- fromIntegral <<$>> SmallVector.toList int'
    frac <- fromIntegral <<$>> SmallVector.toList frac'
    let intPart = sum $ zipWith (\digit pow -> digit * base ^^ pow)
                                (reverse int)
                                ([0..] :: [Integer])
        fracPart = sum $ zipWith (\digit pow -> digit / base ^^ pow)
            frac ([1..] :: [Integer])
    pure $ intPart + fracPart



----------------------
-- === Literals === --
----------------------

-- type family LiteralOf a
-- class HasLiteral a where
--     literal :: Lens' a (LiteralOf a)








-- -- === Creation === --

-- int, bin, oct, hex, frac :: String -> Number
-- int  i = Number Dec i mempty mempty
-- bin  i = Number Bin i mempty mempty
-- oct  i = Number Oct i mempty mempty
-- hex  i = Number Hex i mempty mempty
-- frac i = Number Dec mempty i mempty


-- -- === Utils === --

-- isInteger :: Number -> Bool
-- isInteger n = n ^. fracPart == "" && n ^. expPart == ""

-- toInt :: Number -> Integer
-- toInt = read . view intPart

-- toDouble :: Number -> Double
-- toDouble x = read $ x ^. intPart
--                 <> (if x ^. fracPart . to null . to not then "." <> x ^. fracPart else "")
--                 <> (if not $ null (x ^. expPart) then "e" <> x ^. expPart else "")

-- fromDouble :: Double -> Number
-- fromDouble d = Number Dec i f mempty where
--     i = takeWhile (/= '.') $ show d
--     f = drop 1 $ dropWhile (/= '.') $ show d

-- -- === Instances === --

-- instance Show Number where
--     show n = pfx $ int <> frac <> exp where
--         int  = n ^. intPart
--         frac = if_ (n ^. fracPart /= "") $ "." <> n ^. fracPart
--         exp  = if_ (n ^. expPart  /= "") $ "e" <> n ^. expPart
--         pfx  = (<>) $ case n ^. base of
--             Dec -> ""
--             Bin -> "0b"
--             Oct -> "0o"
--             Hex -> "0x"

-- -- FIXME[WD]: do we need the real Eq & Ord here?
-- deriving instance Eq  Number
-- deriving instance Ord Number
-- deriving instance Ord Base

-- instance Num Number where
--     fromInteger = int . show

-- ---------------
-- -- FmtString --
-- ---------------

-- data FmtSegment a = StrSegment  a
--                   | ExprSegment a
--                   deriving (Show, Eq, Functor, Traversable, Foldable)

-- newtype FmtString a = FmtString [FmtSegment a] deriving (Show, Eq, Functor, Traversable, Foldable)
-- makeWrapped ''FmtString

