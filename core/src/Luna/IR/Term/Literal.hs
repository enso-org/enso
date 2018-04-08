module Luna.IR.Term.Literal where

import Prologue

import qualified Luna.IR.Component.Term.Definition as Term
import qualified Luna.IR.Term.Format               as Format

import Data.Vector.Storable.Foreign (Vector)




--------------------
-- === Number === --
--------------------

-- === Definition === --

Term.define ''Format.Value [d|
    data Number a = Number
        { _base     :: Word8
        , _intPart  :: Vector Word8
        , _fracPart :: Vector Word8
        }
    |]














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

