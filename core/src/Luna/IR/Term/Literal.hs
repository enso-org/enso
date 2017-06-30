module Luna.IR.Term.Literal (module Luna.IR.Term.Literal, module X) where

import Luna.Prelude
import Luna.Prelude as X (String)
import Prelude      (read)



----------------------
-- === Literals === --
----------------------

type family LiteralOf a
class HasLiteral a where
    literal :: Lens' a (LiteralOf a)



---------------------
-- === Numbers === --
---------------------

-- === Definition === --

data Base   = Dec | Bin | Oct | Hex deriving (Show, Eq)
data Number = Number { _base     :: Base
                     , _intPart  :: String
                     , _fracPart :: String
                     , _expPart  :: String
                     }

makeLenses ''Number


-- === Creation === --

int, bin, oct, hex, frac :: String -> Number
int  i = Number Dec i mempty mempty
bin  i = Number Bin i mempty mempty
oct  i = Number Oct i mempty mempty
hex  i = Number Hex i mempty mempty
frac i = Number Dec mempty i mempty


-- === Utils === --

isInteger :: Number -> Bool
isInteger n = n ^. fracPart == ""

toInt :: Number -> Int
toInt = read . view intPart

toDouble :: Number -> Double
toDouble x = read $ x ^. intPart <> "." <> x ^. fracPart <> (if not $ null (x ^. expPart) then "e" <> x ^. expPart else "")

fromDouble :: Double -> Number
fromDouble d = Number Dec i f mempty where
    i = takeWhile (/= '.') $ show d
    f = drop 1 $ dropWhile (/= '.') $ show d

-- === Instances === --

instance Show Number where
    show n = pfx $ int <> frac <> exp where
        int  = n ^. intPart
        frac = if_ (n ^. fracPart /= "") $ "." <> n ^. fracPart
        exp  = if_ (n ^. expPart  /= "") $ "e" <> n ^. expPart
        pfx  = (<>) $ case n ^. base of
            Dec -> ""
            Bin -> "0b"
            Oct -> "0o"
            Hex -> "0x"

-- FIXME[WD]: do we need the real Eq & Ord here?
deriving instance Eq  Number
deriving instance Ord Number
deriving instance Ord Base

instance Num Number where
    fromInteger = int . show

---------------
-- FmtString --
---------------

data FmtSegment a = StrSegment  a
                  | ExprSegment a
                  deriving (Show, Eq, Functor, Traversable, Foldable)

newtype FmtString a = FmtString [FmtSegment a] deriving (Show, Eq, Functor, Traversable, Foldable)
makeWrapped ''FmtString
