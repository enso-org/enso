module Text.Parser.Char where

import Prelude
import Control.Applicative
import Text.Megaparsec (satisfy)
import Text.Megaparsec.Prim
import Text.Parser.Combinators
import Data.Word
import qualified Data.Char as Char


satisfyOrd :: (MonadParsec e s m, Token s ~ Char) => (Int -> Bool) -> m Char
satisfyOrd f = satisfy (f . Char.ord)

isSpace :: Char -> Bool
isSpace s = Char.generalCategory s == Char.Space

isTab :: Char -> Bool
isTab s = s == '\t'

space' :: (MonadParsec e s m, Token s ~ Char) => m Char
space' = satisfy isSpace

space :: (MonadParsec e s m, Token s ~ Char) => m Char
space = satisfy (\s -> isSpace s || isTab s)

countedSpace :: (MonadParsec e s m, Token s ~ Char) => Word64 -> m Word64
countedSpace n = (1 <$ satisfy isSpace) <|> (n <$ satisfy isTab)

spaces' :: (MonadParsec e s m, Token s ~ Char) => m String
spaces' = many space'

spaces :: (MonadParsec e s m, Token s ~ Char) => m String
spaces = many space

countedSpaces :: (MonadParsec e s m, Token s ~ Char) => Word64 -> m Word64
countedSpaces n = sum <$> many (countedSpace n)

-- | Standard end of line used in ASCII based systems
eol :: (MonadParsec e s m, Token s ~ Char) => m String
eol = ((\s -> (s :)) <$> satisfyOrd (== cr)) <?*> (pure <$> satisfyOrd (== lf))
    where lf = 0x000A
          cr = 0x000D

  -- | There's 8 possible line terminators according to
  --   https://en.wikipedia.org/wiki/Newline
rareEol :: (MonadParsec e s m, Token s ~ Char) => m String
rareEol = (pure <$> satisfyOrd (`elem` [lf, vt, ff, nel, ls, ps]))
  <|> ((\s t -> [s,t]) <$> satisfyOrd (== cr) <*> satisfyOrd (== lf))
    where lf  = 0x000A
          vt  = 0x000B
          ff  = 0x000C
          cr  = 0x000D
          nel = 0x0085
          ls  = 0x2028
          ps  = 0x2029


eols :: (MonadParsec e s m, Token s ~ Char) => m [String]
eols = many eol

spaceOrEol :: (MonadParsec e s m, Token s ~ Char) => m String
spaceOrEol = (pure <$> space) <|> eol

spacesOrEols :: (MonadParsec e s m, Token s ~ Char) => m [String]
spacesOrEols = many spaceOrEol


natDigit, decDigit, binDigit, octDigit, hexDigit :: (MonadParsec e s m, Token s ~ Char) => m Char
natDigit = satisfy (ordBetween 48 58) <?> "positive digit"
decDigit = satisfy (ordBetween 47 58) <?> "digit"
binDigit = satisfy (ordBetween 47 50) <?> "binary digit"
octDigit = satisfy (ordBetween 47 56) <?> "octal digit"
hexDigit = decDigit <|> satisfy (ordBetween 64 71) <|> satisfy (ordBetween 96 103) <?> "hex digit"

ordBetween :: Int -> Int -> Char -> Bool
ordBetween min max c = let ord = Char.ord c in ord > min && ord < max
