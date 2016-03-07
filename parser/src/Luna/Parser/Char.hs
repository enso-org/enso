module Luna.Parser.Char (module Luna.Parser.Char, module X) where

import Prelude.Luna

import           Text.Parser.Char         as X hiding (spaces)
import           Luna.Parser.Combinators
import qualified Data.Char as Char


isSpace :: Char -> Bool
isSpace = Char.isSpace

isHSpace :: Char -> Bool
isHSpace s = isSpace s && not (isVSpace s)

isVSpace :: Char -> Bool
isVSpace = flip elem [ '\n'
                     , '\r'
                     , '\v' -- vertical tab
                     , '\f' -- form feed (page break)
                     ]

spaces1 :: CharParsing m => m String
spaces1 = many1 (satisfy isSpace)

spaces :: CharParsing m => m String
spaces = spaces1 <|> pure mempty

vspaces1 :: CharParsing m => m String
vspaces1 = many1 (satisfy isVSpace)

vspaces :: CharParsing m => m String
vspaces = vspaces1 <|> pure mempty

hspaces1 :: CharParsing m => m String
hspaces1 = many1 (satisfy isHSpace)

hspaces :: CharParsing m => m String
hspaces = hspaces1 <|> pure mempty


notSpaces1 :: CharParsing m => m String
notSpaces1 = many1 (satisfy $ not ∘ isSpace)
