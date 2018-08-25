module Luna.Syntax.Text.Parser.IR.Name where

import Prologue

import qualified Data.Char as Char



isOperatorBeginChar :: Char -> Bool
isOperatorBeginChar = \c -> let
    ord   = Char.ord c
    check = (ord == 33)              -- !
         || (ord >= 36 && ord <= 38) -- $%&
         || (ord >= 42 && ord <= 47) -- *+,-./
         || (ord >= 58 && ord <= 63) -- :;<>=?
         || (ord == 92)              -- \
         || (ord == 94)              -- ^
         || (ord == 126)             -- ~
         || Char.generalCategory c == Char.MathSymbol
    in check
{-# INLINE isOperatorBeginChar #-}
