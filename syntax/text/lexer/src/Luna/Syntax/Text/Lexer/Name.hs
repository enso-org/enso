{-# LANGUAGE OverloadedStrings #-}

module Luna.Syntax.Text.Lexer.Name where

import Prologue


-- === Operators === --

regularOperatorChars :: [Char]
regularOperatorChars = "!$%&*+-/<>?^~\\"

isRegularOperatorChar :: Char -> Bool
isRegularOperatorChar = (`elem` regularOperatorChars)

isOperator :: Convertible' s String => s -> Bool
isOperator = test . convertTo' @String where
    test s = all isRegularOperatorChar s
          || s `elem` [",", "..", "...", "=="]
          || (maybeLast s == Just '=') && isOperator (init s)


-- === Special names === --

markerBegin, markerEnd :: Char
markerBegin = '«'
markerEnd   = '»'

metadataHeader :: IsString s => s
metadataHeader = "META"

mkMetadata :: (IsString s, Semigroup s) => s -> s
mkMetadata s = "### " <> metadataHeader <> s
