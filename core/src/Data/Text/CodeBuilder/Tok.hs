{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.CodeBuilder.Tok
-- Copyright   :  (C) 2014 Flowbox
-- License     :  AllRightsReserved
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.Text.CodeBuilder.Tok where

import           Data.Text.CodeBuilder.Doc (Doc, between)
import           Prologue

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Prec = Top
          | Bottom
          | Lvl Int
          deriving (Show, Generic, Eq)


data Tok = Tok { _prec :: Prec
               , _doc  :: Doc
               }

makeLenses ''Tok

----------------------------------------------------------------------
-- Combinators
----------------------------------------------------------------------

sbox, weak, parensed, bracked, braced :: Tok -> Tok

sbox = Tok Top    . view doc
weak = Tok Bottom . view doc
parensed = sbox . (doc %~ between "(" ")")
bracked  = sbox . (doc %~ between "[" "]")
braced   = sbox . (doc %~ between "{" "}")

precParens :: Prec -> Tok -> Doc
precParens tresh t@(Tok prec bldr) = view doc $ if checkPrec tresh prec then parensed t
                                                                        else t

checkPrec :: Prec -> Prec -> Bool
checkPrec p1 p2 = case p2 of
    Top -> False
    _   -> p1 >= p2


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Monoid Tok where
    mempty = Tok Top mempty
    (Tok pri doc) `mappend` (Tok pri' doc') = Tok pri $ doc `mappend` doc'

instance Num Prec where
    fromInteger = Lvl . fromInteger

instance IsString Tok where
    fromString = Tok Top . fromString

instance FromText Tok where
    fromText = Tok Top . fromText

instance Ord Prec where
    a `compare` b = case a of
        Top -> case b of
            Top -> EQ
            _   -> GT
        Bottom -> case b of
            Bottom -> EQ
            _   -> LT
        Lvl l -> case b of
            Top    -> LT
            Bottom -> GT
            Lvl l' -> l `compare` l'
