{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}


module Data.Text.CodeBuilder.Tok where

import Prologue

import Data.Text.CodeBuilder.Doc (Doc, between)



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
precParens tresh t@(Tok prec _bldr) = view doc $ if checkPrec tresh prec then parensed t
                                                                         else t

checkPrec :: Prec -> Prec -> Bool
checkPrec p1 p2 = case p2 of
    Top -> False
    _   -> p1 >= p2


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Mempty    Tok where mempty                        = Tok Top mempty
instance Semigroup Tok where Tok pri doc <> Tok _pri' doc' = Tok pri (doc <> doc')

instance Num Prec where
    fromInteger = Lvl . fromInteger

instance IsString Tok where
    fromString = Tok Top . fromString

instance Convertible a Doc => Convertible a Tok where
    convert = Tok Top . convert

instance Ord Prec where
    a `compare` b = case a of
        Top -> case b of
            Top -> EQ
            _   -> GT
        Bottom -> case b of
            Bottom -> EQ
            _      -> LT
        Lvl l -> case b of
            Top    -> LT
            Bottom -> GT
            Lvl l' -> l `compare` l'

