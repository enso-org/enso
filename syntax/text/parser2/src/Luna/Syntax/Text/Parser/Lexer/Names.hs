module Luna.Syntax.Text.Parser.Lexer.Names where

import Prologue

import qualified Data.Char as Char



---------------------------
-- === Special names === --
---------------------------

acc, lam, minus, update, wildcard          :: Convertible' Char   s => s
app, arrow, assign, invalid, typed, uminus :: Convertible' String s => s
acc      = convert' '.'         ; {-# INLINE acc      #-}
app      = convert' "#app#"     ; {-# INLINE app      #-}
arrow    = convert' "->"        ; {-# INLINE arrow    #-}
assign   = convert' "#=#"       ; {-# INLINE assign   #-}
invalid  = convert' "#invalid#" ; {-# INLINE invalid  #-}
lam      = convert' ':'         ; {-# INLINE lam      #-}
minus    = convert' '-'         ; {-# INLINE minus    #-}
typed    = convert' "::"        ; {-# INLINE typed    #-}
uminus   = convert' "#uminus#"  ; {-# INLINE uminus   #-}
update   = convert' '='         ; {-# INLINE update   #-}
wildcard = convert' '_'         ; {-# INLINE wildcard #-}

rawAssign :: Convertible' Char s => s
rawAssign = convert' '=' ; {-# INLINE rawAssign #-}


isIdentBodyChar, isVarHeadChar, isConsHeadChar :: Char -> Bool
isIdentBodyChar = \c -> Char.isAlphaNum c
isVarHeadChar   = \c -> Char.isLower c || c == '_'
isConsHeadChar  = Char.isUpper
{-# INLINE isIdentBodyChar #-}
{-# INLINE isVarHeadChar   #-}
{-# INLINE isConsHeadChar  #-}

varNamePfxChar :: Char
varNamePfxChar = '_'
{-# INLINE varNamePfxChar #-}

identBodySfxChar :: Char
identBodySfxChar = '\''
{-# INLINE identBodySfxChar #-}

markerBegin, markerEnd :: Char
markerBegin = '«'
markerEnd   = '»'
{-# INLINE markerBegin #-}
{-# INLINE markerEnd   #-}
