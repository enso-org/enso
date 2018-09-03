{-# LANGUAGE OverloadedStrings #-}

module Luna.Syntax.Text.Parser.Lexer.Names where

import Prologue


---------------------------
-- === Special names === --
---------------------------

acc, app, arrow, assign, invalid, lam, minus, typed, uminus, update, wildcard
    :: IsString s => s
acc      = "."         ; {-# INLINE acc      #-}
app      = "#app#"     ; {-# INLINE app      #-}
arrow    = "->"        ; {-# INLINE arrow    #-}
assign   = "#=#"       ; {-# INLINE assign   #-}
invalid  = "#invalid#" ; {-# INLINE invalid  #-}
lam      = ":"         ; {-# INLINE lam      #-}
minus    = "-"         ; {-# INLINE minus    #-}
typed    = "::"        ; {-# INLINE typed    #-}
uminus   = "#uminus#"  ; {-# INLINE uminus   #-}
update   = "="         ; {-# INLINE update   #-}
wildcard = "_"         ; {-# INLINE wildcard #-}

rawAssign :: IsString s => s
rawAssign = "=" ; {-# INLINE rawAssign #-}
