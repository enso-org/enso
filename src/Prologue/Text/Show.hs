module Prologue.Text.Show (module Prologue.Text.Show, module X) where

import Prelude (Int, (+), (>))
import Text.Show as X (Show, ShowS, show, showsPrec, showList, shows, showString, showChar, showParen)

showAppPrec :: Int
showAppPrec = 10 ; {-# INLINE showAppPrec #-}

showsPrec' :: Show a => a -> ShowS
showsPrec' = showsPrec (showAppPrec + 1) ; {-# INLINE showsPrec' #-}

showParen' :: Int -> ShowS -> ShowS
showParen' d = showParen (d > showAppPrec) ; {-# INLINE showParen' #-}
