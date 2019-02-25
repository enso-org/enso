{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Prologue.Text.Show (module Prologue.Text.Show, module X) where

import Prelude (Int, (+), (>), (.))
import Text.Show          as X (Show, ShowS, show, showsPrec, showList, shows, showString, showChar, showParen)

-- needed for tryReads only:
import Prelude (Read, String, Either(..), reads)
import Data.Convert

showAppPrec :: Int
showAppPrec = 10 ; {-# INLINE showAppPrec #-}

showsPrec' :: Show a => a -> ShowS
showsPrec' = showsPrec (showAppPrec + 1) ; {-# INLINE showsPrec' #-}

showParen' :: Int -> ShowS -> ShowS
showParen' d = showParen (d > showAppPrec) ; {-# INLINE showParen' #-}


-- FIXME[WD]: Refactor or remove
tryReads :: forall s' s a. (Read a, Convertible' s String) => s -> Either String a
tryReads s = case reads (convert' s) of
    [(a,[])]  -> Right a
    ((_,s):_) -> Left  s
    _         -> Left "No read"


show' :: (Convertible' String s, Show a) => a -> s
show' = convert' . show ; {-# INLINE show' #-}
