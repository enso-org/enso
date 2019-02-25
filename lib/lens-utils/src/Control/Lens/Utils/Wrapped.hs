{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Lens.Utils.Wrapped where

import Prelude
import Control.Lens
import Data.Coerce


wrapped'    = _Wrapped'       ; {-# INLINE wrapped'    #-}
unwrapped'  = _Unwrapped'     ; {-# INLINE unwrapped'  #-}
wrapping'   = _Wrapping'      ; {-# INLINE wrapping'   #-}
unwrapping' = _Unwrapping'    ; {-# INLINE unwrapping' #-}
unwrap'     = view wrapped'   ; {-# INLINE unwrap'     #-}
wrap'       = view unwrapped' ; {-# INLINE wrap'       #-}

wrapped    = _Wrapped         ; {-# INLINE wrapped    #-}
unwrapped  = _Unwrapped       ; {-# INLINE unwrapped  #-}
wrapping   = _Wrapping        ; {-# INLINE wrapping   #-}
unwrapping = _Unwrapping      ; {-# INLINE unwrapping #-}
_unwrap    = view wrapped     ; {-# INLINE _unwrap    #-}
_wrap      = view unwrapped   ; {-# INLINE _wrap      #-}

rewrap     = _Wrapped %~ id ; {-#INLINE rewrap #-}

wrappedM' f = fmap wrap' . f . unwrap' ; {-# INLINE wrappedM' #-}


wrap   :: forall a. Coercible (Unwrapped a) a => Unwrapped a -> a
unwrap :: forall a. Coercible a (Unwrapped a) => a -> Unwrapped a
wrap   = coerce ; {-# INLINE wrap   #-}
unwrap = coerce ; {-# INLINE unwrap #-}

