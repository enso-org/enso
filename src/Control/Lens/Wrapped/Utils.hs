{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Lens.Wrapped.Utils where

import Prelude
import Control.Lens


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
unwrap     = view wrapped     ; {-# INLINE unwrap     #-}
wrap       = view unwrapped   ; {-# INLINE wrap       #-}

rewrap     = _Wrapped %~ id ; {-#INLINE rewrap #-}

wrappedM' f = fmap wrap' . f . unwrap' ; {-# INLINE wrappedM' #-}
