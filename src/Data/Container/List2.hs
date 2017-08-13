{-# LANGUAGE UndecidableInstances #-}

module Data.Container.List2 where

import Data.Container.Class

import Prelude
import Data.Convert
import Control.Lens

-- TODO: TH vvv
type ToList    a = Convertible  a [Item a]
type ToList'   a = Convertible' a [Item a]
type FromList  a = Convertible  [Item a] a
type FromList' a = Convertible' [Item a] a
type IsList    a = (ToList  a, FromList  a)
type IsList'   a = (ToList' a, FromList' a)

toList    :: ToList    a => a -> [Item a]
toList'   :: ToList'   a => a -> [Item a]
fromList  :: FromList  a => [Item a] -> a
fromList' :: FromList' a => [Item a] -> a
asList    :: IsList    a => Iso' a [Item a]
asList'   :: IsList'   a => Iso' a [Item a]
toList    = convert               ; {-# INLINE toList    #-}
toList'   = convert'              ; {-# INLINE toList'   #-}
fromList  = convert               ; {-# INLINE fromList  #-}
fromList' = convert'              ; {-# INLINE fromList' #-}
asList    = iso toList  fromList  ; {-# INLINE asList    #-}
asList'   = iso toList' fromList' ; {-# INLINE asList'   #-}
