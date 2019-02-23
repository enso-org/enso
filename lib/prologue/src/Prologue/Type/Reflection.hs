{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType          #-}

module Prologue.Type.Reflection (module Prologue.Type.Reflection, module X) where

import Type.Reflection as X (Typeable, TypeRep, SomeTypeRep, typeOf, typeRep, withTypeable)
import Data.Typeable   as X (Proxy(Proxy))

import Data.Kind
import qualified Type.Reflection as T


---------------------
-- === TypeRep === --
---------------------

-- === Accessing === --

someTypeRep :: forall a. Typeable a => SomeTypeRep
someTypeRep = T.someTypeRep (Proxy :: Proxy a) ; {-# INLINE someTypeRep #-}

typeOfProxy :: forall proxy a. Typeable a => proxy a -> TypeRep a
typeOfProxy _ = typeRep @a ; {-# INLINE typeOfProxy #-}

type family Typeables ls :: Constraint where
    Typeables '[] = ()
    Typeables (l ': ls) = (Typeable l, Typeables ls)

class TypeableMany (ls :: [Type]) where someTypeReps :: [SomeTypeRep]
instance (Typeable l, TypeableMany ls) => TypeableMany (l ': ls) where
    someTypeReps = someTypeRep @l : someTypeReps @ls ; {-# INLINE someTypeReps #-}
instance TypeableMany '[] where
    someTypeReps = [] ; {-# INLINE someTypeReps #-}

