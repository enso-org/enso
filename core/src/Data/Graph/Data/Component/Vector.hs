{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Data.Component.Vector where

import Data.Graph.Data.Component.Class
import Prologue                        hiding (fromList)

import qualified Data.Construction            as Data
import qualified Data.Property                as Property
import qualified Data.Vector.Storable.Foreign as Vector
import qualified Foreign.Storable.Utils       as Storable
import qualified Foreign.Storable1.Deriving   as Storable1

import Foreign.Storable (Storable)



-----------------
-- === Vector === --
-----------------

-- === Definition === --

newtype Vector comp layout = Vector (Vector.Vector (Component comp layout))
    deriving (Eq, Show, Storable)
makeLenses       ''Vector
Storable1.derive ''Vector


-- === API === --

fromList :: MonadIO m => [Component comp layout] -> m (Vector comp layout)
fromList = \lst -> wrap <$> Vector.fromList lst ; {-# INLINE fromList #-}

toList :: MonadIO m => Vector comp layout -> m [Component comp layout]
toList = Vector.toList . unwrap ; {-# INLINE toList #-}


-- === Instances === --

type instance Property.Get Storable.Dynamics (Vector comp) = Storable.Dynamic

-- type instance Vector.Item (Vector comp layout) = Component comp layout

-- instance MonadIO m => Data.Constructor2 m () Vector where
--     construct2 _ = wrap <$> Data.construct1'
--     {-# INLINE construct2 #-}

instance MonadIO m => Data.ShallowDestructor2 m Vector where
    destructShallow2 = Data.destructShallow1 . unwrap
    {-# INLINE destructShallow2 #-}
