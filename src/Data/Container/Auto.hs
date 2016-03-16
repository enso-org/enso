{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Container.Auto where

import Prelude

import Control.Lens
import Control.Lens.Utils
import Data.Container.Class
import Data.Container.Resizable
import Data.Container.Reusable
import Data.Container.List
import Data.Layer
import Data.Default
import GHC.Generics    (Generic)
import Control.DeepSeq (NFData)

#define AUTO (Reusable idx (Resizable style a))

newtype IxedAuto idx style a = IxedAuto AUTO deriving (Generic, Functor, Traversable, Foldable, Monoid, Default)
type    Auto         style a = IxedAuto (Index (Container a)) style a

deriving instance (IsContainer a, FromList (Container a), Default style) => FromList (IxedAuto idx style a)

type instance Index (IxedAuto idx style a) = Index AUTO
type instance Item  (IxedAuto idx style a) = Item  AUTO

type instance            DataStore       (IxedAuto idx style a) = DataStore AUTO
type instance            Container       (IxedAuto idx style a) = Container AUTO
instance      Monad m => IsContainerM  m (IxedAuto idx style a) where fromContainerM = fmap IxedAuto  . fromContainerM
instance      Monad m => HasContainerM m (IxedAuto idx style a) where viewContainerM = viewContainerM . view layered
                                                                      setContainerM  = layered        . setContainerM

instance (HasContainer a, ToList (Container a)) => ToList (IxedAuto idx s a) where toList = toList . unwrap'

-- Wrappers & layers

type instance       Unlayered  (IxedAuto idx style a) = AUTO
instance            Layered    (IxedAuto idx style a)
instance Monad m => LayeredM m (IxedAuto idx style a)
instance            Wrapped    (IxedAuto idx style a) where
    type            Unwrapped  (IxedAuto idx style a) = AUTO
    _Wrapped' = iso (\(IxedAuto a) -> a) IxedAuto


deriving instance Show AUTO => Show (IxedAuto idx style a)

-- === Instances === --

-- Normal Form

instance (NFData style, NFData a) => NFData (IxedAuto idx style a)

--instance (NFData [Item t], TrackUsedElems' t, t ~ Reusable idx a) => NFData (Reusable idx a) where


--instance (NFData [Item t], TrackUsedElems' t) => NFData t where

--instance Show a => Show (Auto idx style a) where
--    showsPrec d (Auto a) = showParen (d > app_prec) $
--            showString "Auto " . showsPrec (succ app_prec) (view (layered . layered) a)
--            --showString $ "Auto " <> show (elems a :: [Item a]) -- . showsPrec (succ app_prec) (view (layered . layered) a)
--        where app_prec = 10

