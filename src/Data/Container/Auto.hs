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


#define AUTO (Reusable idx (Resizable style a))

newtype Auto idx style a = Auto AUTO deriving (Functor, Traversable, Foldable, Monoid, Default)
type    Auto'    style a = Auto (Index (Container a)) style a

deriving instance (IsContainer a, FromList (Container a), Default style) => FromList (Auto idx style a)

type instance Index (Auto idx style a) = Index   AUTO
type instance Item  (Auto idx style a) = Item AUTO

type instance            DataStore       (Auto idx style a) = DataStore AUTO
type instance            Container       (Auto idx style a) = Container AUTO
instance      Monad m => IsContainerM  m (Auto idx style a) where fromContainerM = fmap Auto . fromContainerM
instance      Monad m => HasContainerM m (Auto idx style a) where viewContainerM = viewContainerM . view layered
                                                                  setContainerM  = layered . setContainerM

instance (HasContainer a, ToList (Container a)) => ToList (Auto idx s a) where toList = toList . unwrap'

-- Wrappers & layers

type instance       Unlayered  (Auto idx style a) = AUTO
instance            Layered    (Auto idx style a)
instance Monad m => LayeredM m (Auto idx style a)
instance            Wrapped    (Auto idx style a) where
    type            Unwrapped  (Auto idx style a) = AUTO
    _Wrapped' = iso (\(Auto a) -> a) Auto


deriving instance Show AUTO => Show (Auto idx style a)

--instance Show a => Show (Auto idx style a) where
--    showsPrec d (Auto a) = showParen (d > app_prec) $
--            showString "Auto " . showsPrec (succ app_prec) (view (layered . layered) a)
--            --showString $ "Auto " <> show (elems a :: [Item a]) -- . showsPrec (succ app_prec) (view (layered . layered) a)
--        where app_prec = 10

