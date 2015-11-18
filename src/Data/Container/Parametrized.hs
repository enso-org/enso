{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds    #-}

module Data.Container.Parametrized where


--import Prologue
--import Data.Container.Class
--import Data.Container.Poly
--import Data.Container.Weak
--import Data.Layer


--newtype Parametrized t (m :: * -> *) a = Parametrized { _parametrized :: t (m a) } deriving (Show, Functor, Traversable, Foldable, FromList, Monoid, Default)

--type instance Item              (Parametrized t m a) = Item        (Unwrapped (Parametrized t m a))
--type instance ModsOf (cls :: k) (Parametrized t m a) = ModsOf cls  (Unwrapped (Parametrized t m a))
--type instance DataStoreOf       (Parametrized t m a) = DataStoreOf (Unwrapped (Parametrized t m a))
--type instance ContainerOf       (Parametrized t m a) = ContainerOf (Unwrapped (Parametrized t m a))

---- Wrappers & layers

--type instance Unlayered (Parametrized t m a) = Unwrapped (Parametrized t m a)
--instance      Layered   (Parametrized t m a)
--instance      Wrapped   (Parametrized t m a) where 
--	type Unwrapped (Parametrized t m a) = t (m a)
--	_Wrapped' = iso (\(Parametrized c) -> c) Parametrized


---- Instances

--instance IsContainer  (t (m a)) => IsContainer  (Parametrized t m a) where fromContainer = Parametrized . fromContainer
--instance HasContainer (t (m a)) => HasContainer (Parametrized t m a) where container     = layered . container

--type instance ElementOf   (Parametrized t m a) = ElementOf  (t (m a))
--type instance IndexOf el  (Parametrized t m a) = IndexOf el (t (m a))

--type instance HomoIndexOf (Parametrized t m) = HomoIndexOf m


---- Lenses

--makeLenses ''Parametrized
