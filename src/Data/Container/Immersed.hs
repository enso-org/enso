{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Container.Immersed where

import Prelude

import Data.Container.Class
import Data.Layer_OLD
import Control.Lens
import Control.Monad

instance {-# OVERLAPPABLE #-} ( Monad m
                              , HasContainerM m (Unlayered l)
                              , LayeredM m l
                              , ImmersedM (Container (Unlayered l)) m a
                              )          => ImmersedM l m a where viewImmersedM' = viewLayeredM >=> viewContainerM >=> viewImmersedM'
                                                                  setImmersedM'  v l = flip withDivedM l $ setImmersedM' v

instance {-# OVERLAPPABLE #-} (Monad m)  => ImmersedM l m l where viewImmersedM' = return
                                                                  setImmersedM'  = const . return
class                                       ImmersedM l m a where viewImmersedM' :: l -> m a
                                                                  setImmersedM'  :: a -> l -> m l

viewImmersedM :: (ImmersedM (Container l) m a, HasContainerM m l, Monad m) => l -> m a
viewImmersedM = viewContainerM >=> viewImmersedM'

setImmersedM :: (Monad m, HasContainerM m l, ImmersedM (Container l) m a) => a -> l -> m l
setImmersedM v l = do
    a  <- viewContainerM l
    a' <- setImmersedM' v a
    setContainerM a' l 

withImmersedM :: ( Monad m
                 , HasContainerM m l
                 , ImmersedM (Container l) m t
                 , ImmersedM (Container l) m a
                 ) => (t -> m a) -> l -> m l
withImmersedM f l = viewImmersedM l >>= f >>= flip setImmersedM l

withImmersedM' :: ( Monad m
                  , HasContainerM m l
                  , ImmersedM (Container l) m a
                  ) => (a -> a) -> l -> m l
withImmersedM' = withImmersedM . (return .)

dived :: (Layered a, HasContainer (Unlayered a)) => Lens' a (Container (Unlayered a))
dived = layered . container

withDivedM :: (Monad m, LayeredM m a, HasContainerM m (Unlayered a)) => (Container (Unlayered a) -> m (Container (Unlayered a))) -> a -> m a
withDivedM f a = flip withLayeredM a $ withContainerM f