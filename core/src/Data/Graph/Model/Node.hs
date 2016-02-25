{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Model.Node where

import Prologue hiding (Getter, Setter)

import Data.Container
import Data.Index
import Data.Prop
import Data.Record


-- === Definitions === --

data Node = Node deriving (Show, Eq, Ord)
--type family NodeOf a


-- === Instances === --

---- Wrappers

--makeWrapped ''Node
--type instance Uncovered (Node a) = Uncovered (Unlayered (Node a))
--type instance Unlayered (Node a) = Unwrapped (Node a)
--instance      Layered   (Node a)

---- Construction

--instance Monad m => LayerConstructor m (Node a) where constructLayer = return ∘ Node    ; {-# INLINE constructLayer #-}
--instance Monad m => LayerDestructor  m (Node a) where destructLayer  = return ∘ unwrap' ; {-# INLINE destructLayer  #-}

---- Conversions

--instance Castable a a' => Castable (Node a) (Node a') where
--    cast = wrapped %~ cast ; {-# INLINE cast #-}

---- Properties

--type instance            Prop p (Node t) = Prop p t
--instance Getter a t => Getter a (Node t) where getter a = getter a ∘ unwrap'      ; {-# INLINE getter #-}
--instance Setter a t => Setter a (Node t) where setter   = over wrapped' ∘∘ setter ; {-# INLINE setter #-}

---- Records

--type instance           RecordOf  (Node a) = RecordOf a
--instance IsRecord  a => IsRecord  (Node a) where asRecord = wrapped' ∘ asRecord
--instance HasRecord a => HasRecord (Node a) where record   = wrapped' ∘ record
