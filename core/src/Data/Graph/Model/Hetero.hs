{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Model.Hetero (module Data.Graph.Model.Hetero, module X) where

import Prologue hiding (Getter, Setter)

import Data.Container.Hetero as X (Hetero(..))
import Data.Prop
import Data.Graph.Model.Ref
import Data.Graph.Model.Edge
import Data.Graph.Model.Node
import Data.Graph.Model.Dynamic


-- === Instances === --



-- Hetero reference handling

-- | When referencing the Hetero graph, we query the underlying one for its native node and edge representations
--   by using `# Node` and `# Edge` families respectively.

instance (Referred r a n', BiCastable n n', n' ~ (a # r))
      =>  Referred r (Hetero a) n where focus r = wrapped' ∘ focus' (retarget r) ∘ casted ; {-# INLINE focus #-}
instance  Referred I (Hetero a) n where focus   = impossible
instance  Referred r (Hetero a) I where focus   = impossible
instance  Referred r (Hetero I) n where focus   = impossible

-- Dynamic

instance (Castable (Prop t g) (Prop t g), Dynamic' t g) => Dynamic' t (Hetero g)

instance (Dynamic' t g, Castable a (g # t)) => Dynamic t (Hetero g) a where
    add a (Hetero g) = Hetero <$> add' (cast a) g & _1 %~ retarget ; {-# INLINE add    #-}
    remove           =  fmap ∘ remove' ∘ retarget                  ; {-# INLINE remove #-}
