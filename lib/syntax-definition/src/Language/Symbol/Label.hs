{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Language.Symbol.Label where

-- import OCI.Data.Name as Name
import Prologue



-------------------
-- === Label === --
-------------------

class HasLabel a where
    type family LabelOf a
    label :: Lens' a (LabelOf a)

    type LabelOf a = a
    default label :: (LabelOf a ~ a) => Lens' a (LabelOf a)
    label = id



---------------------
-- === Labeled === --
---------------------

-- === Definition === --

data Labeled l a = Labeled { __label :: l, _labeledContent :: a } deriving (Show, Eq, Functor)
makeLenses ''Labeled


-- === Utils === --

labeled :: l -> a -> Labeled l a
labeled = Labeled

unlabel :: Labeled l a -> a
unlabel = view labeledContent


-- === Instances === --

instance HasLabel (Labeled l a) where
    type LabelOf  (Labeled l a) = l
    label = labeled_label

instance Copointed (Labeled l) where
    copoint = view labeledContent




-- TOREFACTOR?

-- instance HasLabel Name.Nam`e
