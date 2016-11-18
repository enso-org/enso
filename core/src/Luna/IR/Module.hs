module Luna.IR.Module where


import Prelude.Luna


-- === Definitions === --

data Module body = Module { __body_ :: body
                          } deriving (Show, Generic, Eq, Read, Functor, Traversable, Foldable)
makeLenses ''Module


-- === Instances === --

instance Default body => Default (Module body) where
    def = Module def
