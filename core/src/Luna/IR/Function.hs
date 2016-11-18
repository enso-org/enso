{-# LANGUAGE TypeOperators #-}

module Luna.IR.Function (module Luna.IR.Function, module X) where

import Luna.IR.Function.Argument   as X
--import Luna.IR.Function.Definition as X


---------------------------------------------------------------------------------------------
-- OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD --
---------------------------------------------------------------------------------------------

import Luna.Prelude


data Signature a = Signature { _self  :: Maybe a
                             , _args  :: [Arg a]
                             , _out   :: a
                             } deriving (Generic, Show, Functor, Foldable, Traversable)

data Function a b = Function { _sig  :: Signature a
                             , _body :: b
                             } deriving (Generic, Show, Functor, Foldable, Traversable)

makeLenses ''Signature
makeLenses ''Function


-- === Instances === --

instance NFData a => NFData (Signature a)

-- Castable
instance Castable n n' => Castable (Signature n) (Signature n') where
    cast = fmap cast ; {-# INLINE cast #-}



