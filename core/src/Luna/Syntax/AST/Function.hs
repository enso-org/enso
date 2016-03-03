{-# LANGUAGE TypeOperators #-}

module Luna.Syntax.AST.Function (module Luna.Syntax.AST.Function, module X) where

import Luna.Syntax.AST.Function.Argument   as X
--import Luna.Syntax.AST.Function.Definition as X


---------------------------------------------------------------------------------------------
-- OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD OLD --
---------------------------------------------------------------------------------------------

import Prelude.Luna
import Luna.Syntax.AST.Function.Header


data Signature a = Signature { _self  :: Maybe a
                             , _args  :: [Arg a]
                             , _out   :: a
                             } deriving (Show, Functor, Foldable, Traversable)

data Function a b = Function { _sig  :: Signature a
                             , _body :: b
                             } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Signature
makeLenses ''Function


-- === Instances === --

-- Castable
instance Castable n n' => Castable (Signature n) (Signature n') where
    cast = fmap cast ; {-# INLINE cast #-}



