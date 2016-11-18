module Luna.IR.Function.Signature where

--import Prelude.Luna

--import Luna.IR.Function.Argument


---- === Data definitions === --

--data Method   a = Method   { __self_ :: a
--                           , __func_ :: Function a
--                           } deriving (Show, Functor, Foldable, Traversable)

--data Function a = Function { __args_ :: [ArgDef a]
--                           , __out_  :: a
--                           } deriving (Show, Functor, Foldable, Traversable)

---- === Accessors === --

--type family Signature a

--class HasSignature a where
--    signature :: Lens' a (Signature a)


---- === Instances === --

--type instance Signature (Method   a) = Method   a
--type instance Signature (Function a) = Function a

--instance   HasSignature (Method   a) where signature = id ; {-# INLINE signature #-}
--instance   HasSignature (Function a) where signature = id ; {-# INLINE signature #-}
