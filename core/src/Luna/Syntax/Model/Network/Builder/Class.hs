module Luna.Syntax.Model.Network.Builder.Class where

import Prelude.Luna

import Control.Monad.Trans.Identity
import Luna.Syntax.Model.Network.Builder.Term.Class (TransBinder, runTransBinder)

-- === Definitions === --

newtype NetworkBuilderT m a = NetworkBuilderT (IdentityT m a) deriving (Show, Functor, Traversable, Foldable, Applicative, Monad, MonadTrans, MonadFix)
makeWrapped ''NetworkBuilderT


-- === Utils === --

runNetworkBuilderT :: NetworkBuilderT m a -> m a
runNetworkBuilderT = runIdentityT ∘ unwrap'


-- === Instances === --

instance Monad m => TransBinder IdentityT (NetworkBuilderT m) a where runTransBinder = runIdentityT

--class (MonadTrans n, Monad (n m), Monad m) => TransBinder n m a | m -> n where
--    runTransBinder :: n m a -> m a
