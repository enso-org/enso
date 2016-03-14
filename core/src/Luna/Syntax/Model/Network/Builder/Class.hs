{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Network.Builder.Class where

import Prelude.Luna

import Control.Monad.Trans.Identity
import Control.Monad.Except
import Control.Monad.Writer
import Luna.Syntax.Model.Network.Builder.Term.Class (TermBindBuilder, buildTerm', TransBinder, runTransBinder, buildElem2, ElemBuilder2)
import Control.Monad.Delayed as Delayed

import Data.Graph
import Data.Graph.Builder (MonadBuilder, write)
import Data.Graph.Builders (rawConnection)

import qualified Control.Monad.State.Lazy as State

import Luna.Syntax.AST.Term (TermOf)

-- === Definitions === --

newtype NetworkBuilderT m a = NetworkBuilderT (IdentityT m a) deriving (Show, Functor, Traversable, Foldable, Applicative, Monad, MonadTrans, MonadFix, MonadIO, MonadError e, MonadWriter w)
makeWrapped ''NetworkBuilderT


-- === Utils === --

runNetworkBuilderT :: NetworkBuilderT m a -> m a
runNetworkBuilderT = runIdentityT ∘ unwrap'


-- === Instances === --

instance (MonadBuilder g m, Referred Edge (Link a) g)
      => TransBinder (State.StateT [(Ref Node a, Ref Edge (Link a))]) (NetworkBuilderT m) (Ref Node a) where
    runTransBinder m = do
        (a, pending) <- State.runStateT m mempty
        flip mapM pending $ \(nref, cref) ->
            write cref $ rawConnection nref a
        return a


instance ElemBuilder2 (TermOf t) m t => TermBindBuilder m t where buildTerm' = buildElem2


--class    ElemBuilder2 el m  a where buildElem2 :: el -> m a


-- (NetworkBuilderT m)


--class TermBindBuilder m t where
--    buildTerm' :: TermOf t -> m t



--cref <- reserveConnection
--        --write cref (rawConnection t t)
--        --delayed $ write cref ∘ rawConnection t =<< State.get
--        lift $ State.modify ((t, cref):) -- FIXME[WD]: remove lift
--        return cref




-- State.MonadState [(Ref Node a, Ref Edge (Link a))]

--class (MonadTrans n, Monad (n m), Monad m) => TransBinder n m a | m -> n where
--    runTransBinder :: n m a -> m a


--run' :: Delayed t m a -> m (a, [m t])





--class (MonadTrans n, Monad (n m), Monad m) => TransBinder n m a | m a -> n where
--    runTransBinder :: n m a -> m a

