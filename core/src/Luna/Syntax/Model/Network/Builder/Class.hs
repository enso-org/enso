{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Network.Builder.Class where

import Prelude.Luna

import Control.Monad.Trans.Identity
import Control.Monad.Except
import Control.Monad.Catch
import Control.Monad.Writer
import Luna.Syntax.Model.Network.Builder.Term.Class (ElemBuilder3, buildElem3, ParamResolver, resolveParams, buildElem2, ElemBuilder2)
import Control.Monad.Delayed as Delayed

import Data.Graph
import Data.Graph.Builder (MonadBuilder, write)
import Data.Graph.Builders (rawConnection)

import qualified Control.Monad.State.Lazy as State
import Control.Monad.Event (Dispatcher, dispatch)

import Luna.Syntax.AST.Term (TermOf)

-- === Definitions === --

newtype NetworkBuilderT m a = NetworkBuilderT (IdentityT m a) deriving (Show, Functor, Traversable, Foldable, Applicative, Monad, MonadTrans, MonadFix, MonadIO, MonadError e, MonadWriter w, MonadThrow, MonadCatch, MonadMask)
makeWrapped ''NetworkBuilderT


-- === Utils === --

runNetworkBuilderT :: NetworkBuilderT m a -> m a
runNetworkBuilderT = runIdentityT ∘ unwrap'


-- === Instances === --

instance (MonadBuilder g m, Referred Edge g (Link a), Dispatcher CONNECTION (Ref Edge (Link a)) m)
      => ParamResolver (State.StateT [(Ref Node a, Ref Edge (Link a))] (NetworkBuilderT m)) (NetworkBuilderT m) (Ref Node a) where
    resolveParams m = do
        (a, pending) <- State.runStateT m mempty
        flip mapM pending $ \(nref, cref) -> do
             write (retarget cref) (rawConnection nref a)
             dispatch CONNECTION cref
        return a


-- FIXME[WD]: Make less polymorphic
instance ElemBuilder2 (TermOf t) m t => ElemBuilder3 m t where buildElem3 = buildElem2


--class    ElemBuilder2 el m  a where buildElem2 :: el -> m a


-- (NetworkBuilderT m)


--class ElemBuilder3 m t where
--    buildTerm' :: TermOf t -> m t



--cref <- reserveConnection
--        --write cref (rawConnection t t)
--        --delayed $ write cref ∘ rawConnection t =<< State.get
--        lift $ State.modify ((t, cref):) -- FIXME[WD]: remove lift
--        return cref




-- State.MonadState [(Ref Node a, Ref Edge (Link a))]

--class (MonadTrans n, Monad (n m), Monad m) => Binder n m a | m -> n where
--    resolveParams :: n m a -> m a


--run' :: Delayed t m a -> m (a, [m t])





--class (MonadTrans n, Monad (n m), Monad m) => Binder n m a | m a -> n where
--    resolveParams :: n m a -> m a

