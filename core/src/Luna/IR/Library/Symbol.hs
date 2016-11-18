{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Luna.IR.Library.Symbol where

import Prologue  hiding (Symbol, s, p)

import qualified Control.Monad.State            as State
import           Control.Monad.Catch            (MonadMask, MonadCatch, MonadThrow)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Luna.IR.Function      (Function)
import           Luna.IR.Name.Path          (QualPath)
import           Data.Graph                     (Ref, Cluster)
import           Data.Graph.Model.Node


-- === Definitions === --

type SymbolMap n g = Map QualPath (Function (Ref Node n) g) -- FIXME[WD->MK]: this should be more general.
                                                            --                we should not limit it to nodes and graphs
type LocalMap  c   = Map QualPath (Ref Cluster c)

data Env n c g = Env { _symbols      :: SymbolMap n g
                     , _localSymbols :: LocalMap  c
                     } deriving (Show)

makeLenses ''Env

instance Default (Env n c g) where
    def = Env def def


---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Declarations === --

type    Symbol  n c g     = SymbolT n c g Identity
newtype SymbolT n c g m a = SymbolT (State.StateT (Env n c g) m a)
                              deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                                       , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''SymbolT


-- === Utils === --

runT  ::            SymbolT n c g m a -> Env n c g -> m (a, Env n c g)
evalT :: Monad m => SymbolT n c g m a -> Env n c g -> m a
execT :: Monad m => SymbolT n c g m a -> Env n c g -> m (Env n c g)

runT  = State.runStateT  . unwrap' ; {-# INLINE runT  #-}
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}
execT = State.execStateT . unwrap' ; {-# INLINE execT #-}

run  :: Symbol n c g a -> Env n c g -> (a, Env n c g)
eval :: Symbol n c g a -> Env n c g -> a
exec :: Symbol n c g a -> Env n c g -> Env n c g

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: MonadSymbol n c g m => (Env n c g -> Env n c g) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadSymbol n c g m => (Env n c g -> (a, Env n c g)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modifyM :: MonadSymbol n c g m => (Env n c g -> m (a, Env n c g)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modify_ :: MonadSymbol n c g m => (Env n c g -> Env n c g) -> m ()
modify_ = modify . fmap ((),)
{-# INLINE modify_ #-}


-- === Instances === --

class Monad m => MonadSymbol n c g m | m -> n, m -> g, m -> c where
    get :: m (Env n c g)
    put :: Env n c g -> m ()

instance Monad m => MonadSymbol n c g (SymbolT n c g m) where
    get = SymbolT   State.get ; {-# INLINE get #-}
    put = SymbolT . State.put ; {-# INLINE put #-}

instance State.MonadState s m => State.MonadState s (SymbolT n c g m) where
    get = SymbolT $ lift   State.get ; {-# INLINE get #-}
    put = SymbolT . lift . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadSymbol n c g m, MonadTrans t, Monad (t m)) => MonadSymbol n c g (t m) where
    get = lift get   ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

-- Primitive
instance PrimMonad m => PrimMonad (SymbolT n c g m) where
    type PrimState (SymbolT n c g m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

-- === Behaviors === --

loadFunctions :: MonadSymbol n c g m => SymbolMap n g -> m ()
loadFunctions s = modify_ $ symbols %~ Map.union s

lookupFunction :: MonadSymbol n c g m => QualPath -> m (Maybe (Function (Ref Node n) g))
lookupFunction p = Map.lookup p  . view symbols <$> get

loadLambda :: MonadSymbol n c g m => QualPath -> Ref Cluster c -> m ()
loadLambda path lam = modify_ $ localSymbols %~ Map.insert path lam

lookupLambda :: MonadSymbol n c g m => QualPath -> m (Maybe $ Ref Cluster c)
lookupLambda path = Map.lookup path . view localSymbols <$> get
