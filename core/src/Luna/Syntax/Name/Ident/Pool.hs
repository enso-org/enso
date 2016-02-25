{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# LANGUAGE RankNTypes      #-}

module Luna.Syntax.Name.Ident.Pool where

import Prelude.Luna

import           Control.Monad.Catch            (MonadMask, MonadCatch, MonadThrow)
import qualified Control.Monad.State            as State
import           Data.Pool
import           Data.Tuple (swap)
import           Luna.Syntax.Name.Ident.Class


-----------------------
-- === IdentPool === --
-----------------------

-- === Definitions === --

data IdentPoolState = IdentPoolState { _varNames  :: Pool VarIdent
                                     , _typeNames :: Pool TypeIdent
                                     }

makeLenses ''IdentPoolState


-- === State implementation === --

---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Declarations === --

type    IdentPool      = IdentPoolT Identity
newtype IdentPoolT m a = IdentPoolT (State.StateT IdentPoolState m a)
                              deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                                       , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''IdentPoolT


-- === Utils === --

runT  ::            IdentPoolT m a -> IdentPoolState -> m (a, IdentPoolState)
evalT :: Monad m => IdentPoolT m a -> IdentPoolState -> m a
execT :: Monad m => IdentPoolT m a -> IdentPoolState -> m IdentPoolState

runT  = State.runStateT  . unwrap' ; {-# INLINE runT  #-}
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}
execT = State.execStateT . unwrap' ; {-# INLINE execT #-}

run  :: IdentPool a -> IdentPoolState -> (a, IdentPoolState)
eval :: IdentPool a -> IdentPoolState -> a
exec :: IdentPool a -> IdentPoolState -> IdentPoolState

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: MonadIdentPool m => (IdentPoolState -> IdentPoolState) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadIdentPool m => (IdentPoolState -> (a, IdentPoolState)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modifyM :: MonadIdentPool m => (IdentPoolState -> m (a, IdentPoolState)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modify_ :: MonadIdentPool m => (IdentPoolState -> IdentPoolState) -> m ()
modify_ = modify . fmap ((),)
{-# INLINE modify_ #-}


-- === Instances === --

class Monad m => MonadIdentPool m where
    get :: m IdentPoolState
    put :: IdentPoolState -> m ()

instance Monad m => MonadIdentPool (IdentPoolT m) where
    get = IdentPoolT   State.get ; {-# INLINE get #-}
    put = IdentPoolT . State.put ; {-# INLINE put #-}

instance State.MonadState s m => State.MonadState s (IdentPoolT m) where
    get = IdentPoolT $ lift   State.get ; {-# INLINE get #-}
    put = IdentPoolT . lift . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadIdentPool m, MonadTrans t, Monad (t m)) => MonadIdentPool (t m) where
    get = lift get   ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<



-- === Utils === --

newVarIdent :: MonadIdentPool m => m VarIdent
newVarIdent = new

newTypeIdent :: MonadIdentPool m => m TypeIdent
newTypeIdent = new

newVarIdent' :: (MonadIdentPool m, IsString a) => m a
newVarIdent' = (fromString ∘ toString) <$> newVarIdent

newTypeIdent' :: (MonadIdentPool m, IsString a) => m a
newTypeIdent' = (fromString ∘ toString) <$> newTypeIdent


-- === Internal utils === --


newNamePool :: IsString a => String -> String -> [Char] -> [Char] -> Pool a
newNamePool preffix suffix base chars = Pool
                                      $ drop 1
                                      $ fmap fromString
                                      $ fmap (preffix <>)
                                      $ concat
                                      $ iterate permute [suffix] where
    permute a = fmap (:) chars <*> a

varNamePool :: IsString a => Pool a
varNamePool  = newNamePool "" "#" ['a' .. 'z'] ['a' .. 'z']

typeNamePool :: IsString a => Pool a
typeNamePool = newNamePool "" "#" ['A' .. 'Z'] ['a' .. 'z']


-- === Isntances === --

instance Default IdentPoolState where
    def = IdentPoolState varNamePool typeNamePool where

instance MonadIdentPool m => Generator m VarIdent  where new = modify $ varNames  allocate
instance MonadIdentPool m => Generator m TypeIdent where new = modify $ typeNames allocate
