{-# OPTIONS_GHC -Wno-unused-foralls #-}

{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE BangPatterns #-}

module Control.Monad.State.Layered where

import Prelude hiding (Monad) -- hiding ((.)) -- https://ghc.haskell.org/trac/ghc/ticket/14001

import Control.Applicative
import Control.Lens.Utils hiding (Getter, Setter, Setter')
import Control.Monad.Branch
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.Identity hiding (Monad)
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Trans
import Data.Constraint
import Data.Default
import Data.Kind (Type)
import Type.Data.Bool

import qualified Prelude as P
import qualified Control.Monad.State.Strict as S

type M = P.Monad


-------------------
-- === State === --
-------------------

-- === Definition === --

type    State  s     = StateT s Identity
newtype StateT s m a = StateT (S.StateT s m a)
    deriving ( Applicative, Alternative, Functor, M, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadTrans, MonadThrow, MonadBranch )
makeWrapped ''StateT

type        States  ss = StatesT ss Identity
type family StatesT ss m where
    StatesT '[]       m = m
    StatesT (s ': ss) m = StateT s (StatesT ss m)


-- === State data discovery === --

type        InferStateData    (l :: k) m = InferStateData' k l m
type family InferStateData' k (l :: k) m where
    InferStateData' Type l m = l
    InferStateData' k    l m = StateData l m

type TransStateData l t m = (InferStateData l (t m) ~ InferStateData l m)

type family StateData l m where
    StateData l (StateT s m) = If (MatchedBases l s) s (StateData l m)
    StateData l (t m)        = StateData l m

type family MatchedBases (a :: ka) (b :: kb) :: Bool where
    MatchedBases (a :: k) (b   :: k) = a == b
    MatchedBases (a :: k) (b t :: l) = MatchedBases a b
    MatchedBases (a :: k) (b   :: l) = 'False


-- === Monad === --

-- Definitions

type         Monad  l m = (Getter l m, Setter l m)
class M m => Getter l m where get :: m (InferStateData l m)
class M m => Setter l m where put :: InferStateData l m -> m ()

-- Instancess

instance                       M m                                                      => Getter (l :: Type) (StateT l m) where get   = wrap   S.get    ; {-# INLINE get #-}
instance                       M m                                                      => Setter (l :: Type) (StateT l m) where put a = wrap $ S.put a  ; {-# INLINE put #-}
instance {-# OVERLAPPABLE #-}  Getter l m                                               => Getter (l :: Type) (StateT s m) where get   = lift $ get @l   ; {-# INLINE get #-}
instance {-# OVERLAPPABLE #-}  Setter l m                                               => Setter (l :: Type) (StateT s m) where put a = lift $ put @l a ; {-# INLINE put #-}
instance {-# OVERLAPPABLE #-} (M m, Getter__ ok l (StateT s m), ok ~ MatchedBases l s)  => Getter (l :: k)    (StateT s m) where get   = get__  @ok @l   ; {-# INLINE get #-}
instance {-# OVERLAPPABLE #-} (M m, Setter__ ok l (StateT s m), ok ~ MatchedBases l s)  => Setter (l :: k)    (StateT s m) where put a = put__  @ok @l a ; {-# INLINE put #-}
instance {-# OVERLAPPABLE #-} (M (t m), MonadTrans t, Getter l m, TransStateData l t m) => Getter (l :: k)    (t m)        where get   = lift $ get @l   ; {-# INLINE get #-}
instance {-# OVERLAPPABLE #-} (M (t m), MonadTrans t, Setter l m, TransStateData l t m) => Setter (l :: k)    (t m)        where put a = lift $ put @l a ; {-# INLINE put #-}

-- Helpers

class M m => Getter__ (ok :: Bool) l m where get__ :: m (InferStateData l m)
class M m => Setter__ (ok :: Bool) l m where put__ :: InferStateData l m -> m ()

instance (M m, InferStateData l (StateT s m) ~ s)    => Getter__ 'True  l (StateT s m) where get__   = get @s          ; {-# INLINE get__ #-}
instance (M m, InferStateData l (StateT s m) ~ s)    => Setter__ 'True  l (StateT s m) where put__ a = put @s a        ; {-# INLINE put__ #-}
instance (Getter l m, TransStateData l (StateT s) m) => Getter__ 'False l (StateT s m) where get__   = lift $ get @l   ; {-# INLINE get__ #-}
instance (Setter l m, TransStateData l (StateT s) m) => Setter__ 'False l (StateT s m) where put__ a = lift $ put @l a ; {-# INLINE put__ #-}

-- Replicators

type MonadStates  ss m = (Getters ss m, Setters ss m)
type Getters ss m = Monads__ Getter ss m
type Setters ss m = Monads__ Setter ss m
type family Monads__ p ss m :: Constraint where
    Monads__ p (s ': ss) m = (p s m, Monads__ p ss m)
    Monads__ p '[]       m = ()


-- === Accessing === --

use :: ∀ l m s a. (Getter l m, s ~ InferStateData l m)
     => Lens' s a -> m a
use l = view l <$> get @l ; {-# INLINE use #-}

uses :: ∀ l m s r a. (Getter l m, s ~ InferStateData l m)
     => Lens' s a -> (a -> r) -> m r
uses l f = f . view l <$> get @l ; {-# INLINE uses #-}

gets :: ∀ l m s a. (Getter l m, s ~ InferStateData l m)
     => (s -> a) -> m a
gets f = f <$> get @l ; {-# INLINE gets #-}


-- === Top state accessing === --

type family TopStateData m where
    TopStateData (StateT s m) = s
    TopStateData (t m)        = TopStateData m

type Monad'  m = (Getter' m, Setter' m)
type Getter' m = Getter (TopStateData m) m
type Setter' m = Setter (TopStateData m) m

get' :: ∀ m. Getter' m => m (TopStateData m)
put' :: ∀ m. Setter' m => TopStateData m -> m ()
get' = get @(TopStateData m) ; {-# INLINE get' #-}
put' = put @(TopStateData m) ; {-# INLINE put' #-}

gets' :: ∀ m s a. (Getter' m, s ~ TopStateData m) => Lens' s a -> m a
gets' l = view l <$> get' ; {-# INLINE gets' #-}


-- === Construction & running === --

stateT :: (s -> m (a,s)) -> StateT s m a
stateT = StateT . S.StateT ; {-# INLINE stateT #-}

runT  :: ∀ s m a.              StateT s m a -> s -> m (a, s)
evalT :: ∀ s m a. S.Monad m => StateT s m a -> s -> m a
execT :: ∀ s m a. S.Monad m => StateT s m a -> s -> m s
runT  = S.runStateT . unwrap ; {-# INLINE runT  #-}
evalT !m !s = let fst (!a, !_) = a in fst <$> runT m s ; {-# INLINE evalT #-}
execT !m !s = let snd (!_, !a) = a in snd <$> runT m s ; {-# INLINE execT #-}

runDefT  :: ∀ s m a.            Default s  => StateT s m a -> m (a, s)
evalDefT :: ∀ s m a. (S.Monad m,Default s) => StateT s m a -> m a
execDefT :: ∀ s m a. (S.Monad m,Default s) => StateT s m a -> m s
runDefT  !s = runT  s def ; {-# INLINE runDefT  #-}
evalDefT !s = evalT s def ; {-# INLINE evalDefT #-}
execDefT !s = execT s def ; {-# INLINE execDefT #-}

run  :: ∀ s a. State s a -> s -> (a, s)
eval :: ∀ s a. State s a -> s -> a
exec :: ∀ s a. State s a -> s -> s
run  = S.runState  . unwrap ; {-# INLINE run  #-}
eval = S.evalState . unwrap ; {-# INLINE eval #-}
exec = S.execState . unwrap ; {-# INLINE exec #-}

runDef  :: ∀ s a. Default s => State s a -> (a, s)
evalDef :: ∀ s a. Default s => State s a -> a
execDef :: ∀ s a. Default s => State s a -> s
runDef  = flip run  def ; {-# INLINE runDef  #-}
evalDef = flip eval def ; {-# INLINE evalDef #-}
execDef = flip exec def ; {-# INLINE execDef #-}


-- === Generic state modification === --

type InferMonadState s l m = (Monad l m, s ~ InferStateData l m)
modifyM  :: ∀ l s m a. InferMonadState s l m => (s -> m (a, s)) -> m a
modifyM_ :: ∀ l s m a. InferMonadState s l m => (s -> m     s)  -> m ()
modify   :: ∀ l s m a. InferMonadState s l m => (s ->   (a, s)) -> m a
modify_  :: ∀ l s m a. InferMonadState s l m => (s ->       s)  -> m ()
modify    = modifyM  @l . fmap return       ; {-# INLINE modify   #-}
modify_   = modifyM_ @l . fmap return       ; {-# INLINE modify_  #-}
modifyM_  = modifyM  @l . (fmap.fmap) ((),) ; {-# INLINE modifyM_ #-}
modifyM f = do (!a,!t) <- f =<< get @l
               a <$ put @l t
{-# INLINE modifyM #-}

sub           :: ∀ l s m a. InferMonadState s l m =>               m a -> m a
with          :: ∀ l s m a. InferMonadState s l m => s          -> m a -> m a
withModified  :: ∀ l s m a. InferMonadState s l m => (s ->   s) -> m a -> m a
withModifiedM :: ∀ l s m a. InferMonadState s l m => (s -> m s) -> m a -> m a
with              = withModified  @l . const       ; {-# INLINE with          #-}
withModified      = withModifiedM @l . fmap return ; {-# INLINE withModified  #-}
withModifiedM f m = sub @l $ modifyM_ @l f >> m    ; {-# INLINE withModifiedM #-}
sub             m = do s <- get @l
                       m <* put @l s
{-#INLINE sub #-}


-- === Top level state modification === --

type TopMonadState s m = (Monad' m, s ~ TopStateData m)
modifyM'  :: ∀ s m a. TopMonadState s m => (s -> m (a, s)) -> m a
modifyM'_ :: ∀ s m a. TopMonadState s m => (s -> m     s)  -> m ()
modify'   :: ∀ s m a. TopMonadState s m => (s ->   (a, s)) -> m a
modify'_  :: ∀ s m a. TopMonadState s m => (s ->       s)  -> m ()
modify'    = modifyM'  . fmap return       ; {-# INLINE modify'   #-}
modify'_   = modifyM'_ . fmap return       ; {-# INLINE modify'_  #-}
modifyM'_  = modifyM'  . (fmap.fmap) ((),) ; {-# INLINE modifyM'_ #-}
modifyM' f = do (!a,!t) <- f =<< get'
                a <$ put' t
{-# INLINE modifyM' #-}

sub'           :: ∀ s m a. TopMonadState s m =>               m a -> m a
with'          :: ∀ s m a. TopMonadState s m => s          -> m a -> m a
withModified'  :: ∀ s m a. TopMonadState s m => (s ->   s) -> m a -> m a
withModifiedM' :: ∀ s m a. TopMonadState s m => (s -> m s) -> m a -> m a
with'              = withModified'  . const       ; {-# INLINE with'          #-}
withModified'      = withModifiedM' . fmap return ; {-# INLINE withModified'  #-}
withModifiedM' f m = sub' $ modifyM'_ f >> m      ; {-# INLINE withModifiedM' #-}
sub'             m = do s <- get'
                        m <* put' s
{-# INLINE sub' #-}


-- === Other modifications === --

mapT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapT f = _Wrapped %~ S.mapStateT f ; {-# INLINE mapT #-}

-- === Instances === --

instance PrimMonad m => PrimMonad (StateT s m) where
  type PrimState (StateT s m) = PrimState m
  primitive = lift . primitive ; {-# INLINE primitive #-}

deriving newtype instance MonadCatch m => MonadCatch (StateT s m)

