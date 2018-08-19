{-# LANGUAGE NoMonoLocalBinds        #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | All of the functions below work only on «interesting» subterms.
-- It is up to the instance writer to decide which subterms are
-- interesting and which subterms should count as immediate. This can
-- also depend on the context @c@.
--
-- The context, denoted @c@, is a constraint (of kind @* -> Constraint@)
-- that provides additional facilities to work with the data.
-- In most cases, the context cannot be inferred automatically.
-- You need to provide it using the
-- <https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#visible-type-application type application syntax>:
--
-- > gmap @Show f x
-- > everywhere @Typeable f x
--
-- etc.
--
-- For more information, see:
--
-- [Scrap your boilerplate with class]
-- <https://www.microsoft.com/en-us/research/publication/scrap-your-boilerplate-with-class/>
--
-- [Generalizing generic fold]
-- <http://ro-che.info/articles/2013-03-11-generalizing-gfoldl>

module Data.Generics.Traversable (module Data.Generics.Traversable, module X) where
import Data.Generics.Traversable.Class     as X
import Data.Generics.Traversable.Instances ()

import Prelude

import Control.Applicative   (WrappedMonad (WrapMonad), unwrapMonad)
import Control.Monad         (void, (<=<))
import Data.Functor.Constant (Constant (Constant), getConstant)
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Data.Monoid           (Endo (Endo), appEndo)
import GHC.Exts              (Constraint)

-- | 'Rec' enables \"deep traversals\".
--
-- It is satisfied automatically when its superclass constraints are
-- satisfied — you are not supposed to declare new instances of this class.
class    (GTraversable (Rec c) a, c a) => Rec (c :: * -> Constraint) a
instance (GTraversable (Rec c) a, c a) => Rec (c :: * -> Constraint) a

-- | Generic map over the immediate subterms
gmap :: ∀ ctx a. GTraversable ctx a => (∀ a. ctx a => a -> a) -> a -> a
gmap = \f a -> runIdentity (gtraverse @ctx (Identity . f) a) ; {-# INLINE gmap #-}

-- | Generic map over the immediate subterms
gmap1 :: ∀ ctx a t1. GTraversable1 ctx a
      => (∀ a t1. ctx a => a t1 -> a t1) -> a t1 -> a t1
gmap1 = \f a -> runIdentity (gtraverse1 @ctx (Identity . f) a) ; {-# INLINE gmap1 #-}

-- | Generic monadic map over the immediate subterms
gmapM :: ∀ ctx m a. (Monad m, GTraversable ctx a)
      => (∀ a. ctx a => a -> m a) -> a -> m a
gmapM = \f -> unwrapMonad . gtraverse @ctx (WrapMonad . f) ; {-# INLINE gmapM #-}

-- | Generic monadic map over the immediate subterms
gmapM1 :: ∀ ctx m a t1. (Monad m, GTraversable1 ctx a)
       => (∀ a t1. ctx a => a t1 -> m (a t1)) -> a t1 -> m (a t1)
gmapM1 = \f -> unwrapMonad . gtraverse1 @ctx (WrapMonad . f) ; {-# INLINE gmapM1 #-}

-- | Generic monadic map over the immediate subterms
gmapM_ :: ∀ ctx m a. (Monad m, GTraversable ctx a)
       => (∀ a. ctx a => a -> m ()) -> a -> m ()
gmapM_ = \f -> void . gmapM @ctx (\d -> d <$ f d) ; {-# INLINE gmapM_ #-}

-- | Generic monadic map over the immediate subterms
gmapM1_ :: ∀ ctx m a t1. (Monad m, GTraversable1 ctx a)
       => (∀ a t1. ctx a => a t1 -> m ()) -> a t1 -> m ()
gmapM1_ = \f -> void . gmapM1 @ctx (\d -> d <$ f d) ; {-# INLINE gmapM1_ #-}

-- | Generic monoidal fold over the immediate subterms (cf. 'Data.Foldable.foldMap')
gfoldMap :: ∀ c r a . (Monoid r, GTraversable c a)
         => (∀ d . (c d) => d -> r) -> a -> r
gfoldMap = \f -> getConstant . gtraverse @c (Constant . f) ; {-# INLINE gfoldMap #-}

-- | Generic right fold over the immediate subterms
gfoldr :: ∀ c a r . (GTraversable c a)
       => (∀ d . (c d) => d -> r -> r) -> r -> a -> r
gfoldr = \f z t ->
    let !s   = gfoldMap @c (Endo . f) t
        !out = appEndo s z
    in out
{-# INLINE gfoldr #-}

-- | Generic strict left fold over the immediate subterms
gfoldl' :: ∀ c a r . (GTraversable c a)
        => (∀ d . (c d) => r -> d -> r) -> r -> a -> r
gfoldl' = \f z0 xs ->
    let !out = gfoldr @c (\x k z -> k $! f z x) id xs z0
    in out
{-# INLINE gfoldl' #-}

gfoldlM :: ∀ c a r m. (GTraversable c a, Monad m)
        => (∀ d. c d => r -> d -> m r) -> r -> a -> m r
gfoldlM = \f z0 xs -> gfoldr @c (\x k z -> f z x >>= k) return xs z0
{-# INLINE gfoldlM #-}

-- | Apply a transformation everywhere in bottom-up manner
everywhere :: ∀ c a. (Rec c a)
           => (∀ d. (Rec c d) => d -> d) -> a -> a
everywhere = \f ->
    let go :: ∀ b . Rec c b => b -> b
        go = f . gmap @(Rec c) go
    in  go
{-# INLINE everywhere #-}

-- | Apply a transformation everywhere in top-down manner
everywhere' :: ∀ c a. (Rec c a)
            => (∀ d. (Rec c d) => d -> d) -> a -> a
everywhere' = \f ->
    let go :: ∀ b . Rec c b => b -> b
        go = gmap @(Rec c) go . f
    in  go
{-# INLINE everywhere' #-}

-- | Monadic variation on everywhere
everywhereM :: ∀ c m a. (Monad m, Rec c a)
            => (∀ d. (Rec c d) => d -> m d) -> a -> m a
everywhereM = \f ->
    let go :: ∀ b . Rec c b => b -> m b
        go = f <=< gmapM @(Rec c) go
    in  go
{-# INLINE everywhereM #-}

-- | Strict left fold over all elements, top-down
everything :: ∀ c r a. (Rec c a)
           => (r -> r -> r) -> (∀ d . (Rec c d) => d -> r) -> (a -> r)
everything = \combine f ->
    let go :: ∀ b . Rec c b => b -> r
        go x = gfoldl' @(Rec c) (\a y -> combine a (go y)) (f x) x
    in  go
{-# INLINE everything #-}

