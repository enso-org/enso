-- | Based on «Scrap Your Zippers: A Generic Zipper for Heterogeneous Types.
-- Michael D. Adams.  WGP '10: Proceedings of the 2010 ACM SIGPLAN
-- workshop on Generic programming, 2010»
-- (<http://michaeldadams.org/papers/scrap_your_zippers/>).
--
-- Compared to the original @syz@ package, this implementation (based on
-- 'GTraversable') gives more flexibility as to where a zipper may point
-- to and what is considered as siblings.
--
-- Specifically, a zipper may point to any element which `gtraverse`
-- applies its function to.
--
-- == Example
-- === syz
-- Consider the classical example: lists. With syz, a list is interpreted as a right-balanced
-- tree.
--
-- >>> let z = fromJust . down' $ toZipper ['a'..'d']
-- >>> getHole z :: Maybe Char
-- Just 'a'
--
-- The zipper @z@ points to the first element of the list. Now let's
-- move to the right:
--
-- >>> let z' = fromJust . right $ z
-- >>> getHole z' :: Maybe Char
-- Nothing
-- >>> getHole z' :: Maybe [Char]
-- Just "bcd"
--
-- Instead of pointing to the second element of the list, as one might
-- expect, the zipper @z\'@ points to the tail of the list. In order to
-- actually move to the second element, we need another 'down'':
--
-- >>> let z'' = fromJust . down' $ z'
-- >>> getHole z'' :: Maybe Char
-- Just 'b'
--
-- === traverse-with-class
-- 'GTraversable'-based zippers behave more intuitively in this regard,
-- thanks to the uniform instance for lists.
--
-- >>> let z = fromJust . down' $ toZipper ['a'..'d'] :: Zipper Typeable [Char]
-- >>> getHole z :: Maybe Char
-- Just 'a'
--
-- So far it's more or less the same as with syz. We needed to add a type
-- annotation for the zipper itself to clarify the context which should
-- be available at each hole ('Typeable' in this case). Now let's see
-- what's to the right of us:
--
-- >>> let z' = fromJust . right $ z
-- >>> getHole z' :: Maybe Char
-- Just 'b'
--
-- That is, we jumped right to the second element of the list. Likewise,
--
-- >>> let z'' = rightmost z
-- >>> getHole z'' :: Maybe Char
-- Just 'd'
--
-- So, unlike in @syz@, all of the list elements are siblings.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Data.Generics.Traversable.Zipper where

import Prelude

import Control.Monad (liftM)
import Data.Generics.Traversable
import Data.Typeable (Typeable, cast)
import GHC.Exts (Constraint)

-- Core types

-- | A generic zipper with a root object of type @root@.
data Zipper (c :: * -> Constraint) root =
  forall hole. (Rec c hole) =>
    Zipper hole (Context c hole root)

---- Internal types and functions
data Context c hole root where
    CtxtNull :: Context c a a
    CtxtCons ::
      forall hole root rights parent c. (Rec c parent) =>
        Left c (hole -> rights)
        -> Right c rights parent
        -> Context c parent root
        -> Context c hole root

combine :: Left c (hole -> rights)
         -> hole
         -> Right c rights parent
         -> parent
combine lefts hole rights =
  fromRight ((fromLeft lefts) hole) rights

-- Left is essentially Ørjan Johansen’s free applicative functor.
-- (see http://ro-che.info/articles/2013-03-31-flavours-of-free-applicative-functors.html)
--
-- This allows us to convert any GTraversable value to a zipper.
data Left c expects
  = LeftUnit expects
  | forall b. (Rec c b) => LeftCons (Left c (b -> expects)) b

instance Functor (Left c) where
  fmap f (LeftUnit x) = LeftUnit $ f x
  fmap f (LeftCons lft x) = LeftCons (fmap (f .) lft) x

instance Applicative (Left c) where
  pure = LeftUnit
  tx <*> LeftUnit e = fmap ($ e) tx
  tx <*> LeftCons ty az = LeftCons ((.) <$> tx <*> ty) az

unit :: Rec c b => b -> Left c b
unit = LeftCons (LeftUnit id)

toLeft :: forall a c . (Rec c a) => a -> Left c a
toLeft = gtraverse @(Rec c) unit

fromLeft :: Left c r -> r
fromLeft (LeftUnit a)   = a
fromLeft (LeftCons f b) = fromLeft f b

data Right c provides parent where
  RightNull :: Right c parent parent
  RightCons ::
    (Rec c b) => b -> Right c a t -> Right c (b -> a) t

fromRight :: r -> Right c r parent -> parent
fromRight f (RightNull)     = f
fromRight f (RightCons b r) = fromRight (f b) r

-- Core interface

---- Injection and projection

-- | Move up a zipper to the root and return the root object.
fromZipper :: Zipper c a -> a
fromZipper (Zipper hole CtxtNull) = hole
fromZipper (Zipper hole (CtxtCons l r ctxt)) =
  fromZipper (Zipper (combine l hole r) ctxt)

-- | Create a zipper.  The focus starts at the root of the object.
toZipper :: Rec c a => a -> Zipper c a
toZipper x = Zipper x CtxtNull

---- Basic movement

-- | Move left.  Returns 'Nothing' iff already at leftmost sibling.
left  :: Zipper c a -> Maybe (Zipper c a)
left (Zipper _ CtxtNull) = Nothing
left (Zipper _ (CtxtCons (LeftUnit _) _ _)) = Nothing
left (Zipper h (CtxtCons (LeftCons l h') r c)) =
  Just (Zipper h' (CtxtCons l (RightCons h r) c))

-- | Move right.  Returns 'Nothing' iff already at rightmost sibling.
right :: Zipper c a -> Maybe (Zipper c a)
right (Zipper _ CtxtNull) = Nothing
right (Zipper _ (CtxtCons _ RightNull _)) = Nothing
right (Zipper h (CtxtCons l (RightCons h' r) c)) =
  Just (Zipper h' (CtxtCons (LeftCons l h) r c))

-- | Move down.  Moves to rightmost immediate child.  Returns 'Nothing' iff at a leaf and thus no children exist.
down  :: forall a c . Zipper c a -> Maybe (Zipper c a)
down (Zipper (hole :: holeT) ctxt) =
  case toLeft hole :: Left c holeT of
    LeftUnit _ -> Nothing
    LeftCons l hole' ->
      Just (Zipper hole' (CtxtCons l RightNull ctxt))

-- | Move down. Move to the leftmost immediate child.  Returns 'Nothing' iff at a leaf and thus no children exist.
down' :: Zipper c a -> Maybe (Zipper c a)
down' z = liftM leftmost (down z)

-- | Move up.  Returns 'Nothing' iff already at root and thus no parent exists.
up    :: Zipper c a -> Maybe (Zipper c a)
up (Zipper _ CtxtNull) = Nothing
up (Zipper hole (CtxtCons l r ctxt)) =
  Just (Zipper (combine l hole r) ctxt)

---- Basic hole manipulation

-- | Apply a generic query to the hole.
query
  :: (forall d . Rec c d => d -> b)
  -> Zipper c a -> b
query f (Zipper hole _ctxt) = f hole

-- | Apply a generic transformation to the hole.
trans
  :: (forall d . Rec c d => d -> d)
  -> Zipper c a -> Zipper c a
trans f (Zipper hole ctxt) = Zipper (f hole) ctxt

-- | Apply a generic monadic transformation to the hole
transM
  :: Monad m
  => (forall d . Rec c d => d -> m d)
  -> Zipper c a -> m (Zipper c a)
transM f (Zipper hole ctxt) = do
  hole' <- f hole
  return (Zipper hole' ctxt)

-- Convenience hole manipulation interface

-- | Get the value in the hole.  Returns 'Nothing' iff @a@ is not the type of the value in the hole.
getHole :: (Typeable b) => Zipper Typeable a -> Maybe b
getHole = query cast

-- | Set the value in the hole.  Does nothing iff @a@ is not the type of the value in the hole.
setHole :: (Typeable a) => a -> Zipper Typeable b -> Zipper Typeable b
setHole h z = trans (maybe id const $ cast h) z

-- | Set the value in the hole.  Returns 'Nothing' iff @a@ is not the type of the value in the hole.
setHole' :: (Typeable a) => a -> Zipper Typeable b -> Maybe (Zipper Typeable b)
setHole' h z = transM (const (cast h)) z
-- Generic zipper traversals
---- Traversal helpers

-- | A movement operation such as 'left', 'right', 'up', or 'down'.
type Move c a = Zipper c a -> Maybe (Zipper c a)

-- | Apply a generic query using the specified movement operation.
moveQ :: Move c a -- ^ Move operation
      -> b -- ^ Default if can't move
      -> (Zipper c a -> b) -- ^ Query if can move
      -> Zipper c a -- ^ Zipper
      -> b
moveQ move b f z = case move z of
                     Nothing -> b
                     Just z' -> f z'

------ Query
-- | Apply a generic query to the left sibling if one exists.
leftQ :: b -- ^ Value to return of no left sibling exists.
      -> (Zipper c a -> b) -> Zipper c a -> b
leftQ b f z = moveQ left b f z

-- | Apply a generic query to the right sibling if one exists.
rightQ :: b -- ^ Value to return if no right sibling exists.
       -> (Zipper c a -> b) -> Zipper c a -> b
rightQ b f z = moveQ right b f z

-- | Move to the leftmost sibling.
leftmost :: Zipper c a -> Zipper c a
leftmost z = leftQ z leftmost z

-- | Move to the rightmost sibling.
rightmost :: Zipper c a -> Zipper c a
rightmost z = rightQ z rightmost z

