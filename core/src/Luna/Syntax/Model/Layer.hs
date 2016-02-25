{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Layer where

import Prologue hiding (Getter, Setter)

import Data.Construction
import Data.Layer.Cover
import Data.Prop
import Luna.Syntax.AST.Term (LayoutType)
import Type.Bool
import Data.Record


--------------------
-- === Layers === --
--------------------

-- === Definitions === --

type family LayerData layout d base
newtype     Layer     layout t base = Layer (LayerData layout t base) -- deriving (Show) --, Eq, Ord, Functor, Traversable, Foldable)

data Attached t a = Attached t a deriving (Show, Eq, Ord, Functor, Traversable, Foldable)


-- === Instances === --

deriving instance Show (Unwrapped (Layer l t a)) => Show (Layer l t a)

-- Wrappers
makeWrapped ''Layer
type instance Uncovered (Layer l t a) = Uncovered (Unlayered (Layer l t a))
type instance Unlayered (Layer l t a) = Unwrapped (Layer l t a)
instance      Layered   (Layer l t a)

type instance Uncovered (Attached t a) = Uncovered (Unlayered (Attached t a))
type instance Unlayered (Attached t a) = a
instance      Layered   (Attached t a) where
    layered = lens (\(Attached _ a) -> a) (\(Attached d _) a -> Attached d a) ; {-# INLINE layered #-}

-- Construction
instance (Monad m, Creator    m t) => LayerConstructor m (Attached t a) where constructLayer a = flip Attached a <$> create  ; {-# INLINE constructLayer #-}
instance (Monad m, Destructor m t) => LayerDestructor  m (Attached t a) where destructLayer (Attached t a) = a <$ destruct t ; {-# INLINE destructLayer  #-}

-- Casting
instance (Castable a a', Castable t t') => Castable (Attached t a) (Attached t' a') where
    cast (Attached d a) = Attached (cast d) (cast a) ; {-# INLINE cast #-}

instance Castable (Unwrapped (Layer l t a)) (Unwrapped (Layer l' t' a')) => Castable (Layer l t a) (Layer l' t' a') where
    cast = wrapped %~ cast ; {-# INLINE cast #-}

-- Attributes
type instance Prop prop (Attached (Layer l t a) base) = If (prop == t) (Unwrapped (Layer l t a)) (Prop prop base)

instance {-# OVERLAPPABLE #-} (Prop  a (Attached (Layer l a' t) base) ~ Prop a base, Getter a base)
                           => Getter a (Attached (Layer l a' t) base) where getter a (Attached _ t) = getter a t ; {-# INLINE getter #-}
instance {-# OVERLAPPABLE #-} Getter a (Attached (Layer l a  t) base) where getter _ (Attached d _) = unwrap' d  ; {-# INLINE getter #-}

instance {-# OVERLAPPABLE #-} (Prop  a (Attached (Layer l a' t) base) ~ Prop a base, Setter a base)
                           => Setter a (Attached (Layer l a' t) base) where setter a v (Attached d t) = Attached d $ setter a v t ; {-# INLINE setter #-}
instance {-# OVERLAPPABLE #-} Setter a (Attached (Layer l a  t) base) where setter _ v (Attached _ t) = Attached (Layer v) t      ; {-# INLINE setter #-}


--------------------
-- === Shell === ---
--------------------

data (layers :: [*]) :<  (a :: *)        = Shell (ShellStructure layers a)
type (layers :: [*]) :<: (a :: [*] -> *) = layers :< a layers

type family ShellStructure ls a where
    ShellStructure '[]       a = Cover a
    ShellStructure (l ': ls) a = AttachedLayer l (ShellStructure ls a)

type AttachedLayer t a = Attached (Layer (LayoutType (Uncovered a)) t (Uncovered a)) a

-- === Utils === ---

type family Shelled a where Shelled (t ls) = ls :<: t


-- === Instances === --

-- Primitive
deriving instance Show (Unwrapped (ls :< a)) => Show (ls :< a)

-- Wrappers
makeWrapped ''(:<)
type instance Uncovered (ls :< a) = a
type instance Unlayered (ls :< a) = Unwrapped (ls :< a)
instance      Layered   (ls :< a)

-- Layouts
--type instance LayoutOf (ls :<: a) = LayoutOf (Unlayered (ls :<: a))
--type instance LayoutOf (Cover a) = LayoutOf (Unlayered (Cover a))

-- Construction
instance Monad m => LayerConstructor m (ls :< a) where constructLayer = return ∘ wrap'   ; {-# INLINE constructLayer #-}
instance Monad m => LayerDestructor  m (ls :< a) where destructLayer  = return ∘ unwrap' ; {-# INLINE destructLayer  #-}

-- Conversion
-- FIXME[WD]: change the implementation to be independent from layers. Wee need to implement full-lens Layered and Covered type classes
instance Castable (Unwrapped (ls :< a)) (Unwrapped (ls' :< a')) => Castable (ls :< a) (ls' :< a') where
    cast = wrapped %~ cast ; {-# INLINE cast #-}


-- Attributes
type instance                                Prop a (ls :< t) = Prop a (Unwrapped (ls :< t))
instance Getter a (Unwrapped (ls :< t)) => Getter a (ls :< t) where getter a = getter a ∘ unwrap'      ; {-# INLINE getter #-}
instance Setter a (Unwrapped (ls :< t)) => Setter a (ls :< t) where setter   = over wrapped' ∘∘ setter ; {-# INLINE setter #-}

-- Records
type instance RecordOf (ls :< t) = RecordOf t
instance (HasRecord (Uncovered (ls :< t)), Uncovered (Unwrapped (ls :< t)) ~ t, Covered (Unwrapped (ls :< t)))
      => HasRecord (ls :< t) where record = covered ∘ record

---------------------------
-- === Native layers === --
---------------------------

-- === Layout-specific === --

data Type        = Type        deriving (Show, Eq, Ord)
data Redirect    = Redirect    deriving (Show, Eq, Ord)
data Lambda      = Lambda      deriving (Show, Eq, Ord)
data Replacement = Replacement deriving (Show, Eq, Ord)

-- === Universal === --

instance Castable Bool Bool where cast = id
instance Castable (Maybe a) (Maybe a) where cast = id
instance Castable Char Char where cast = id
instance {-# OVERLAPPABLE #-} Castable a a' => Castable (Maybe a) (Maybe a') where cast = fmap cast

-- Note layer
data Note = Note deriving (Show, Eq, Ord)
type instance LayerData l Note t = String
instance Monad m => Creator m (Layer l Note a) where create = return $ Layer ""

-- Name layer
data Name = Name deriving (Show, Eq, Ord)
type instance LayerData l Name a = String
instance Monad m => Creator    m (Layer l Name a) where create     = return $ Layer ""
instance Monad m => Destructor m (Layer l Name a) where destruct _ = return ()

-- TODO: move it to a specific modules

-- Markable layer
data Markable = Markable deriving (Show, Eq, Ord)
type instance LayerData l Markable t = Bool
instance Monad m => Creator    m (Layer l Markable a) where create = return $ Layer False
instance Monad m => Destructor m (Layer l Markable a) where destruct _ = return ()

-- Meta layer
data Meta a = Meta deriving (Eq)
type instance LayerData l (Meta a) t = Maybe a
instance Monad m => Creator    m (Layer l (Meta a) b) where create = return $ Layer Nothing
instance Monad m => Destructor m (Layer l (Meta a) b) where destruct _ = return ()
