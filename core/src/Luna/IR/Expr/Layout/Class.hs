{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr.Layout.Class (module Luna.IR.Expr.Layout.Class, module X) where


import Luna.Prelude as Prelude
import Data.Property
import Luna.IR.Expr.Format
import Luna.IR.Expr.Atom

import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits (ErrorMessage(Text, ShowType, (:<>:)), TypeError)
import Type.Error
import Type.List (In)
import           Type.Set    as Set
import Data.Reprx
import Type.Bool
import Type.Inference (KnownTypeT, runInferenceT2)
import           Data.RTuple (Assoc(..), SetAssoc, LookupAssoc)
import qualified Data.RTuple as List
import Data.Abstract as X -- TODO[WD]: Consider removing the re-export

-- FIXME: remove or refactor
data Named n a
data NAME = NAME deriving (Show)




---------------------------
-- === Generalizable === --
---------------------------

-- === Definition === --

type family Generalizable a b :: Bool

generalize :: Assert (Generalizable a b) (GeneralizableError a b) => a -> b
generalize = unsafeCoerce ; {-# INLINE generalize #-}

universal :: a -> Universal a
universal = unsafeCoerce ; {-# INLINE universal #-}

abstract :: a -> Abstract a
abstract = unsafeCoerce ; {-# INLINE abstract #-}

type GeneralizableError a b = Sentence
                            ( 'Text  "Cannot generalize"
                        :</>: Ticked ('Text (TypeRepr a))
                        :</>: 'Text  "to"
                        :</>: Ticked ('Text (TypeRepr b)))


-- === Instances === --

type instance Generalizable [a] [b] = Generalizable a b

type instance Generalizable (Layout '[])               a = 'True
type instance Generalizable (Layout ((k ':= v) ': ls)) a = Generalizable v (Sub k a) && Generalizable (Layout ls) a

type instance Generalizable (Atomic a) (Form   b) = Atomic a `In` Atoms (Form b)
type instance Generalizable (Atomic a) (Atomic b) = a == b
type instance Generalizable (Form   a) (Form   b) = (Form b > Form a) `Or` (Form a == Form b)



---------------------
-- === Layouts === --
---------------------

-- === Abstract === --

data LAYOUT


-- === Definition === --

data Layout (ls :: [Assoc * *])
type family LayoutOf a


-- === Scoping === --

type family Current       a
type family Simplify      l
type family Universal     a
type family DefaultLayout p


-- === Sub === --

type family Sub t a

type instance Sub t (Layout ls) = SubLayout t ls
type family SubLayout t (ls :: [Assoc * *]) where
    SubLayout t ((t ':= v) ': _ ) = v
    SubLayout t (l         ': ls) = SubLayout t ls



-------------------
-- === Merge === --
-------------------

-- === Definition === --

type family Merge a b
type a <+> b = Merge a b


-- === Layout Merge === --

type KeySet ks = AddKeys '[] ks

type family AddKey (ls :: [*]) k :: [*]

type family AddKeys ls ks where
    AddKeys ls '[] = ls
    AddKeys ls (k ': ks) = AddKeys (AddKey ls k) ks

type instance Merge (Layout ls) (Layout ls') = Layout (MergeByKeys (AddKeys (List.Keys ls) (List.Keys ls')) ls ls')

type family MergeByKeys (allKeys :: [*]) (ls :: [Assoc * *]) (ls' :: [Assoc * *]) :: [Assoc * *] where
    MergeByKeys '[]       ls ls' = '[]
    MergeByKeys (k ': ks) ls ls' = (k ':= MergeLookup k (LookupAssoc k ls) (LookupAssoc k ls'))
                                ': MergeByKeys ks ls ls'

type family MergeLookup k l r where
    MergeLookup k 'Nothing  ('Just a)  = Merge (DefaultLayout k) a
    MergeLookup k ('Just a) 'Nothing   = Merge a (DefaultLayout k)
    MergeLookup k ('Just a) ('Just a') = Merge a a'


-- === Instances === --

type instance Merge (Form   a) ()         = Form a
type instance Merge ()         (Form   b) = Form b
type instance Merge (Form   a) (Form   b) = If (Form a > Form b) (Form a) (Form b)
type instance Merge (Form   a) (Atomic b) = (Form a) <+> (Atomic b # Format)
type instance Merge (Atomic a) (Form   b) = (Atomic a # Format) <+> (Form b)

type instance Merge (Atomic a) (Atomic b) = If (a == b) (Atomic a) ((Atomic a # Format) <+> (Atomic b # Format))-- TODO: refactor
type instance Merge (Atomic a) ()         = Atomic a
type instance Merge ()         (Atomic a) = Atomic a

type instance Merge Bottom        (Atomic a) = Bottom
type instance Merge (Atomic a) Bottom        = Bottom

-----------------
-- === Bottom === --
-----------------

data Bottom
type instance Sub t Bottom = Bottom
