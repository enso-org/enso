{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layout.Class (module OCI.IR.Layout.Class, module X) where


import Luna.Prelude as Prelude
import Data.Property
import OCI.IR.Layout.Format

import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits (ErrorMessage(Text, ShowType, (:<>:)), TypeError)
import Type.Error_old
import Type.List (In)
import           Type.Set    as Set
import Data.Reprx
import Type.Bool
import           Data.RTuple (Assoc(..), SetAssoc, LookupAssoc)
import qualified Data.RTuple as List
import Data.Abstract as X -- TODO[WD]: Consider removing the re-export


---------------------------
-- === Generalizable === --
---------------------------

-- === Definition === --

type family Generalizable a b :: Bool

type Generalizable' a b = Generalizable a b ~ 'True

type AssertGeneralizable a b = Assert (Generalizable a b) (GeneralizableError a b)

generalize :: AssertGeneralizable a b => a -> b
generalize = unsafeCoerce

universal :: a -> Universal a
universal = unsafeCoerce

abstract :: a -> Abstract a
abstract = unsafeCoerce

type GeneralizableError a b = Sentence
                            ( 'Text  "Cannot generalize"
                        :</>: Ticked ('Text (TypeRepr a))
                        :</>: 'Text  "to"
                        :</>: Ticked ('Text (TypeRepr b)))


-- === Instances === --

type instance Generalizable [a] [b] = Generalizable a b

type instance Generalizable (Layout '[])               a = 'True
type instance Generalizable (Layout ((k ':= v) ': ls)) a = Generalizable v (a ^ k) && Generalizable (Layout ls) a


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
infixr 7 ^
type a ^ t = Sub t a

type instance Sub t (Layout ls) = SubLayout t ls
type family SubLayout t (ls :: [Assoc * *]) where
    SubLayout t ((t ':= v) ': _ ) = v
    SubLayout t (l         ': ls) = SubLayout t ls
    SubLayout _ '[]               = Fail

data Fail

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


-----------------
-- === Bottom === --
-----------------

data Bottom
type instance Sub t Bottom = Bottom
type instance Generalizable Bottom Bottom = 'True
