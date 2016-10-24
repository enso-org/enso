{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}

module Luna.Syntax.Term.Expr.Layout where


import Prelude.Luna
import Control.Lens.Property
import Luna.Syntax.Term.Expr.Format
import Luna.Syntax.Term.Expr.Atom
import Data.RTuple (Assoc(..), SetAssoc)
import Type.Set as Set hiding (Set)
import qualified Data.RTuple as List
import Unsafe.Coerce (unsafeCoerce)


-- FIXME: remove or refactor
data Named n a





-----------------------------
-- === Layout generics === --
-----------------------------

data Layout = Layout deriving (Show)
data Name     = Name     deriving (Show) -- TODO: refactor
data Type                                -- TODO: refactor

type family DefaultLayout (p :: k)

-- TODO: refactor
type family LookupAssoc k s where
    LookupAssoc k '[]            = 'Nothing
    LookupAssoc k (k ':= v ': _) = 'Just v
    LookupAssoc k (l ':= _ ': s) = LookupAssoc k s


type family MatchLayouts m1 m2

type family MatchByKeys (ks :: [*]) (bs :: [Assoc * *]) (bs' :: [Assoc * *]) :: [Assoc * *] where
    MatchByKeys '[] bs bs' = '[]
    MatchByKeys (k ': ks) bs bs' = (k ':= MatchFinal (LookupAssoc k bs) (LookupAssoc k bs')) ': MatchByKeys ks bs bs'

type family MatchFinal l r where
    MatchFinal 'Nothing  ('Just a)  = a
    MatchFinal ('Just a) 'Nothing   = a
    MatchFinal ('Just a) ('Just a') = MatchLayouts a a'

type family MatchByKeys2 (ks :: [*]) (bs :: [Assoc * *]) (bs' :: [Assoc * *]) :: [Assoc * *] where
    MatchByKeys2 '[] bs bs' = '[]
    MatchByKeys2 (k ': ks) bs bs' = (k ':= MatchFinal2 (LookupAssoc k bs) (LookupAssoc k bs')) ': MatchByKeys2 ks bs bs'

type family MatchFinal2 l r where
    MatchFinal2 'Nothing  ('Just a)  = a
    MatchFinal2 ('Just a) 'Nothing   = a
    MatchFinal2 ('Just a) ('Just a') = Merge a a'



----------------------------
-- === Uniform layout === --
----------------------------

-- === Definitions === --

data Uniform a


-- === Utils === --

type Universal a = Set Layout (Uniform Draft) a

universal :: a -> Universal a
universal = unsafeCoerce ; {-# INLINE universal #-}



-------------------------
-- === Prim layout === --
-------------------------

-- === Definition === --

data Prim name atom


-- === Instances === --

type instance Get Atom (Prim _ atom) = atom
type instance Get Name (Prim name _) = name
type instance Get Type (Prim _ _)    = Star

type instance Set Atom a (Prim n _) = Prim n a
type instance Set Name n (Prim _ a) = Prim n a



-----------------------------
-- === Compound layout === --
-----------------------------

-- === Definition === --

data Compound t (ls :: [Assoc * *])

type family ToCompound a

-- === Instances === --

type instance Get p   (Compound t ls) = Get p ls
type instance Set p a (Compound t ls) = Compound t (SetAssoc p a ls)

type instance MatchLayouts (Compound t bs) (Compound t bs') = Compound t (MatchByKeys (Set.ToList (Concat (AsSet (List.Keys bs)) (AsSet (List.Keys bs')))) bs bs')

type instance Merge (Compound t bs) (Compound t bs') = Compound t (MatchByKeys2 (Set.ToList (Concat (AsSet (List.Keys bs)) (AsSet (List.Keys bs')))) bs bs')



-------------------------------
-- === Hierarchy layouts === --
-------------------------------

-- === Definition === --
-- TODO: refactor, it exists in Luna.Syntax.Term.Expr.Format

-- === Instances === --

type instance Sub t (a :> b) = b
type instance Atoms (a :> b) = Atoms a
