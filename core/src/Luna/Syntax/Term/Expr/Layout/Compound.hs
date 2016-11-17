{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term.Expr.Layout.Compound where

import Prelude.Luna

import           Data.Property
import           Type.Bool
import           Data.RTuple (Assoc(..), SetAssoc, LookupAssoc)
import qualified Data.RTuple as List
import           Type.Set    as Set

import Luna.Syntax.Term.Expr.Atom
import Luna.Syntax.Term.Expr.Format
import Luna.Syntax.Term.Expr.Layout.Class



-----------------------------
-- === Compound layout === --
-----------------------------

-- === Definition === --

data Compound t (ls :: [Assoc * *])


-- === Instances === --

type instance Access p   (Compound t ls) = Access p ls
type instance Update p a (Compound t ls) = Compound t (SetAssoc p a ls)

-- Merge

type instance Merge (Compound t bs) (Compound t bs') = Compound t (MergeByKeys (Set.ToList (Concat (AsSet (List.Keys bs)) (AsSet (List.Keys bs')))) bs bs')

type family MergeByKeys (ks :: [*]) (bs :: [Assoc * *]) (bs' :: [Assoc * *]) :: [Assoc * *] where
    MergeByKeys '[]       bs bs' = '[]
    MergeByKeys (k ': ks) bs bs' = (k ':= MergeLookup (LookupAssoc k bs) (LookupAssoc k bs'))
                                ': MergeByKeys ks bs bs'

type family MergeLookup l r where
    MergeLookup 'Nothing  ('Just a)  = a
    MergeLookup ('Just a) 'Nothing   = a
    MergeLookup ('Just a) ('Just a') = Merge a a'


-- Generalize

instance (t ~ t', GeneralizableByKeys (Set.ToList (Concat (AsSet (List.Keys bs)) (AsSet (List.Keys bs')))) bs bs')
      => Generalize (Compound t bs) (Compound t' bs')

type family GeneralizableByKeys (ks :: [*]) (bs :: [Assoc * *]) (bs' :: [Assoc * *]) :: Constraint where
    GeneralizableByKeys '[]       bs bs' = ()
    GeneralizableByKeys (k ': ks) bs bs' = ( GeneralizableLookup (LookupAssoc k bs) (LookupAssoc k bs')
                                           , GeneralizableByKeys ks bs bs'
                                           )

type family GeneralizableLookup l r :: Constraint where
    GeneralizableLookup 'Nothing  _         = ()
    GeneralizableLookup ('Just a) ('Just b) = Generalize a b
