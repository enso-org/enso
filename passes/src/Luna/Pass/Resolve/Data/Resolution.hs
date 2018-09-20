{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Resolve.Data.Resolution where

import Prologue

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Luna.IR                       as IR
import qualified Luna.Pass.Attr                as Attr
import qualified Luna.Pass.Sourcing.Data.Class as Class
import qualified Luna.Pass.Sourcing.Data.Unit  as Unit
import qualified Luna.Pass.Sourcing.Data.Def   as Def

import Data.Map (Map)

------------------------
-- === Resolution === --
------------------------

data Resolution a = Resolved a
                  | Ambiguous [a]
                  | Unresolved
                  deriving Show

instance Mempty (Resolution a) where
    mempty = Unresolved

instance Semigroup (Resolution a) where
    Unresolved <> a = a
    a <> Unresolved = a
    Ambiguous as <> Resolved a   = Ambiguous (as <> [a])
    Resolved a   <> Ambiguous as = Ambiguous (a : as)
    Resolved a   <> Resolved b   = Ambiguous [a, b]
    Ambiguous as <> Ambiguous bs = Ambiguous (as <> bs)

-----------------------
-- === Resolvers === --
-----------------------

-- === Resolves === --

class Resolves r a b where
    resolve :: a -> r -> Resolution b

-- === Resolver === --

newtype Resolver a b = Resolver (Map a (Resolution b)) deriving (Show, Default)
makeLenses ''Resolver

instance Ord a => Resolves (Resolver a b) a b where
    resolve k = Map.findWithDefault mempty k . unwrap

instance Mempty (Resolver a b) where
    mempty = Resolver def

instance Ord a => Semigroup (Resolver a b) where
    Resolver a <> Resolver b = Resolver $ Map.unionWith (<>) a b

-- === ConsResolver === --

data ConsRef = ConsRef
    { _unitName  :: IR.Qualified
    , _className :: IR.Name
    , _cons      :: Class.Constructor
    } deriving Show
makeLenses ''ConsRef

newtype ConsResolver = ConsResolver (Resolver IR.Name ConsRef)
    deriving (Show, Default, Mempty, Semigroup)
type instance Attr.Type ConsResolver = Attr.Atomic
makeLenses ''ConsResolver

instance Resolves ConsResolver IR.Name ConsRef where
    resolve k = resolve k . unwrap

-- === DefResolver === --

newtype DefRef = DefRef IR.Qualified deriving Show
makeLenses ''DefRef

newtype DefResolver = DefResolver (Resolver IR.Name DefRef)
    deriving (Show, Default, Mempty, Semigroup)
type instance Attr.Type DefResolver = Attr.Atomic
makeLenses ''DefResolver

instance Resolves DefResolver IR.Name DefRef where
    resolve k = resolve k . unwrap

-- === ClassResolver === --

newtype ClassRef = ClassRef IR.Qualified deriving Show
makeLenses ''ClassRef

newtype ClassResolver = ClassResolver (Resolver IR.Name ClassRef)
    deriving (Show, Default, Mempty, Semigroup)
type instance Attr.Type ClassResolver = Attr.Atomic
makeLenses ''ClassResolver

instance Resolves ClassResolver IR.Name ClassRef where
    resolve k = resolve k . unwrap


-- === UnitResolver === --

data UnitResolver = UnitResolver
    { _conses  :: ConsResolver
    , _defs    :: DefResolver
    , _classes :: ClassResolver
    } deriving Show
makeLenses ''UnitResolver

instance Mempty UnitResolver where
    mempty = UnitResolver mempty mempty mempty

instance Semigroup UnitResolver where
    UnitResolver c1 d1 cl1 <> UnitResolver c2 d2 cl2 = UnitResolver (c1  <> c2)
                                                                    (d1  <> d2)
                                                                    (cl1 <> cl2)

----------------------
-- === Builders === --
----------------------

resolverFromUnit :: IR.Qualified -> Unit.Unit -> UnitResolver
resolverFromUnit unitName (Unit.Unit defs clss) = result where
    result   = UnitResolver consRes (DefResolver $ Resolver defsRes) clsRes
    defsRes  = Resolved (DefRef unitName) <$ unwrap defs
    consRes  = mconcat consRess
    consRess = Map.elems $ Map.mapWithKey (consResolverFromClass unitName) classes
    classes  = view Def.documented <$> clss
    clsRes   = ClassResolver . Resolver $ Resolved (ClassRef unitName) <$ clss

consResolverFromClass ::
    IR.Qualified -> IR.Name -> Class.Class -> ConsResolver
consResolverFromClass unitName className cls = ConsResolver resolver where
    constructors = cls ^. Class.constructors
    resolver = Resolver $ Resolved . ConsRef unitName className <$> constructors

resolverForUnit :: Map IR.Qualified UnitResolver
                -> IR.Qualified
                -> Unit.Imports
                -> UnitResolver
resolverForUnit units unitName (Unit.Imports imps) = resolver where
    resolver          = mconcat . Map.elems $ relevantResolvers
    relevantResolvers = Map.restrictKeys units (Set.fromList $ unitName : imps)

