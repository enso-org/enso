{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Scope where

import Prologue

import qualified Control.Monad.State.Layered          as State
import qualified Control.Monad.State.Layered          as State
import qualified Data.Map.Strict                      as Map
import qualified Data.Set                             as Set
import qualified Data.TreeSet                         as TreeSet
import qualified Language.Symbol.Operator.Assoc       as Assoc
import qualified Language.Symbol.Operator.Prec        as Prec
import qualified Language.Symbol.Operator.Prec.RelMap as Prec
import qualified Luna.Data.Name                       as Name

-- import Data.Container.Map
-- -- import Data.Container.Map.IntMap      (IntMap)
-- import Data.Container.Mono
import Control.Lens.Utils             (at')
import Control.Monad.State.Layered    (StateT)
import Data.Map.Strict                (Map)
import Data.Set                       (Set)
import Data.TreeSet                   (SparseTreeSet)
import Language.Symbol.Operator.Assoc (Assoc)
import Luna.IR                        (Name)


-------------------
-- === Scope === --
-------------------

-- === Definition === --

type PrecRelMap = Prec.RelMap Name

data Scope = Scope
    { _precRelMap :: PrecRelMap
    , _assocMap   :: Map Name Assoc
    , _nameTree   :: SparseTreeSet Name
    , _multiNames :: Map Name (NonEmpty Name)
    } deriving (Show)
makeLenses ''Scope


-- === PrecRelMap management === --

getPrecRelMap :: State.Monad Scope m => m PrecRelMap
getPrecRelMap = view precRelMap <$> State.get @Scope

putPrecRelMap :: State.Monad Scope m => PrecRelMap -> m ()
putPrecRelMap p = State.modify_ @Scope $ precRelMap .~ p


-- === Name.Multipart management === --

addMultipartName :: State.Monad Scope m => NonEmpty Name -> m ()
addMultipartName n = State.modify_ @Scope
                 $ \s -> s & nameTree %~ TreeSet.insert n
                           & multiNames . at (Name.mixfix n) .~ Just n

lookupMultipartNames :: State.Monad Scope m => Name -> m (SparseTreeSet Name)
lookupMultipartNames n = (^. nameTree . ix n) <$> State.get @Scope

lookupMultipartName :: State.Monad Scope m => Name -> m (Maybe (NonEmpty Name))
lookupMultipartName n = view (multiNames . at n) <$> State.get @Scope


-- === Instances === --

instance Mempty  Scope where mempty = Scope mempty mempty mempty mempty
instance Default Scope where def    = mempty

-- TODO:
-- Maybe the check if names are eq should be refactored to the Prec manager?
instance Monad m => Prec.RelReader Name (StateT Scope m) where
    readRelLabel = \a b -> if a == b
        then pure $ Just EQ
        else view (Prec.inferred . at a . at' b) <$> getPrecRelMap
    {-# INLINE readRelLabel #-}

instance Monad m => Prec.RelWriter Name (StateT Scope m) where
    writeRelLabel t a b = State.modify_ @Scope $ precRelMap %~ Prec.insertRel t a b

instance Monad m => Assoc.Reader Name (StateT Scope m) where
    readLabel n = fromMaybe Assoc.Left . Map.lookup n . view assocMap <$> State.get @Scope

instance Monad m => Assoc.Writer Name (StateT Scope m) where
    writeLabel a n = State.modify_ @Scope $ assocMap %~ Map.insert n a

