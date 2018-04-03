{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Scope where

import Prologue

import qualified Control.Monad.State.Layered          as State
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import qualified Data.TreeSet                         as TreeSet
import qualified Language.Symbol.Operator.Assoc       as Assoc
import qualified Language.Symbol.Operator.Prec        as Prec
import qualified Language.Symbol.Operator.Prec.RelMap as Prec
-- import qualified OCI.IR.Name.Multipart                as Name
import qualified Text.Parser.Expr as Expr

-- import Data.Container.Map
-- -- import Data.Container.Map.IntMap      (IntMap)
-- import Data.Container.Mono
import Data.Map                       (Map)
import Data.Set                       (Set)
import Data.TreeSet                   (SparseTreeSet)
import Language.Symbol.Operator.Assoc (Assoc)
-- import OCI.IR                         (Name)
-- import OCI.IR.Name.Multipart          (MultipartName)

-- -------------------
-- -- === Scope === --
-- -------------------

-- -- === Definition === --

-- type PrecRelMap = Prec.RelMap Name

-- data Scope = Scope { _precRelMap :: PrecRelMap
--                    , _assocMap   :: Map Name Assoc
--                    , _nameTree   :: SparseTreeSet Name
--                    , _multiNames :: Map Name MultipartName
--                    }
-- makeLenses ''Scope


-- -- === PrecRelMap management === --

-- getPrecRelMap :: MonadState Scope m => m PrecRelMap
-- getPrecRelMap = view precRelMap <$> get @Scope

-- putPrecRelMap :: MonadState Scope m => PrecRelMap -> m ()
-- putPrecRelMap p = modify_ @Scope $ precRelMap .~ p


-- -- === MultipartName management === --

-- addMultipartName :: MonadState Scope m => MultipartName -> m ()
-- addMultipartName n = modify_ @Scope $ \s -> s & nameTree %~ TreeSet.insert (convert n)
--                                               & multiNames . at (convert n) .~ Just n

-- lookupMultipartNames :: MonadState Scope m => Name -> m (SparseTreeSet Name)
-- lookupMultipartNames n = (^. nameTree . ix n) <$> get @Scope

-- lookupMultipartName :: MonadState Scope m => Name -> m (Maybe MultipartName)
-- lookupMultipartName n = view (multiNames . at n) <$> get @Scope


-- -- === Instances === --

-- instance Mempty  Scope where mempty = Scope mempty mempty mempty mempty
-- instance Default Scope where def    = mempty

-- instance Monad m => Prec.RelReader Name (StateT Scope m) where
--     readRelLabel a b = fromMaybe (error $ "FIXME: prec not found between " <> show a <> " and " <> show b)
--                      . view (Prec.inferred . at a . at' b)
--                    <$> getPrecRelMap

-- instance Monad m => Prec.RelWriter Name (StateT Scope m) where
--     writeRelLabel t a b = modify_ @Scope $ precRelMap %~ Prec.insertRel t a b

-- instance Monad m => Assoc.Reader Name (StateT Scope m) where
--     readLabel n = fromMaybe Assoc.Left . Map.lookup n . view assocMap <$> get @Scope

-- instance Monad m => Assoc.Writer Name (StateT Scope m) where
--     writeLabel a n = modify_ @Scope $ assocMap %~ Map.insert n a
