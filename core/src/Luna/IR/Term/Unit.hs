{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term.Unit where

import Luna.Prelude as P

import OCI.IR.Class as IR hiding (Import)
import OCI.IR.Name
import qualified OCI.IR.Name.Path as Name
import           OCI.IR.Name.Path (HasPath, path)
import           OCI.IR.Name.Qualified
import OCI.IR.Term
import Data.Property
import Data.ByteString (ByteString)
import Data.Families (makeLensedTerm)
import qualified Data.TreeSet as TreeSet
import           Data.TreeSet (SparseTreeSet)
import qualified GHC.Exts as GHC
import Data.Map (Map)

import Data.Text32 (Text32)

newtype Code = Code Text32 deriving (Show, Generic)
makeLenses ''Code

instance Binary Code

instance Convertible Code Text32 where convert = unwrap
instance Convertible Text32 Code where convert = wrap
instance {-# OVERLAPPABLE #-} Convertible Text32 a => Convertible Code a where convert = convertVia @Text32
instance {-# OVERLAPPABLE #-} Convertible a Text32 => Convertible a Code where convert = convertVia @Text32

--------------------
-- === Digest === --
--------------------

newtype Digest = Digest ByteString deriving (Generic, Show)
makeLenses ''Digest

instance Binary Digest



---------------------------------
-- === UnresolvedImportSrc === --
---------------------------------

-- === Definition === --

newtype TermUnresolvedImportSrc a = UnresolvedImportSrc ImportSource deriving (Show, Generic, Functor, Foldable, Traversable)
data    ImportSource = Relative QualName
                     | Absolute QualName
                     | World
                     deriving (Show, Generic)
makeLensedTerm ''TermUnresolvedImportSrc


------------------------------
-- === UnresolvedImport === --
------------------------------

-- === Definition === --

data TermUnresolvedImport a = UnresolvedImport { __source :: !a , _targets :: !UnresolvedImportTgt } deriving (Show, Functor, Foldable, Traversable, Generic)
data UnresolvedImportTgt    = Everything | Listed [Name] deriving (Show, Generic)
makeLensedTerm ''TermUnresolvedImport


-- === Instances === --

-- Binary
instance Binary a => Binary (TermUnresolvedImport a)
instance Binary UnresolvedImportTgt

-- Monoids
instance Mempty    UnresolvedImportTgt where mempty = Listed mempty
instance Semigroup UnresolvedImportTgt where
    Everything <> _           = Everything
    _          <> Everything  = Everything
    Listed lst <> Listed lst' = Listed $ lst <> lst'



---------------------------------
-- === UnresolvedImportHub === --
---------------------------------

-- === Definition === --

newtype TermUnresolvedImportHub a = UnresolvedImportHub [a] deriving (Show, Functor, Foldable, Traversable, Generic)
makeLensedTerm ''TermUnresolvedImportHub



--------------------
-- === Import === --
--------------------

-- === Definition === --

data TermImport a = Import { _source :: !a , _target :: !a } deriving (Show, Functor, Foldable, Traversable, Generic)
makeLensedTerm ''TermImport



-----------------------
-- === ImportHub === --
-----------------------

-- === Definition === --

newtype TermImportHub a = ImportHub (Map Name a) deriving (Show, Functor, Foldable, Traversable, Generic)
makeLensedTerm ''TermImportHub

-----------------------
-- === FFI Nodes === --
-----------------------

data TermForeignImportList a = ForeignImportList
    { __language :: !Name
    , __imports  :: ![a]
    } deriving (Eq, Foldable, Functor, Generic, Show)
makeLensedTerm ''TermForeignImportList

data TermForeignLocationImportList a = ForeignLocationImportList
    { __importLocation :: !a
    , __imports        :: ![a]
    } deriving (Eq, Foldable, Functor, Generic, Show)
makeLensedTerm ''TermForeignLocationImportList

data TermForeignSymbolImport a = ForeignSymbolImport
    { __safety     :: !a
    , __importName :: !a
    , __localName  :: !Name
    , __type       :: !a
    } deriving (Eq, Foldable, Functor, Generic, Show)
makeLensedTerm ''TermForeignSymbolImport

-- FIXME [Ara] May be able to become a `Maybe` pending discussion in the
-- following issue: https://github.com/luna/luna/issues/179
data ForeignImportType
    = Default -- Unsafe if not specified.
    | Safe
    | Unsafe
    deriving (Eq, Generic, Show)

data TermForeignImportSafety a = ForeignImportSafety
    { __safety :: !ForeignImportType
    } deriving (Eq, Foldable, Functor, Generic, Show)
makeLensedTerm ''TermForeignImportSafety

-- Useful for a resolution pass after parse time.
{- data TermResolvedForeignImport a = ResolvedForeignImport -}
    {- { __importLocation :: !a -}
    {- , __importName     :: !a -}
    {- , __localName      :: !Name -}
    {- } deriving (Eq, Foldable, Functor, Generic, Show) -}
{- makeLensedTerm ''TermResolvedForeignImport -}

-----------------------
-- === Interface === --
-----------------------

-- === Definition === --

data Iface = Iface { __digestIR :: !DigestIR
                   , __codeHash :: !Digest
                   , __rest     :: !ByteString -- To be refined
                   } deriving (Generic, Show)

data DigestIR = DigestIR { _digest :: !Digest -- irHash + deps hashes
                         , _irHash :: !Digest -- IR shape hash
                         , _irDeps :: ![QualName]
                         } deriving (Generic, Show)

makeLenses ''Iface
makeLenses ''DigestIR

instance Binary Iface
instance Binary DigestIR




-- FIXME[WD]: We should refactor UnitProxy to OCI after removing UniTerm and relaxing IR deps.
-----------------------
-- === UnitProxy === --
-----------------------

-- === Definition === --

data TermUnitProxy a = UnitProxy { __name :: QualName, __subunits :: ![a]} deriving (Show, Functor, Foldable, Traversable)
makeLensedTerm ''TermUnitProxy


-- === Instances === --

instance HasQualName (TermUnitProxy a) where qualName = termUnitProxy_name



------------------
-- === Unit === --
------------------

-- === Definition === --

data TermUnit a = Unit { _imports :: !a , _subunits :: ![a], _cls :: !a } deriving (Show, Functor, Foldable, Traversable)
makeLensedTerm ''TermUnit




-- ------------------
-- === UnitSet === --
-- ------------------

-- === Definition === --

newtype UnitSet = UnitSet (Map Name (SparseTreeSet Name)) deriving (Show)
makeLenses ''UnitSet


-- === Instances === --

-- List
type instance Item UnitSet = Item (Unwrapped UnitSet)
deriving instance ToList   UnitSet
deriving instance FromList UnitSet
instance GHC.IsList UnitSet where
    type Item UnitSet = Item UnitSet
    toList   = toList
    fromList = fromList

-- Indexing
type instance Index   UnitSet = Index   (Unwrapped UnitSet)
type instance IxValue UnitSet = IxValue (Unwrapped UnitSet)
instance At   UnitSet where at = wrapped .: at
instance Ixed UnitSet where ix = wrapped .: ix
