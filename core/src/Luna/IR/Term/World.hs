module Luna.IR.Term.World where

import Luna.Prelude as P

import OCI.IR.Class hiding (Import)
import OCI.IR.Name
import OCI.IR.Name.Qualified
import OCI.IR.Term
import Data.Property
import Data.ByteString (ByteString)
import Data.Families (makeLensedTerm)

import           Data.Map (Map)
import qualified Data.Map as Map
import Luna.IR.Format
import OCI.IR (Layer, Req, Model, Generalizable')
import OCI.IR.Layout.Class (Abstract, generalize)
import OCI.Pass
import OCI.Pass.Manager (MonadPassRunner)
import Data.Event
import Luna.IR.ToRefactor2 (modifyExprTerm, readTerm, readSource)

type ExprCons2' m a = (MonadRef m, Writer Net AnyExpr m, NewElemEvent m (Expr a), TermEncoder a)



-------------------
-- === World === --
-------------------

-- | World contains references to all known modules.

-- === Definition === --

newtype TermWorld a = World (Map QualName a) deriving (Show, Functor, Foldable, Traversable, Mempty, Default)
makeLensedTerm ''TermWorld


-- === Formats === --

type instance Access Format World = Draft


-- === Constructors === --

emptyWorld2  :: ExprCons2' m World => m (Expr World)
emptyWorld2' :: ExprCons2' m World => m SomeExpr
emptyWorld2  = expr $ uncheckedWorld mempty
emptyWorld2' = fmap generalize emptyWorld2


-- === Instances === --

-- Indexing
type instance IxValue (TermWorld a) = a
type instance Index   (TermWorld a) = QualName
instance At   (TermWorld a) where at = wrapped .: at
instance Ixed (TermWorld a) where ix = wrapped .: ix


--------------------------------
-- === World modification === --
--------------------------------

-- === Attribute === --

newtype WorldExpr = WorldExpr (Expr World) deriving (Show)
makeLenses ''WorldExpr


-- === World initializer === --

data WorldInitializer
type instance Abstract WorldInitializer = WorldInitializer
type instance Inputs  Net   WorldInitializer = '[]
type instance Outputs Net   WorldInitializer = '[AnyExpr]
type instance Inputs  Layer WorldInitializer = '[]
type instance Outputs Layer WorldInitializer = '[]
type instance Inputs  Attr  WorldInitializer = '[]
type instance Outputs Attr  WorldInitializer = '[WorldExpr]
type instance Inputs  Event WorldInitializer = '[] -- will never be used
type instance Outputs Event WorldInitializer = '[New // AnyExpr]
type instance Preserves     WorldInitializer = '[]

initWorld :: (MonadPassRunner m, TermEncoder World) => Pass WorldInitializer m
initWorld = putAttr @WorldExpr . wrap =<< emptyWorld2


-- === Unit registration === --

type UnitRegistration m = Req m '[ Editor  // Attr  // WorldExpr
                                 , Editor  // Layer // AnyExpr     // Model
                                 , Reader  // Layer // AnyExprLink // Model
                                 , Writer  // Net   // AnyExprLink
                                 , Emitter // New   // AnyExprLink
                                 ]

registerUnit :: (MonadRef m, UnitRegistration m) => QualName -> Expr a -> m ()
registerUnit name unit = do
    world <- getWorld
    lnk   <- link (unsafeRelayout unit) world
    modifyExprTerm world (wrapped . at name .~ Just lnk)


type TermWorld' = TermWorld (Link SomeExpr (Expr World))

type WorldReader    m = (WorldReaderReq m, MonadRef m)
type WorldReaderReq m = Req m '[ Reader // Layer // AnyExpr     // Model
                               , Reader // Layer // AnyExprLink // Model
                               , Reader // Attr  // WorldExpr
                               ]

type WorldModifier    m = (WorldReader m, WorldModifierReq m)
type WorldModifierReq m = Req m '[ Writer // Layer // AnyExpr // Model ]

getWorld' :: (Reader Attr WorldExpr m, ExprGeneralizable' World a) => m (Expr a)
getWorld  :: Reader Attr WorldExpr m => m (Expr World)
getWorld' = generalize <$> getWorld
getWorld  = unwrap     <$> getAttr @WorldExpr

readWorld :: WorldReader m => m TermWorld'
readWorld = do
    world <- getWorld
    fmap generalize . unwrap <$> readTerm world

readWorldMap :: WorldReader m => m (Map QualName (Link SomeExpr (Expr World)))
readWorldMap = unwrap <$> readWorld

-- | Warning, this function could be costly, because it reads edges between world and every module
readWorldUnitMap :: WorldReader m => m (Map QualName SomeExpr)
readWorldUnitMap = mapM readSource =<< readWorldMap

-- FIXME[WD]: We should remove unsafeGeneralize here as soon as we clean layouts.
--            World wants us to use `Expr Draft`, but `SomeExpr` is just the right type here
modifyWorld :: WorldModifier m => (TermWorld' -> TermWorld') -> m ()
modifyWorld f = do
    world <- getWorld
    modifyExprTerm world (wrapped %~ (fmap unsafeGeneralize . f . fmap unsafeGeneralize))

lookupUnit :: WorldReader m => QualName -> m (Maybe SomeExpr)
lookupUnit name = do
    world <- readWorld
    readSource <$$> world ^. at name
