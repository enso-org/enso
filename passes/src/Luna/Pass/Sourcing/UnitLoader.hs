{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Luna.Pass.Sourcing.UnitLoader where

import Luna.Prelude as P

import Luna.IR
import OCI.IR.Name.Qualified (QualName, mkQualName, uncheckedQualNameFromPath, qualName)
import qualified OCI.IR.Name.Qualified as QualName
import qualified OCI.IR.Name.Path as Name
import OCI.Pass hiding (modify_)

import qualified Data.Map     as Map
import           Data.Map     (Map)
import qualified Data.Set     as Set
import           Data.Set     (Set)
import Control.Monad.State.Dependent
import qualified Data.Event as Event

import qualified Luna.IR.Expr as Term
import Luna.IR.Expr (Term(Term))

import Control.Monad.Raise
import Luna.IR.ToRefactor2

import qualified OCI.Pass as Pass
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.UTF8 as ByteString
import qualified Control.Exception as IO
import           System.IO.Error   (IOError)
import System.Log hiding (nested)
import           Crypto.Hash (SHA1)
import qualified Crypto.Hash as Crypto

import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import Luna.IR.Term.Unit (UnresolvedImport, Code, Iface, imports, UnitSet)
import qualified Luna.IR.Term.Unit as Import
import qualified Luna.IR.Term.Unit as Term
import qualified Luna.IR.Term.Cls  as Term
import           Luna.IR.Term.Cls  (Cls)
import qualified Data.Text.Encoding as Text
import OCI.IR.Combinators
import qualified Data.List as List
import Data.Functor.Utils (nested)
import qualified Luna.Syntax.Text.Parser.Parsing as Parser
import Luna.Syntax.Text.Parser.CodeSpan (CodeSpan)
import Data.TreeSet (SparseTreeSet)
import qualified Data.TreeSet as TreeSet
import qualified Data.TreeMap as TreeMap
import qualified GHC.Exts as GHC
import Data.Either (partitionEithers)
import Data.Container.Mono (prepend)
import qualified Luna.IR.Term.Cls as Cls
import Luna.IR.Term.Cls (cls)
import OCI.IR.Layout.Typed (type (>>))
import qualified Luna.IR.Term.World as World
import qualified Luna.IR.Term.Unit as Unit
import Luna.Syntax.Text.Parser.Errors (Invalids)

-- -------------------------
-- === Module loading === --
-- -------------------------
--
-- WARNING: This algorithm doesn't take QMonad in concideration. It have to be adjusted after we impllement QMonad.
--
-- [ ] def loadUnits:
--     [x] Data: UnitsToLoad - list of UnitProxy to be loaded
--               Sources     - mapping between Unit name and its src and interface files
--     [x] Loop over UnitsToLoad:
--         [x] Read src and interface files
--         [.] Read Unit's imports:
--             [ ] Compare computed sources hash with interface's `codeHash`
--             [.] If sources changed, parse imports header
--             [ ] Otherwise, use imports from interface
--         [x] Lookup imports in unitMap. If does not exist, create new UnitProxy to be resolved.
--         [.] Replace (in-place) current UnitProxy with UnitDecl
--
--     [.] Data: Graph of UnitDecls
--     [ ] Compute SCC of all modules and find recursive imports
--     [ ] Loop over topologicaly (from SCC) sorted UnitDecls:
--         [ ] If does not import anything:
--             [ ] If src did not change or does not exist, load interface
--             [ ] Otherwise loadUnitFromSrc
--         [ ] If it imports other units, they are already resolved, so:
--             [ ] If src did not change:
--                 [ ] If loaded import deps did not change: use interface as unit definition
--                 [ ] Otherwise <load sources>
--             [.] Otherwise loadUnitFromSrc
--
-- [.] def loadUnitFromSrc:
--     [.] Parse the sources
--     [ ] Run AA
--     [ ] For each definition:
--         [ ] Compute `irHash` (using it's IR structure, so text changes that don't matter are rejected)
--         [ ] Compute `digest` (using it's `irHash` and `digests` of it's deps (from AA)) -- Do we need to sort them topologically? Probably not, just run recursively calling function.
--         [ ] Use `irHash`, `digest` and AA deps to build DigestIR layer for given definition
--         [ ] Compare `digest` with definition with the same name from interface and use it if match (if interface exists)

-- ---------------------------
-- === Module reloading === --
-- ---------------------------
--
-- [ ] Find all related modules and remove them
-- [ ] Reload the removed modules using loading mechanism




-- -------------------------
-- === Module loading === --
-- -------------------------
--
-- WARNING: This algorithm doesn't take QMonad in concideration. It have to be adjusted after we impllement QMonad.
--
-- [ ] def loadUnits:
--     [x] Data: UnitsToLoad - list of UnitsToLoad to be loaded
--               Sources     - mapping between Unit name and its src and interface files
--     [x] Loop over UnitsToLoad:
--         [x] Read src and interface files
--         [ ] Run monad Q
--             [ ] For each Q-expression check if its `irHash` matches interface and either compute or use iface one
--             [ ] Find all import statements
--         [.] Read Unit's imports: TODO ...
--             [ ] Compare computed sources hash with interface's `codeHash`
--             [.] If sources changed, parse imports header
--             [ ] Otherwise, use imports from interface
--         [x] Lookup imports in unitMap. If does not exist, create new UnitProxy to be resolved.
--         [.] Replace (in-place) current UnitProxy with UnitDecl
--
--     [.] Data: Graph of UnitDecls
--     [ ] Compute SCC of all modules and find recursive imports
--     [ ] Loop over topologicaly (from SCC) sorted UnitDecls:
--         [ ] If does not import anything:
--             [ ] If src did not change or does not exist, load interface
--             [ ] Otherwise loadUnitFromSrc
--         [ ] If it imports other units, they are already resolved, so:
--             [ ] If src did not change:
--                 [ ] If loaded import deps did not change: use interface as unit definition
--                 [ ] Otherwise <load sources>
--             [.] Otherwise loadUnitFromSrc
--
-- [.] def loadUnitFromSrc:
--     [.] Parse the sources
--     [ ] Run AA
--     [ ] For each definition:
--         [ ] Compute `irHash` (using it's IR structure, so text changes that don't matter are rejected)
--         [ ] Compute `digest` (using it's `irHash` and `digests` of it's deps (from AA)) -- Do we need to sort them topologically? Probably not, just run recursively calling function.
--         [ ] Use `irHash`, `digest` and AA deps to build DigestIR layer for given definition
--         [ ] Compare `digest` with definition with the same name from interface and use it if match (if interface exists)




-- ---------------------------
-- -- === QMonad design === --
-- ---------------------------
--
-- Module body behaves just like class body. QMonad can be run within such body as well as in other definition places (function body, typelevel, etc).
--
-- QMonad should behave as much as possbile just like regular function body, including:
--     - allowing recursive bindings
--
--
-- -- === Impots === --
--
-- TODO: Are after running QMonad all imports known at compile? We could deliverr `import` functionality in Luna Dynamic, which will run the import passes.
-- QMonad could allow for dynamic imports in two forms:
--     1. Listed imports:
--
--        (e1, e2, e3) = dynImportBasedOn val
--        val = e1
--
--     2. Wildcard imports:
--
--        imps = dynImportAll
--        val  = imps ["somename"]
--
--     TODO: think about recursive bindings and the second form.


-- Should QMonad allow for Haskell's like type reification? Proably not, because it will require separate parsing / compilation stages like with TH.
-- QMonad evaluation generates [IRDecl], which could be recursively dependent and it's impossible to fully typecheck part of it without knowing all of decls.
-- QMonad is allowed to traverse known IRDecls, change them, affect their types by changing type's IR, but cannot access fully resolved types, because they do not exist yet.
-- TODO: Is there any use case where such information would be needed?
--
--
--     (e1, e2, e3) = dynImportBasedOn val
--     val = e1
--
--     a = def :: t
--     t = a.foo
--




passErr :: (Logging m, Throws PassEvalError m, Exception e) => e -> m a
passErr e = do
    err   . convert $ displayException e
    raise . PassEvalError $ toException e

data DescError = DescError P.String deriving (Show)
descError :: P.String -> SomeException
descError = toException . DescError

instance Exception DescError where displayException (DescError e) = e
data UnsupportedYet     = UnsupportedYet     P.String deriving (Show)
instance Exception UnsupportedYet     where displayException (UnsupportedYet     e) = "Not supported yet: " <> e


--------------------
-- === Errors === --
--------------------

data ModuleNotFound     = ModuleNotFound     QualName        deriving (Show)
data ModulesNotFound    = ModulesNotFound    [QualName]      deriving (Show) -- FIXME[WD]: Allow reporting multiple errors
data SourceLoadingError = SourceLoadingError QualName        deriving (Show)
data DecodeError        = DecodeError        SomeException   deriving (Show)
data AmbiguousPkgError  = AmbiguousPkgError  QualName [Name] deriving (Show)
data ImportsNotFound    = ImportsNotFound    QualName [Name] deriving (Show)

instance Exception ModuleNotFound     where displayException (ModuleNotFound       n) = "Module '" <> convert n <> "' not found"
instance Exception ModulesNotFound    where displayException (ModulesNotFound      n) = "Modules " <> show n <> " not found"
instance Exception SourceLoadingError where displayException (SourceLoadingError   n) = "Error occurred while loading sources of module '" <> convert n <> "'"
instance Exception DecodeError        where displayException (DecodeError          e) = "Decode error: " <> displayException e
instance Exception AmbiguousPkgError  where displayException (AmbiguousPkgError p ns) = "Module " <> show p <> " found in several packages: " <> show ns
instance Exception ImportsNotFound    where displayException (ImportsNotFound   p ns) = "Module " <> show p <> " does not export symbols: " <> show ns

moduleNotFound, sourceLoadingError :: QualName -> SomeException
moduleNotFound     = toException . ModuleNotFound
sourceLoadingError = toException . SourceLoadingError

modulesNotFound :: [QualName] -> SomeException
modulesNotFound = toException . ModulesNotFound

ambiguousPkgError :: QualName -> [Name] -> SomeException
ambiguousPkgError  = toException .: AmbiguousPkgError

importsNotFoundError :: QualName -> [Name] -> SomeException
importsNotFoundError  = toException .: ImportsNotFound

decodeError :: Exception e => e -> SomeException
decodeError = toException . DecodeError . toException

data SrcReadError  = SrcReadError  deriving (Show)
data SrcWriteError = SrcWriteError deriving (Show)


--------------------
-- === Source === --
--------------------

data Source = Source { _codePath  :: FilePath
                     , _ifacePath :: FilePath
                     } deriving (Show)
makeLenses ''Source



----------------------------
-- === SourcesManager === --
----------------------------

-- === Definition === --

type IOLogging m = (MonadIO m, Logging m)
data SourcesManager = SourcesManager
    { _readCode   :: forall m. IOLogging m => QualName -> m (Either SomeException Code)
    , _readIface  :: forall m. IOLogging m => QualName -> m (Either SomeException Iface)
    , _writeCode  :: forall m. IOLogging m => QualName -> Code  -> m (Either SomeException ())
    , _writeIface :: forall m. IOLogging m => QualName -> Iface -> m (Either SomeException ())
    }
makeLenses ''SourcesManager


-- === FSSourceManager === --

fsSourceManager :: Map QualName Source -> SourcesManager
fsSourceManager srcMap = SourcesManager (readSrc "code"      (view codePath)  decodeCode)
                                        (readSrc "interface" (view ifacePath) decodeIface)
                                        undefined
                                        undefined where

    readEither :: MonadIO m => FilePath -> m (Either IOError ByteString)
    readEither path = liftIO $ IO.handle (return . Left) $ fmap Right $ ByteString.readFile path

    readSrc :: forall a m. (Monad m, MonadIO m, Logging m) => P.String -> (Source -> FilePath) -> (ByteString -> Either SomeException a) -> QualName -> m (Either SomeException a)
    readSrc srcName pathView decoder modName = runEitherT $ withDebugBy "FSSourceManager" (convert $ "Reading module '" <> convert modName <> "' source " <> srcName) $ do
        srcDesc <- flip fromMaybeM (srcMap ^. at modName) $ left (moduleNotFound modName)
        let path = pathView srcDesc
        debug $ convert $ "Source " <> srcName <> " file path: " <> path
        binE <- readEither path <!> debug (convert $ "Source " <> srcName <> " file does not exist")
        bin  <- hoistEither $ mapLeft (const $ sourceLoadingError modName) binE
        let  srcE = decoder bin
        case srcE of Right _ -> debug $ convert $ "Source " <> srcName <> " decoded successfully"
                     Left  e -> debug $ convert $ displayException e
        hoistEither srcE

decodeCode  :: ByteString -> Either SomeException Code
decodeIface :: ByteString -> Either SomeException Iface
decodeCode  s = Right . wrap . convert $ ByteString.toString s
decodeIface s = mapLeft  (decodeError . descError . view _3)
              $ mapRight (view _3)
              $ Binary.decodeOrFail $ convert s



-------------------------
-- === UnitsToLoad === --
-------------------------

-- === Definition === --

newtype UnitsToLoad = UnitsToLoad (Set (Expr UnitProxy)) deriving (Show, Semigroup, Mempty)
makeLenses ''UnitsToLoad


-- === Utils === --

-- FIXME[WD]: After containers > 0.5.9 land in LTS, we can use Set.lookupMin
popUnitToLoad :: Editor Attr UnitsToLoad m => m (Maybe (Expr UnitProxy))
popUnitToLoad = modifyAttr @UnitsToLoad . wrapped $ \s -> (if Set.null s then Nothing else Just (Set.findMin s), Set.deleteMin s)


-- === Instances === --

type instance Item UnitsToLoad = Expr UnitProxy
instance ToList   UnitsToLoad where toList   = toList . unwrap
instance FromList UnitsToLoad where fromList = wrap . fromList



---------------------------
-- === UnitRequester === --
---------------------------

-- === Attributes === --

newtype UnitsToLoadRequest = UnitsToLoadRequest [QualName] deriving (Show, Semigroup, Mempty)
makeLenses ''UnitsToLoadRequest


-- === Pass definition === --

data UnitRequester
type instance Abstract UnitRequester = UnitRequester
type instance Inputs  Net   UnitRequester = '[]
type instance Outputs Net   UnitRequester = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer UnitRequester = '[AnyExpr // Model, AnyExprLink // Model]
type instance Outputs Layer UnitRequester = '[AnyExpr // Model, AnyExprLink // Model]
type instance Inputs  Attr  UnitRequester = '[UnitsToLoad, UnitsToLoadRequest, WorldExpr]
type instance Outputs Attr  UnitRequester = '[UnitsToLoad]
type instance Inputs  Event UnitRequester = '[] -- will never be used
type instance Outputs Event UnitRequester = '[New // AnyExpr, New // AnyExprLink]
type instance Preserves     UnitRequester = '[]

unitRequester :: MonadPassRunner m => Pass UnitRequester m
unitRequester = do
    reqs   <- unwrap <$> getAttr @UnitsToLoadRequest
    unitsM <- World.lookupUnit <$>= reqs
    let namedUnits   = zipWith (\n u -> (n,) <$> note n u) reqs unitsM :: [Either QualName (QualName, SomeExpr)]
        (fails, oks) = partitionEithers namedUnits
    when (not $ null fails) . passErr $ modulesNotFound fails
    void $ uncurry requestUnit <$>= oks

requestUnit :: ( MonadPassRunner m
               , Req m '[ Reader // Layer // AnyExpr // Model
                        , Editor // Attr  // UnitsToLoad ]
               ) => QualName -> SomeExpr -> m ()
requestUnit name expr = narrow expr >>= \case
    Nothing ->    msg "already loaded"
    Just p  -> do msg "marked to be loaded"
                  modifyAttr_ @UnitsToLoad $ wrapped %~ (Set.insert p)
    where msg s = debug . convert $ "Unit '" <> convertTo @P.String name <> "' " <> s



-----------------------------
-- === UnitInitializer === --
-----------------------------

-- === Pass definition === --

data UnitInitializer
type instance Abstract UnitInitializer = UnitInitializer
type instance Inputs  Net   UnitInitializer = '[]
type instance Outputs Net   UnitInitializer = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer UnitInitializer = '[AnyExpr // Model, AnyExprLink // Model]
type instance Outputs Layer UnitInitializer = '[AnyExpr // Model, AnyExprLink // Model]
type instance Inputs  Attr  UnitInitializer = '[WorldExpr, UnitSet]
type instance Outputs Attr  UnitInitializer = '[WorldExpr]
type instance Inputs  Event UnitInitializer = '[] -- will never be used
type instance Outputs Event UnitInitializer = '[New // AnyExpr, New // AnyExprLink]
type instance Preserves     UnitInitializer = '[]

runUnitInitializer :: MonadPassRunner m => Pass UnitInitializer m
runUnitInitializer = void initLibUnits

initLibUnits :: MonadPassRunner m => SubPass UnitInitializer m [SomeExpr]
initLibUnits = do
    libMap <- unwrap <$> getAttr @UnitSet
    uncurry initLibUnit <$>= Map.assocs libMap

initLibUnit :: MonadPassRunner m => Name -> SparseTreeSet Name -> SubPass UnitInitializer m SomeExpr
initLibUnit libName unitSet = do
    let allUnitPaths = TreeSet.paths unitSet
    debug . convert $ "Initializing library '" <> convert libName <> "' (" <> show (length allUnitPaths) <> " units): ["
                                               <> intercalate ", " (convertVia @QualName <$> allUnitPaths) <> "]"
    topUnits <- TreeSet.foldReduceBranchesM (const . initUnit . convert) (\a k _ -> return $ a |> k) [libName] unitSet
    initUnit (convert libName) False topUnits


initUnit :: MonadPassRunner m => QualName -> Bool -> [SomeExpr] -> SubPass UnitInitializer m SomeExpr
initUnit name isUserUnit subunits = do
    let unitCons    = if isUserUnit then unitProxy' else phantomUnit'
    debug . convert $ if isUserUnit then "Initializing unit '"         <> convertTo @P.String name <> "'"
                                    else "Initializing phantom unit '" <> convertTo @P.String name <> "'"
    unitCons name subunits



------------------------
-- === UnitLoader === --
------------------------

-- === Definition === --

data UnitLoader
type instance Abstract UnitLoader = UnitLoader
type instance Inputs  Net   UnitLoader = '[AnyExpr, AnyExprLink]
type instance Outputs Net   UnitLoader = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer UnitLoader = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Succs, AnyExpr // CodeSpan, AnyExprLink // Model]
type instance Outputs Layer UnitLoader = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Succs, AnyExpr // CodeSpan]
type instance Inputs  Attr  UnitLoader = '[WorldExpr, UnitsToLoad, Invalids, SourcesManager, UnitSet]
type instance Outputs Attr  UnitLoader = '[WorldExpr, UnitsToLoad, Invalids]
type instance Inputs  Event UnitLoader = '[] -- will never be used
type instance Outputs Event UnitLoader = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, OnDeepDelete // AnyExpr]
type instance Preserves     UnitLoader = '[]


unitLoader :: MonadPassRunner m => Pass UnitLoader m
unitLoader = withJustM popUnitToLoad $ \p -> go p >> unitLoader where
    go req = do
        name <- req @. qualName
        withDebug (convert $ "Loading module declaration '" <> convertTo @P.String name <> "'") $ do
            src      <- discoverSources name
            unit     <- parseUnit src
            imphub   <- unit @^. Unit.imports
            cls      <- unit @^. Unit.cls
            imps     <- readWrappedSources (unsafeGeneralize imphub :: UnresolvedImportHubType)
            impUnits <- resolveImpSrc name <$>= imps
            withDebug "Requesting imports" $ uncurry requestUnit <$>= impUnits
            partitionASGCls (unsafeGeneralize cls :: Expr ClsASG)
            replace' unit req


-- === Utils === --

discoverSources :: MonadPassRunner m => QualName -> SubPass UnitLoader m Code
discoverSources name = do
    srcMngr <- getAttr @SourcesManager
    codeE   <- srcMngr ^. readCode  $ name
    ifaceE  <- srcMngr ^. readIface $ name
    src     <- case (codeE  , ifaceE ) of
                    (Left  a, Left  i) -> passErr (SourceLoadingError name)
                    (Right a, _)       -> return a
                    (Left  a, Right i) -> passErr $ UnsupportedYet "loading modules only from interface files"
    return src -- FIXME[WD]: We should return some complex type here containing the interface when iface loading will be supported

resolveImpSrc :: MonadPassRunner m => QualName -> Expr (UnresolvedImport >> UnresolvedImportSrc) -> SubPass UnitLoader m (QualName, SomeExpr)
resolveImpSrc selfName imp = do
    src    <- imp @^. Import.termUnresolvedImport_source -- FIXME[WD]: naming
    srcImp <- src @. wrapped
    newImp <- redirectImpSrc selfName srcImp
    replace' (snd newImp) src
    return newImp

redirectImpSrc :: MonadPassRunner m => QualName -> ImportSource -> SubPass UnitLoader m (QualName, SomeExpr)
redirectImpSrc selfName = \case
    Import.Relative name -> redirectImpSrc selfName . Import.Absolute $ mergeRelUnitPaths selfName name
    Import.Absolute name -> (name   ,) <$> (fromMaybeM (passErr $ moduleNotFound name) =<< World.lookupUnit name)
    Import.World         -> ("World",) <$> getWorld'

mergeRelUnitPaths :: QualName -> QualName -> QualName
mergeRelUnitPaths src rel = mkQualName (src ^. Name.path <> rel ^. Name.path) (rel ^. QualName.target)

type UnresolvedImportHubType = Expr (UnresolvedImportHub >> UnresolvedImport >> UnresolvedImportSrc)
parseUnit = withDebug "Parsing sources" . Parser.parsingBase_ Parser.unit . convert



-- === Cls partitioning === --

-- | Converting ASGCls to Cls
partitionASGCls :: MonadPassRunner m => Expr ClsASG -> Pass UnitLoader m
partitionASGCls t = do
    Term (Term.ClsASG name _ _ unitDeclsl) <- readTerm t
    withDebug (convert $ "Converting ASGCls to Cls representation of '" <> convertTo @P.String name <> "'") $ do
        unitDecls <- mapM readSource unitDeclsl
        unitCls   <- Cls.wireCls' =<< (foldr ($) mempty <$> mapM partitionASGDecl unitDecls)
        replace unitCls t

partitionASGDecl :: MonadPassRunner m => Expr a -> SubPass UnitLoader m (Cls.TermCls SomeExpr -> Cls.TermCls SomeExpr)
partitionASGDecl decl = matchExpr decl $ \case
    ASGRootedFunction n body -> do
        n'   <- source n
        name <- matchExpr n' $ \case
            Var n -> return n
        set (Cls.methods . at name) . Just <$> rootedFunction body
    cls@(ClsASG name _ _ _)     -> return $ Cls.classes . at name ?~ unsafeGeneralize decl
    _                           -> return id





-----------------------------
-- === ImportsResolver === --
-----------------------------

-- === Definition === --

data ImportsResolver
type instance Abstract ImportsResolver = ImportsResolver
type instance Inputs  Net   ImportsResolver = '[AnyExpr, AnyExprLink]
type instance Outputs Net   ImportsResolver = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer ImportsResolver = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Succs, AnyExpr // CodeSpan, AnyExprLink // Model]
type instance Outputs Layer ImportsResolver = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Succs, AnyExpr // CodeSpan]
type instance Inputs  Attr  ImportsResolver = '[WorldExpr, UnitsToLoad, SourcesManager, UnitSet]
type instance Outputs Attr  ImportsResolver = '[WorldExpr, UnitsToLoad]
type instance Inputs  Event ImportsResolver = '[] -- will never be used
type instance Outputs Event ImportsResolver = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, OnDeepDelete // AnyExpr]
type instance Preserves     ImportsResolver = '[]


importsResolver :: MonadPassRunner m => Pass ImportsResolver m
importsResolver = do
    um <- readWorldUnitMap
    forM_ (Map.elems um) $ \expr -> matchExpr expr $ \case
        Unit{} -> resolveUnitImp $ unsafeGeneralize expr
        _      -> return ()

resolveUnitImp :: MonadPassRunner m => Expr Unit -> SubPass ImportsResolver m ()
resolveUnitImp unit = do
    hub <- unit @^. Unit.imports
    matchExpr hub $ \case
        ImportHub {}             -> debug "Imports already resolved"
        UnresolvedImportHub imps -> do
            imps <- fmap concat . mapM (resolveImp . unsafeGeneralize) =<< readSources imps -- FIXME[WD]: unsafe
            hub' <- impHub' (Map.fromList imps)
            replace hub' hub

resolveImp :: forall m. MonadPassRunner m => Expr (UnresolvedImport >> Unit) -> SubPass ImportsResolver m [(Name, SomeExpr)]
resolveImp imp = do
    src <- imp @^. Import.termUnresolvedImport_source -- FIXME[WD]: naming
    tgt <- imp @.  Import.targets

    cls <- src @^. Unit.cls
    Term (Term.Cls _ conses classes methods) <- readTerm (unsafeGeneralize cls :: Expr Cls)
    let nameMaps     = [conses, classes, methods] :: [Map Name (ExprLink Draft Cls)]
        lookupName :: Name -> SubPass ImportsResolver m (Maybe (Expr Draft))
        lookupName n = mapM readSource . maybeHead . catMaybes $ Map.lookup n <$> nameMaps
        allNames     = concat $ Map.keys <$> nameMaps
        mkImport   n = mapM (imp' $ generalize src) =<< lookupName n
        impNames     = case tgt of Import.Everything -> allNames
                                   Import.Listed lst -> lst
    maybeNewImps <- zip impNames <$> (mkImport <$>= impNames)
    let (errNames, newImps) = partitionMaybeTaggedList maybeNewImps
    when (not $ null errNames) . passErr $ importsNotFoundError "LOC-TODO" errNames
    return newImps
