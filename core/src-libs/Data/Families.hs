{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE NoOverloadedStrings       #-}
{-# LANGUAGE NoOverloadedLists         #-}

module Data.Families (module Data.Families, module X) where

import Prologue_old (if_)
import Prelude
import Data.Default
import Data.Functor.Utils hiding ((.))
import Data.Monoid
import Data.String
import Control.Monad.Fail
import Control.Monad.State
import Control.Lens.Utils hiding (cons)
import Control.Applicative
import Control.Monad.Fail

import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH as X hiding (Con, Dec)
import qualified Data.Char as Char



class HasName    a where name    :: Lens' a (NameOf a)
class HasCtx     a where ctx     :: Lens' a [Pred]
class HasKind    a where kind    :: Lens' a (Maybe Kind)
class HasDerivs  a where derivs  :: Lens' a [Pred]


class IsTH t a where
    th :: a -> t

instance IsTH t a => IsTH (Q t) a where
    th = return . th ; {-# INLINE th #-}

-- instance {-# OVERLAPPABLE #-}             IsTH a    a  where th = id          ; {-# INLINE th #-}
instance {-# OVERLAPPABLE #-} IsTH t a => IsTH [t]  a  where th = return . th ; {-# INLINE th #-}
instance {-# OVERLAPPABLE #-} IsTH t a => IsTH [t] [a] where th = fmap th     ; {-# INLINE th #-}
instance {-# OVERLAPPABLE #-} (IsTH a b', b ~ Maybe b')
      => IsTH (Maybe a) b  where th = fmap th ; {-# INLINE th #-}


type ToOverlap = IsTH Overlap
type ToCxt     = IsTH Cxt

-------------------
-- === Utils === --
-------------------

class ToUpper a where
    toUpper :: a -> a

instance ToUpper String where
    toUpper []     = []
    toUpper (c:cs) = Char.toUpper c : toUpper cs
    {-# INLINE toUpper #-}


------------------
-- === Type === --
------------------

class HasType a where
    tp :: Lens' a Type

class MayHaveType a where
    checkTp :: Lens' a (Maybe Type)

type ToType = IsTH Type
toType :: ToType a => a -> Type
toType = th

type ToExp = IsTH Exp
toExp :: ToExp a => a -> Exp
toExp = th

instance IsTH Type Type where th = id ; {-# INLINE th #-}
instance IsTH Exp  Exp  where th = id ; {-# INLINE th #-}
instance IsTH Type String where
    th = LitT . StrTyLit ; {-# INLINE th #-}

instance ToType a => IsTH Type (ZipList a) where
    th = toType . getZipList ; {-# INLINE th #-}

instance {-# OVERLAPPABLE #-} ToType a => IsTH Type [a] where
    th lst = foldr AppT PromotedNilT (AppT PromotedConsT . toType <$> lst) ; {-# INLINE th #-}



------------------
-- === Name === --
------------------

type family NameOf a

data TypeName = TypeName Name
data VarName  = VarName  Name
makeWrapped ''TypeName
makeWrapped ''VarName

class FromName     a where fromName     :: Name     -> a
class FromTypeName a where fromTypeName :: TypeName -> a
class FromVarName  a where fromVarName  :: VarName  -> a

class ToName     a where toName     :: a -> Name
class ToTypeName a where toTypeName :: a -> TypeName
class ToVarName  a where toVarName  :: a -> VarName

instance FromName Name where
    fromName = id ; {-# INLINE fromName #-}

instance FromName TypeName where
    fromName = TypeName ; {-# INLINE fromName #-} -- FIXME[WD]: add letter case assertions

instance FromName VarName where
    fromName = VarName ; {-# INLINE fromName #-} -- FIXME[WD]: add letter case assertions

instance FromTypeName TypeName where
    fromTypeName = id ; {-# INLINE fromTypeName #-}

instance FromVarName VarName where
    fromVarName = id ; {-# INLINE fromVarName #-}

instance IsTH Type TypeName where
    th = ConT . unwrap' ; {-# INLINE th #-}

instance ToUpper TypeName where
    toUpper = wrapped' %~ toUpper ; {-# INLINE toUpper #-}

instance ToUpper Name where
    toUpper = mkName . toUpper . nameBase ; {-# INLINE toUpper #-}

instance ToName     Name     where toName     = id ; {-# INLINE toName     #-}
instance ToVarName  VarName  where toVarName  = id ; {-# INLINE toVarName  #-}
instance ToTypeName TypeName where toTypeName = id ; {-# INLINE toTypeName #-}

instance ToVarName  Name     where toVarName  = VarName  ; {-# INLINE toVarName  #-} -- FIXME[WD]: add letter case assertions
instance ToTypeName Name     where toTypeName = TypeName ; {-# INLINE toTypeName #-} -- FIXME[WD]: add letter case assertions

instance ToName     String   where toName     = mkName              ; {-# INLINE toName     #-}
instance ToVarName  String   where toVarName  = toVarName  . toName ; {-# INLINE toVarName  #-}
instance ToTypeName String   where toTypeName = toTypeName . toName ; {-# INLINE toTypeName #-}

instance IsTH Name VarName  where th = unwrap' ; {-# INLINE th #-}
instance IsTH Name TypeName where th = unwrap' ; {-# INLINE th #-}

instance IsTH Exp VarName  where th = VarE . unwrap' ; {-# INLINE th #-}
instance IsTH Exp TypeName where th = ConE . unwrap' ; {-# INLINE th #-}

instance IsTH Type VarName  where th = VarT . unwrap' ; {-# INLINE th #-}


-- === Generation === --

strNameCycle :: [String]
strNameCycle = (return <$> ['a' .. 'z']) <> strNameCycle' [] (show <$> [0..]) where
    strNameCycle' []     (n:ns) = strNameCycle' ['a' .. 'z'] ns
    strNameCycle' (b:bs) ns     = (b : head ns) : strNameCycle' bs ns

nameCycle :: FromName a => [a]
nameCycle = fromName . mkName <$> strNameCycle ; {-# INLINE nameCycle #-}

newNameCycle :: Q [Name]
newNameCycle = mapM newName strNameCycle ; {-# INLINE newNameCycle #-}

genNames :: FromName a => Int -> [a]
genNames = flip take nameCycle ; {-# INLINE genNames #-}

genName :: FromName a => a
genName = head nameCycle ; {-# INLINE genName #-}

newNames :: Int -> Q [Name]
newNames = mapM newName . flip take strNameCycle ; {-# INLINE newNames #-}

varName :: String -> VarName
varName = fromString ; {-# INLINE varName #-}

typeName :: String -> TypeName
typeName = fromTypeName . fromString ; {-# INLINE typeName #-}


-- === Instances === --

instance IsString Name where
    fromString = mkName ; {-# INLINE fromString #-}

instance IsString VarName where
    fromString = VarName . fromString ; {-# INLINE fromString #-} -- FIXME[WD]: add letter case assertions

instance IsString TypeName where
    fromString = TypeName . fromString ; {-# INLINE fromString #-} -- FIXME[WD]: add letter case assertions


-----------------
-- === App === --
-----------------

data App a = App { _app_src  :: a
                 , _app_args :: [a]
                 }

makeLenses ''App


class FromApp a where
    fromApp :: App a -> a



apps :: a -> [a] -> App a
apps = App ; {-# INLINE apps #-}

app :: a -> a -> App a
app a = apps a . return ; {-# INLINE app #-}


instance ToType a => IsTH TH.Type (App a) where
    th (App src args) = foldl AppT (th src) (th <$> args) ; {-# INLINE th #-}

instance ToExp a => IsTH TH.Exp (App a) where
    th (App src args) = foldl AppE (th src) (th <$> args) ; {-# INLINE th #-}


-----------------
-- === Var === --
-----------------

data Var = Var { _var_name :: VarName
               , _var_tp   :: Maybe Type
               }

makeLenses ''Var
instance HasName     Var where name    = var_name ; {-# INLINE name    #-}
instance MayHaveType Var where checkTp = var_tp   ; {-# INLINE checkTp #-}

type instance NameOf Var = VarName
class HasParams a where params  :: Lens' a [Var]


-- === Instances === --

instance {-# OVERLAPPING #-} n ~ VarName => Default (n -> Var) where
    def n = Var n def ; {-# INLINE def #-}

instance FromName Var where
    fromName = def . fromName ; {-# INLINE fromName #-}

instance IsTH TyVarBndr Var where
    th a = maybe PlainTV (flip KindedTV) (a ^. checkTp) (th $ a ^. name) ; {-# INLINE th #-}



-----------------
-- === Con === --
-----------------

data Con = Con { _con_name   :: TypeName
               , _con_params :: [Var]
               }

class HasCons a where cons :: Lens' a [Con]

makeLenses ''Con
instance HasName   Con where name   = con_name   ; {-# INLINE name   #-}
instance HasParams Con where params = con_params ; {-# INLINE params #-}

type instance NameOf Con = TypeName


-- === Modification === --

addCon :: HasCons t => Con -> t -> t
addCon a = cons %~ (<> [a]) ; {-# INLINE addCon #-}

addParam :: HasParams t => Var -> t -> t
addParam a = params %~ (<> [a]) ; {-# INLINE addParam #-}


-- === Instances === --

instance {-# OVERLAPPING #-} n ~ TypeName => Default (n -> Con) where
    def n = Con n def ; {-# INLINE def #-}

instance IsTH TH.Con Con where
    th a = NormalC (a ^. name . wrapped') [] -- FIXME[WD]: finish cases


------------------
-- === Data === --
------------------

data Data = Data { _data_ctx     :: [Pred]
                 , _data_name    :: TypeName
                 , _data_params  :: [Var]
                 , _data_kind    :: Maybe Kind
                 , _data_cons    :: [Con]
                 , _data_derivs  :: [Pred]
                 }

makeLenses ''Data
instance HasName   Data where name    = data_name    ; {-# INLINE name    #-}
instance HasCtx    Data where ctx     = data_ctx     ; {-# INLINE ctx     #-}
instance HasParams Data where params  = data_params  ; {-# INLINE params  #-}
instance HasKind   Data where kind    = data_kind    ; {-# INLINE kind    #-}
instance HasCons   Data where cons    = data_cons    ; {-# INLINE cons    #-}
instance HasDerivs Data where derivs  = data_derivs  ; {-# INLINE derivs  #-}

type instance NameOf Data = TypeName


-- === Construction === --

data' :: TypeName -> Data
data' = def ; {-# INLINE data' #-}

-- | phantom takes number of data parameters and generates a phantom data type
--   for example `phantom 2 "Foo"` generated `data Foo a b`
phantomN :: Int -> TypeName -> Data
phantomN i n = data' n & params .~ genNames i ; {-# INLINE phantomN #-}

phantom :: TypeName -> Data
phantom = phantomN 0 ; {-# INLINE phantom #-}

phantom' :: TypeName -> Data
phantom' = phantomN 1 ; {-# INLINE phantom' #-}

phantom'' :: TypeName -> Data
phantom'' = phantomN 2 ; {-# INLINE phantom'' #-}

phantom''' :: TypeName -> Data
phantom''' = phantomN 3 ; {-# INLINE phantom''' #-}

phantom'''' :: TypeName -> Data
phantom'''' = phantomN 4 ; {-# INLINE phantom'''' #-}

phantom''''' :: TypeName -> Data
phantom''''' = phantomN 5 ; {-# INLINE phantom''''' #-}

instance {-# OVERLAPPING #-} n ~ TypeName => Default (n -> Data) where
    def n = Data def n def def def def ; {-# INLINE def #-}


-- === conversions === --

instance IsTH TH.Dec Data where
    th a = DataD (a ^. ctx) (th $ a ^. name) (th $ a ^. params) (a ^. kind) (th $ a ^. cons) (a ^. derivs) ; {-# INLINE th #-}


---------------------
-- === TypeSyn === --
---------------------

data TypeSyn = TypeSyn { _typeSyn_name   :: TypeName
                       , _typeSyn_params :: [Var]
                       , _typeSyn_tp     :: Type
                       }
makeLenses ''TypeSyn
instance HasName   TypeSyn where name   = typeSyn_name   ; {-# INLINE name   #-}
instance HasParams TypeSyn where params = typeSyn_params ; {-# INLINE params #-}
instance HasType   TypeSyn where tp     = typeSyn_tp     ; {-# INLINE tp     #-}

type instance NameOf TypeSyn = TypeName


-- === Construction === --

alias :: ToType t => TypeName -> t -> TypeSyn
alias n t = TypeSyn n def (toType t) ; {-# INLINE alias #-}


-- === Instances === --

instance IsTH TH.Dec TypeSyn where
    th a = TySynD (th $ a ^. name) (th $ a ^. params) (a ^. tp) ; {-# INLINE th #-}



---------------------------
-- === Type Instance === --
---------------------------

data TypeInstance = TypeInstance { _typeInstance_name   :: TypeName
                                 , _typeInstance_args   :: [Type]
                                 , _typeInstance_result :: Type
                                 }


typeInstance :: (ToTypeName n, ToType t, ToType t') => n -> [t] -> t' -> TypeInstance
typeInstance n args res = TypeInstance (toTypeName n) (toType <$> args) (toType res) ; {-# INLINE typeInstance #-}

typeInstance' :: (ToTypeName n, ToType t, ToType t') => n -> t -> t' -> TypeInstance
typeInstance' n = typeInstance n . return ; {-# INLINE typeInstance' #-}


instance IsTH TH.Dec TypeInstance where
    th (TypeInstance n as r) = TySynInstD (th n) (TySynEqn as r) ; {-# INLINE th #-}


----------------------------
-- === Class Instance === --
----------------------------

data ClassInstance = ClassInstance { _classInstance_overlap :: Maybe Overlap
                                   , _classInstance_ctx     :: Cxt
                                   , _classInstance_name    :: TypeName
                                   , _classInstance_tp      :: [Type]
                                   , _classInstance_decs    :: [Dec]
                                   }


classInstance :: (ToCxt ctx, ToTypeName n, ToType t, IsDec dec)
              => ctx -> n -> [t] -> [dec] -> ClassInstance
classInstance ctx n ts desc = ClassInstance Nothing (th ctx) (toTypeName n) (th <$> ts) (toDec <$> desc) ; {-# INLINE classInstance #-}

classInstance' :: (ToTypeName n, ToType t, IsDec dec)
              => n -> [t] -> [dec] -> ClassInstance
classInstance' = classInstance ([] :: Cxt) ; {-# INLINE classInstance' #-}


instance IsTH TH.Dec ClassInstance where
    th (ClassInstance olap cxt n ts decs) = InstanceD olap cxt (th $ apps (th n) ts) (th decs) ; {-# INLINE th #-}


----------------------
-- === Function === --
----------------------

data Function = Function { _function_name   :: VarName
                         , _function_clause :: [Clause]
                         }


function :: ToVarName n => n -> [Clause] -> Function
function n = Function (toVarName n) ; {-# INLINE function #-}

function' :: ToVarName n => n -> Clause -> Function
function' n = function n . return ; {-# INLINE function' #-}


instance IsTH TH.Dec Function where
    th (Function n cs) = FunD (th n) cs ; {-# INLINE th #-}



-----------------
-- === Dec === --
-----------------

data Dec = DataDec          { _dec_dataDec          :: Data          }
         | TypeSynDec       { _dec_typeSynDec       :: TypeSyn       }
         | TypeInstanceDec  { _dec_typeInstanceDec  :: TypeInstance  }
         | ClassInstanceDec { _dec_classInstanceDec :: ClassInstance }
         | FunctionDec      { _dec_functionDec      :: Function      }


class IsDec a where
    toDec :: a -> Dec


instance IsTH TH.Dec Dec where
    th = \case
        DataDec          a -> th a
        TypeSynDec       a -> th a
        TypeInstanceDec  a -> th a
        ClassInstanceDec a -> th a
        FunctionDec      a -> th a
    {-# INLINE th #-}

instance IsDec Data          where toDec = DataDec          ; {-# INLINE toDec #-}
instance IsDec TypeSyn       where toDec = TypeSynDec       ; {-# INLINE toDec #-}
instance IsDec TypeInstance  where toDec = TypeInstanceDec  ; {-# INLINE toDec #-}
instance IsDec ClassInstance where toDec = ClassInstanceDec ; {-# INLINE toDec #-}
instance IsDec Function      where toDec = FunctionDec      ; {-# INLINE toDec #-}


makeLenses ''Dec



-----------------------
-- === THBuilder === --
-----------------------

type    THBuilder      = THBuilderT Q
newtype THBuilderT m a = THBuilderT (StateT [Dec] m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix, MonadFail)
makeWrapped ''THBuilderT

class Monad m => MonadTH a m where
    define :: a -> m ()

instance {-# OVERLAPPABLE #-} (IsDec a, Monad m) => MonadTH  a          (THBuilderT m) where define a = THBuilderT $ modify (<> [toDec a]) ; {-# INLINE define #-}
instance {-# OVERLAPPABLE #-} (IsDec a, Monad m) => MonadTH [a]         (THBuilderT m) where define   = mapM_ define                       ; {-# INLINE define #-}
instance {-# OVERLAPPABLE #-} (IsDec a, Monad m) => MonadTH (ZipList a) (THBuilderT m) where define   = define . getZipList                ; {-# INLINE define #-}


-- === Execution === --

execTHBuilder :: Monad m => THBuilderT m a -> m [Dec]
execTHBuilder = flip execStateT mempty . unwrap' ; {-# INLINE execTHBuilder #-}

build :: Monad m => THBuilderT m a -> m [TH.Dec]
build = fmap th . execTHBuilder ; {-# INLINE build #-}




makeLunaComponents :: String -> String -> [String] -> Q [TH.Dec]
makeLunaComponents (typeName -> comp) (typeName -> fam) (ZipList -> typeNames) = build $ do
    let types  = typeName <$> typeNames
        idents = toUpper  <$> types

    -- define $ phantom  comp                                     -- data Atom
    -- define $ phantom' fam                                      -- data Atomic a
    define $ data' <$> idents                                  -- data STAR; ...
    define $ alias <$> types <*> (app fam <$> idents)          -- type Star = Atomic STAR; ...
    define $ typeInstance' "TypeRepr" <$> idents <*> typeNames -- type instance TypeRepr STAR = "Star"; ...
    define $ typeInstance' "Every" comp types                  -- type instance Every Atom = '[Star, ...]




makeLensedTerm :: Name -> Q [TH.Dec]
makeLensedTerm name = (<>) <$> makeLenses name <*> makeTerm   name

makeLensedTerms :: String -> [Name] -> Q [TH.Dec]
makeLensedTerms famname names = (<>) <$> (concat <$> mapM makeLenses names) <*> makeTerms famname names

-- | makeTerm used as `makeTerm TermFoo` generates:
--     data FOO
--     type Foo = TermicType FOO
--     type instance TermDef  Foo = TermFoo
--     type instance TypeRepr FOO = "Foo"
--     type instance Access TermType (TermFoo a) = Foo
--     uncheckedFoo <TermFoo args> = uncheckedFromTermDef (Foo <TermFoo args>)
makeTerm :: Name -> Q [TH.Dec]
makeTerm termName = build $ do
    let pfxname = nameBase termName
    strName <- maybe (error "Provided type name must start with 'Term'") return (splitTermName pfxname)
    let name        = typeName strName
        boldName    = toUpper name
        termWrapper = typeName "TermicType"
        termDef     = typeName "TermDef"

    define $ data' boldName
    define $ alias name $ app termWrapper boldName
    define $ typeInstance' termDef name (typeName pfxname)
    define $ typeInstance' "TypeRepr" boldName strName
    define $ typeInstance  "Access" ([th $ typeName "TermType", th $ app (th $ typeName pfxname :: Type) (th $ varName "a")] :: [Type]) name

    info <- lift $ reify termName
    let uname       = varName $ "unchecked" <> strName
        argnum      = tvarNum info
        args        = ("arg" <>) . show <$> [1..argnum]
    let app1 = th $ (apps (th name) (th . varName <$> args) :: App TH.Exp) :: TH.Exp
        app2 = th $ app (th $ varName "uncheckedFromTermDef") app1
    define $ function' uname $ Clause (VarP . mkName <$> args) (NormalB app2) []
    return []

tvarNum = \case
    TyConI (NewtypeD {})                    -> 1
    TyConI (DataD _ _ _ _ [NormalC _ ts] _) -> length ts
    TyConI (DataD _ _ _ _ [RecC    _ ts] _) -> length ts


splitTermName :: String -> Maybe String
splitTermName n = if_ (head == term) $ Just tail where
    head = take len n
    tail = drop len n
    len  = length term
    term = "Term"

makeTerms :: String -> [Name] -> Q [TH.Dec]
makeTerms famName pns = (<>) <$> (concat <$> mapM makeTerm pns) <*> (build $ do
    let extractName n = maybe (error "Provided type name must start with 'Term'") return (splitTermName $ nameBase n)
    ns <- mapM extractName pns
    define $ alias (typeName famName) (typeName <$> ns)
    )


-- makeLensedTerms

-- type instance Access TermType        (Term atom _     ) = atom


-- FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME [WD]
-- problem z ta biblioteka jest taki, ze apps lub typeInstance wymagaja takiego samego typu w liscie,
-- a czesto ccemy przekazywac tam raz name raz co innego. Wiec to wyjscia powinny byc polimorficzne, nie wejscia!
