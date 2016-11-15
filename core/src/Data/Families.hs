{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Families where

import Prelude
import Data.Default
import Data.Functor.Utils hiding ((.))
import Data.Monoid
import Data.String
import Control.Monad.State
import Control.Lens.Utils hiding (cons)

import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH hiding (Con, Dec)




class HasName    a where name    :: Lens' a (NameOf a)
class HasCtx     a where ctx     :: Lens' a [Pred]
class HasKind    a where kind    :: Lens' a (Maybe Kind)
class HasDerivs  a where derivs  :: Lens' a [Pred]


class IsTH t a where
    th :: a -> t

instance IsTH t a => IsTH (Q t) a where
    th = return . th ; {-# INLINE th #-}

instance {-# OVERLAPPABLE #-} IsTH t a => IsTH [t] a where
    th = return . th ; {-# INLINE th #-}

instance {-# OVERLAPPABLE #-} IsTH t a => IsTH [t] [a] where
    th = fmap th ; {-# INLINE th #-}



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

mkVarName :: String -> VarName
mkVarName = fromString ; {-# INLINE mkVarName #-}

mkTypeName :: FromTypeName a => String -> a
mkTypeName = fromTypeName . fromString ; {-# INLINE mkTypeName #-}


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

data App a = App { _app_src :: a
                 , _app_tgt :: a
                 }

makeLenses ''App


class FromApp a where
    fromApp :: App a -> a


instance IsTH TH.Type a => IsTH TH.Type (App a) where
    th (App src tgt) = AppT (th src) (th tgt) ; {-# INLINE th #-}

app :: FromApp a => a -> a -> a
app = fromApp .: App ; {-# INLINE app #-}

-- ------------------
-- -- === Type === --
-- ------------------
--
-- data Type = AppType (App Type)
--
-- instance IsTH TH.Type Type where
--     th = \case
--         AppType a -> th a
--
-- instance FromApp  Type where fromApp = AppType ; {-# INLINE fromApp #-}
-- -- instance FromName Type where


------------------
-- === Args === --
------------------

class HasType a where
    tp :: Lens' a Type

class MayHaveType a where
    checkTp :: Lens' a (Maybe Type)


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
    th a = maybe PlainTV (flip KindedTV) (a ^. checkTp) (a ^. name . wrapped') ; {-# INLINE th #-}


--
-- ------------------
-- -- === Args === --
-- ------------------
--
-- data Arg = Arg { _arg_name :: Name } deriving (Show)
--
-- makeLenses ''Arg
-- instance HasName Arg where name = arg_name ; {-# INLINE name #-}
--
-- class HasArgs a where args :: Lens' a [Arg]



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

instance {-# OVERLAPPING #-} n ~ TypeName => Default (n -> Data) where
    def n = Data def n def def def def ; {-# INLINE def #-}


-- === conversions === --

instance IsTH TH.Dec Data where
    th a = DataD (a ^. ctx) (a ^. name . wrapped') (th $ a ^. params) (a ^. kind) (th $ a ^. cons) (a ^. derivs) ; {-# INLINE th #-}


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

alias :: TypeName -> Type -> TypeSyn
alias = flip TypeSyn def ; {-# INLINE alias #-}


-- === Instances === --

instance IsTH TH.Dec TypeSyn where
    th a = TySynD (a ^. name . wrapped') (th $ a ^. params) (a ^. tp) ; {-# INLINE th #-}


-----------------
-- === Dec === --
-----------------

data Dec = DataDec    { _dec_dataDec    :: Data    }
         | TypeSynDec { _dec_typeSynDec :: TypeSyn }

makeLenses ''Dec

class IsDec a where
    toDec :: a -> Dec


instance IsTH TH.Dec Dec where
    th = \case
        DataDec    a -> th a
        TypeSynDec a -> th a
    {-# INLINE th #-}

instance IsDec Data    where toDec = DataDec    ; {-# INLINE toDec #-}
instance IsDec TypeSyn where toDec = TypeSynDec ; {-# INLINE toDec #-}













-----------------------
-- === THBuilder === --
-----------------------

type    THBuilder      = THBuilderT Q
newtype THBuilderT m a = THBuilderT (StateT [Dec] m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)
makeWrapped ''THBuilderT

class Monad m => MonadTH a m where
    define :: a -> m ()

instance {-# OVERLAPPABLE #-} (IsDec a, Monad m) => MonadTH  a  (THBuilderT m) where define a = THBuilderT $ modify (<> [toDec a]) ; {-# INLINE define #-}
instance {-# OVERLAPPABLE #-} (IsDec a, Monad m) => MonadTH [a] (THBuilderT m) where define   = mapM_ define                       ; {-# INLINE define #-}


-- === Execution === --

execTHBuilder :: Monad m => THBuilderT m a -> m [Dec]
execTHBuilder = flip execStateT mempty . unwrap' ; {-# INLINE execTHBuilder #-}

build :: Monad m => THBuilderT m a -> m [TH.Dec]
build = fmap th . execTHBuilder ; {-# INLINE build #-}







makePhantomFamily fam (fmap mkTypeName -> types) = build $ do
    let famName = (mkTypeName fam :: forall a. FromTypeName a => a)
    define $ data' famName & addParam genName
    define $ alias "foo" (app famName famName)
    -- define $ alias "foo" (AppT (ConT famName) (ConT $ mkName "ooo"))
    define $ data' <$> types

    return ()
    -- let famData   = data' fam & addParam genName
    --     typeDatas = data' <$> types
    -- th $ famData : typeDatas
    -- return [DataD [] n [] Nothing [NormalC (mkName fam) []] []]

--
-- strNameCycle1 = ['a' .. 'z']
-- strNameCycle2 = show <$> [1..]
-- [TySynD A_25 [] (AppT (ConT GHC.Base.Maybe) (ConT GHC.Types.Int))][TySynD A_25 [] (AppT (ConT GHC.Base.Maybe) (ConT GHC.Types.Int))]
