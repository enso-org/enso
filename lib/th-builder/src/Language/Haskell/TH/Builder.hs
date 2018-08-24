{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Haskell.TH.Builder (module Language.Haskell.TH.Builder, module X) where

import Prologue hiding (Cons, Data, Type, cons, inline)

import           Control.Lens (lens, makePrisms, _3)
import qualified Data.Char    as Char

import           Data.List.Split            (splitOn)
import           Language.Haskell.TH        as X (Dec, Name, Q, newName, reify,
                                                  runIO)
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH



---------------------------
-- === Common Fields === --
---------------------------

class HasExpr t where
    expr :: forall a. Lens' (t a) a


class HasName a where
    type family   NameOf a
    type instance NameOf a = Name
    name :: Lens' a (NameOf a)

class MayHaveName a where
    maybeName :: Lens' a (Maybe Name)

class HasParams a where
    type family ParamOf a
    params :: Lens' a [ParamOf a]

class HasKind a where
    type family   KindOf a
    type instance KindOf a = TH.Kind
    kind :: Lens' a (KindOf a)

class HasCtx    a where ctx    :: Lens' a [TH.Pred]
class HasDerivs a where derivs :: Lens' a [TH.DerivClause]



---------------------
-- === TH Cons === --
---------------------

class HasCons a where
    consList :: Lens' a [TH.Con]

instance HasCons TH.Dec where
    consList = lens getter (flip setter) where
        getter = \case
            TH.DataD        _ _ _ _ c _ -> c
            TH.NewtypeD     _ _ _ _ c _ -> [c]
            TH.DataInstD    _ _ _ _ c _ -> c
            TH.NewtypeInstD _ _ _ _ c _ -> [c]
            _                           -> []
        setter x = \case
            TH.DataD        t1 t2 t3 t4 _ t6 -> TH.DataD        t1 t2 t3 t4 x t6
            TH.NewtypeD     t1 t2 t3 t4 _ t6 -> TH.NewtypeD     t1 t2 t3 t4 (unsafeHead x) t6
            TH.DataInstD    t1 t2 t3 t4 _ t6 -> TH.DataInstD    t1 t2 t3 t4 x t6
            TH.NewtypeInstD t1 t2 t3 t4 _ t6 -> TH.NewtypeInstD t1 t2 t3 t4 (unsafeHead x) t6
            a                                -> a


class HasNamedFields a where
    namedFields :: Lens' a [TH.VarBangType]

instance HasNamedFields TH.Con where
    namedFields = lens getter (flip setter) where
        getter = \case
            TH.RecC     _ n   -> n
            TH.RecGadtC _ n _ -> n
            _                 -> []
        setter x = \case
            TH.RecC     t1 _    -> TH.RecC     t1 x
            TH.RecGadtC t1 _ t3 -> TH.RecGadtC t1 x t3
            a                   -> a

class HasDerivClauses a where
    derivClauses :: Lens' a [TH.DerivClause]

instance HasDerivClauses TH.Dec where
    derivClauses = lens getter (flip setter) where
        getter = \case
            TH.DataD        _ _ _ _ _ x -> x
            TH.NewtypeD     _ _ _ _ _ x -> x
            TH.DataInstD    _ _ _ _ _ x -> x
            TH.NewtypeInstD _ _ _ _ _ x -> x
            _                           -> []

        setter x = \case
            TH.DataD        t1 t2 t3 t4 t5 _ -> TH.DataD        t1 t2 t3 t4 t5 x
            TH.NewtypeD     t1 t2 t3 t4 t5 _ -> TH.NewtypeD     t1 t2 t3 t4 t5 x
            TH.DataInstD    t1 t2 t3 t4 t5 _ -> TH.DataInstD    t1 t2 t3 t4 t5 x
            TH.NewtypeInstD t1 t2 t3 t4 t5 _ -> TH.NewtypeInstD t1 t2 t3 t4 t5 x
            a                                -> a



-------------------
-- === Lists === --
-------------------

type instance Item TH.Type = TH.Type
instance Convertible [TH.Type] TH.Type where
    convert = foldl' (flip $ TH.AppT . TH.AppT TH.PromotedConsT) TH.PromotedNilT


-------------------
-- === Names === --
-------------------

-- === Generation === --

strNameCycle :: [String]
strNameCycle = (return <$> ['a' .. 'z']) <> strNameCycle' [] (show <$> [0..]) where
    strNameCycle' []     (n:ns) = strNameCycle' ['a' .. 'z'] ns
    strNameCycle' (b:bs) ns     = (b : unsafeHead ns) : strNameCycle' bs ns

unsafeNameCycle  :: Convertible' String a => [a]
unsafeGenName    :: Convertible' String a => a
unsafeGenNames   :: Convertible' String a => Int -> [a]
unsafeGenNamesTN :: Convertible' String a => Int -> [a]
unsafeNameCycle    = convert' <$> strNameCycle
unsafeGenName      = unsafeHead unsafeNameCycle
unsafeGenNames     = flip take unsafeNameCycle
unsafeGenNamesTN i = convert' . ("t" <>) . show <$> [1..i]

newNames :: Convertible' Name a => Int -> Q [a]
newNames = mapM (fmap convert' . newName) . flip take strNameCycle

mapName :: (String -> String) -> Name -> Name
mapName f n = TH.mkName $ (fromMaybe "" $ TH.nameModule n) <> (f $ TH.nameBase n)

toUpper :: Name -> Name
toUpper = mapName (map Char.toUpper)


-- === Utils === --

lowerCase :: BiConvertible' String s => s -> s
lowerCase s = convert' $ case convertTo' @String s of
    []     -> []
    (a:as) -> Char.toLower a : as


-- === TH Conversions === --

instance Convertible Name   TH.TyVarBndr where convert = TH.PlainTV
instance Convertible String Name         where convert = TH.mkName
instance Convertible String TH.TyVarBndr where convert = TH.PlainTV . convert
instance Convertible Name   String       where convert = TH.nameBase

instance IsString Name         where fromString = convert
instance IsString TH.TyVarBndr where fromString = convert
instance Semigroup Name where
    n <> n' = convert (convert n <> convert n' :: String)


-- === Name getters === --

instance MayHaveName TH.Dec where
    maybeName = lens getter (flip setter) where
        getter = \case
            TH.FunD         n _         -> Just n
            TH.DataD        _ n _ _ _ _ -> Just n
            TH.NewtypeD     _ n _ _ _ _ -> Just n
            TH.TySynD       n _ _       -> Just n
            TH.ClassD       _ n _ _ _   -> Just n
            TH.SigD         n _         -> Just n
            TH.InfixD       _ n         -> Just n
            TH.DataFamilyD  n _ _       -> Just n
            TH.DataInstD    _ n _ _ _ _ -> Just n
            TH.NewtypeInstD _ n _ _ _ _ -> Just n
            TH.TySynInstD   n _         -> Just n
            TH.RoleAnnotD   n _         -> Just n
            TH.DefaultSigD  n _         -> Just n
            TH.PatSynD      n _ _ _     -> Just n
            TH.PatSynSigD   n _         -> Just n
            _                           -> Nothing
        setter Nothing  = id
        setter (Just x) = \case
            TH.FunD         _  t2             -> TH.FunD         x  t2
            TH.DataD        t1 _  t3 t4 t5 t6 -> TH.DataD        t1 x  t3 t4 t5 t6
            TH.NewtypeD     t1 _  t3 t4 t5 t6 -> TH.NewtypeD     t1 x  t3 t4 t5 t6
            TH.TySynD       _  t2 t3          -> TH.TySynD       x  t2 t3
            TH.ClassD       t1 _  t3 t4 t5    -> TH.ClassD       t1 x  t3 t4 t5
            TH.SigD         _  t2             -> TH.SigD         x  t2
            TH.InfixD       t1 _              -> TH.InfixD       t1 x
            TH.DataFamilyD  _  t2 t3          -> TH.DataFamilyD  x  t2 t3
            TH.DataInstD    t1 _  t3 t4 t5 t6 -> TH.DataInstD    t1 x  t3 t4 t5 t6
            TH.NewtypeInstD t1 _  t3 t4 t5 t6 -> TH.NewtypeInstD t1 x  t3 t4 t5 t6
            TH.TySynInstD   _  t2             -> TH.TySynInstD   x  t2
            TH.RoleAnnotD   _  t2             -> TH.RoleAnnotD   x  t2
            TH.DefaultSigD  _  t2             -> TH.DefaultSigD  x  t2
            TH.PatSynD      _  t2 t3 t4       -> TH.PatSynD      x  t2 t3 t4
            TH.PatSynSigD   _  t2             -> TH.PatSynSigD   x  t2
            a                                 -> a

instance MayHaveName TH.Con where
    maybeName = lens getter (flip setter) where
        getter = \case
            TH.NormalC n _   -> Just n
            TH.RecC    n _   -> Just n
            TH.InfixC  _ n _ -> Just n
            TH.ForallC _ _ a -> a ^. maybeName
            _                -> Nothing
        setter Nothing = id
        setter (Just n) = \case
            TH.NormalC _ t1    -> TH.NormalC n t1
            TH.RecC    _ t1    -> TH.RecC    n t1
            TH.InfixC  t1 _ t2 -> TH.InfixC  t1 n t2
            TH.ForallC t1 t2 a -> TH.ForallC t1 t2 $ setter (Just n) a
            t                  -> t


----------------------
-- === Literals === --
----------------------

instance Convertible Integer TH.Exp  where convert = TH.LitE . TH.IntegerL
instance Convertible Integer TH.Type where convert = TH.LitT . TH.NumTyLit
instance Convertible Int     TH.Exp  where convert = convertVia @Integer
instance Convertible Int     TH.Type where convert = convertVia @Integer



-----------------
-- === Var === --
-----------------

-- === Definition === --

newtype Var = Var { __name :: Name }
makeLenses ''Var


-- === Utils === --

var :: Convertible Var a => Name -> a
var = convert . Var


-- === Conversions === --

instance Convertible Name Var   where convert = wrap
instance Convertible Var  Name  where convert = unwrap


-- === TH Conversions === --

instance Convertible Var TH.Pat       where convert = TH.VarP    . convert
instance Convertible Var TH.Exp       where convert = TH.VarE    . convert
instance Convertible Var TH.Type      where convert = TH.VarT    . convert
instance Convertible Var TH.TyVarBndr where convert = TH.PlainTV . convert

instance Convertible String Var where convert = Var . convert

instance IsString TH.Exp  where fromString = convertVia @Var
instance IsString TH.Pat  where fromString = convertVia @Var
instance IsString TH.Type where fromString = convertVia @Var


-----------------
-- === App === --
-----------------

-- === Definition === --

data App a = App
    { __base :: a
    , __arg  :: a
    } deriving (Foldable, Functor, Show, Traversable)


-- === Utils === --

app  :: Convertible (App a) a => a -> a -> a
app2 :: Convertible (App a) a => a -> a -> a -> a
app3 :: Convertible (App a) a => a -> a -> a -> a -> a
app4 :: Convertible (App a) a => a -> a -> a -> a -> a -> a
app5 :: Convertible (App a) a => a -> a -> a -> a -> a -> a -> a
app                   = convert .: App
app2 a t1 t2          = apps a [t1, t2]
app3 a t1 t2 t3       = apps a [t1, t2, t3]
app4 a t1 t2 t3 t4    = apps a [t1, t2, t3, t4]
app5 a t1 t2 t3 t4 t5 = apps a [t1, t2, t3, t4, t5]

apps :: Convertible (App a) a => a -> [a] -> a
apps = foldl' app

appQ :: Convertible (App a) a => Q a -> Q a -> Q a
appQ base arg = app <$> base <*> arg


-- === TH Conversions === --

instance a ~ TH.Exp => Convertible (App a) TH.Exp where
    convert (App a b) = TH.AppE a b

instance a ~ TH.Type => Convertible (App a) TH.Type where
    convert (App a b) = TH.AppT a b



-------------------
-- === Typed === --
-------------------

-- === Definition === --

data Typed e t = Typed
    { __expr :: e
    , __tp   :: t
    } deriving (Foldable, Functor, Show, Traversable)


-- === Utils === --

typed, (-::) :: Convertible (Typed e t) e => e -> t -> e
typed = convert .: Typed
(-::) = typed


-- === TH Conversions === --

instance (e ~ TH.Exp, t ~ TH.Type) => Convertible (Typed e t) TH.Exp where
    convert (Typed e t) = TH.SigE e t



------------------
-- === Cons === --
------------------

-- === Definition === --

data Field a = Field
    { __name :: Maybe Name
    , __expr :: a
    } deriving (Foldable, Functor, Show, Traversable)
makeLenses ''Field

data Cons a = Cons
    { __name   :: Name
    , __fields :: [Field a]
    } deriving (Foldable, Functor, Show, Traversable)
makeLenses ''Cons


-- === Utils === --

cons :: Convertible (Cons a) a => Name -> [Field a] -> a
cons = convert .: Cons

cons' :: Convertible (Cons a) a => Name -> a
cons' n = cons n []

field' :: a -> Field a
field' = Field Nothing


-- === Properties === --

instance HasExpr Field     where expr = field_expr
instance HasName (Field a) where
    type NameOf  (Field a) = Maybe Name
    name = field_name


-- === Conversions === --

instance Convertible Var a => Convertible Var (Field a) where
    convert = Field Nothing . convert


-- === TH conversions === --

instance a ~ TH.Pat => Convertible (Cons a) TH.Pat where
    convert (Cons n fs) = if not usesNames then conP else error "TODO" where
        usesNames = not . null $ catMaybes (view name <$> fs)
        conP      = TH.ConP n $ view expr <$> fs

instance a ~ TH.Type => Convertible (Cons a) TH.Type where
    convert (Cons n fs) = foldl' TH.AppT (TH.ConT n) (view expr <$> fs)

instance a ~ TH.Exp => Convertible (Cons a) TH.Exp where
    convert (Cons n fs) = foldl' TH.AppE (TH.ConE n) (view expr <$> fs)



--------------------
-- === Clause === --
--------------------

-- === Definition === --

data Clause = Clause
    { __pats       :: [TH.Pat]
    , __body       :: TH.Exp
    , __whereDecls :: [TH.Dec]
    }


-- === Utils === --

clause :: Convertible Clause t => [TH.Pat] -> TH.Exp -> [TH.Dec] -> t
clause = convert .:. Clause


-- === TH Conversions === --

instance Convertible Clause TH.Clause where
    convert (Clause ps b w) = TH.Clause ps (TH.NormalB b) w



------------------
-- === Data === --
------------------

data Data = Data
    { __ctx    :: [TH.Pred]
    , __name   :: Name
    , __params :: [TH.TyVarBndr]
    , __kind   :: Maybe TH.Kind
    , __cons   :: [TH.Con]
    , __derivs :: [TH.DerivClause]
    }
makeLenses ''Data

instance HasName   Data where name     = data_name
instance HasCtx    Data where ctx      = data_ctx
instance HasCons   Data where consList = data_cons
instance HasDerivs Data where derivs   = data_derivs

instance HasKind Data where
    type KindOf  Data = Maybe TH.Kind
    kind = data_kind

instance HasParams Data where
    type ParamOf   Data = TH.TyVarBndr
    params = data_params


-- === Construction === --

data'' :: Name -> Data
data'' n = Data def n def def def def

data' :: Convertible Data t => Name -> t
data' = convert . data''

-- | Function 'phantom' takes number of data parameters and generates a phantom
--   data type, for example `phantom 2 "Foo"` generates `data Foo a b`
phantomN :: Int -> Name -> Data
phantomN i n = data'' n & params .~ unsafeGenNames i

phantom0, phantom1, phantom2, phantom3, phantom4, phantom5 :: Name -> Data
phantom0 = phantomN 0
phantom1 = phantomN 1
phantom2 = phantomN 2
phantom3 = phantomN 3
phantom4 = phantomN 4
phantom5 = phantomN 5

-- === TH Conversions === --

instance Convertible Data TH.Dec where
    convert (Data ctx name params kind cons derivs) = TH.DataD ctx name params kind cons derivs



---------------------
-- === TypeSyn === --
---------------------

data TypeSyn = TypeSyn
    { __name   :: Name
    , __params :: [TH.TyVarBndr]
    , __tp     :: TH.Type
    }
makeLenses ''TypeSyn

instance HasName   TypeSyn where name = typeSyn_name; {-# INLINE name #-}
instance HasParams TypeSyn where
    type ParamOf TypeSyn = TH.TyVarBndr
    params = typeSyn_params ; {-# INLINE params #-}


-- === Construction === --

alias :: Convertible TypeSyn t => Name -> TH.Type -> t
alias n t = convert $ TypeSyn n def t ; {-# INLINE alias #-}


-- === TH Convertsion === --

instance Convertible TypeSyn TH.Dec where
    convert (TypeSyn name params typ) = TH.TySynD name params typ



---------------------------
-- === Type Instance === --
---------------------------

data TypeInstance = TypeInstance
    { __name   :: Name
    , __args   :: [TH.Type]
    , __result :: TH.Type
    }


typeInstance :: Convertible TypeInstance t => Name -> [TH.Type] -> TH.Type -> t
typeInstance = convert .:. TypeInstance
{-# INLINE typeInstance #-}

typeInstance1 :: Convertible TypeInstance t => Name -> TH.Type -> TH.Type -> t
typeInstance1 n t = typeInstance n [t]
{-# INLINE typeInstance1 #-}

typeInstance2 :: Convertible TypeInstance t => Name -> TH.Type -> TH.Type -> TH.Type -> t
typeInstance2 n t1 t2 = typeInstance n [t1, t2]
{-# INLINE typeInstance2 #-}

instance Convertible TypeInstance TH.Dec where
    convert (TypeInstance n as r) = TH.TySynInstD n (TH.TySynEqn as r) ; {-# INLINE convert #-}



-------------------
-- === Tuple === --
-------------------

-- === Definition === --

data Tuple a = Tuple Int [a] deriving (Show, Foldable, Functor, Traversable)

tuple :: Convertible (Tuple a) a => [a] -> a
tuple els = tupleCons (length els) els

tupleCons :: Convertible (Tuple a) a => Int -> [a] -> a
tupleCons = convert .: Tuple


-- === Instances === --

instance a ~ TH.Type => Convertible (Tuple a) TH.Type where convert (Tuple len els) = if len == 1 then apps (cons' "OneTuple") els else apps (TH.TupleT len) els
instance a ~ TH.Pat  => Convertible (Tuple a) TH.Pat  where convert (Tuple len els) = if len == 1 then cons "OneTuple" (field' <$> els) else TH.TupP els
instance a ~ TH.Exp  => Convertible (Tuple a) TH.Exp  where convert (Tuple len els) = if len == 1 then cons "OneTuple" (field' <$> els) else TH.TupE els


-- FIXME[WD->PM]: TypeInfo does not make a sense. This type consist of some
--                related data useful for a specific usage only
data TypeInfo = TypeInfo
    { __name   :: Name
    , __tyVars :: [TH.Type]
    , __conss  :: [TH.Con]
    }
makeLenses ''TypeInfo

instance HasName TypeInfo where name = typeInfo_name

-- FIXME[WD->PM]: IRREFUTABLE PATTERN!
reifyTypeInfo :: Name -> TH.Q TypeInfo
reifyTypeInfo ty = do
    TH.TyConI tyCon <- TH.reify ty
    return $ getTypeInfo tyCon

getTypeInfo :: Dec -> TypeInfo
getTypeInfo = uncurry TypeInfo . \case
    TH.DataD        _ nm vars _ cs _ -> (nm, tv2t <$> vars, cs)
    TH.NewtypeD     _ nm vars _ c  _ -> (nm, tv2t <$> vars, [c])
    TH.DataInstD    _ nm vars _ cs _ -> (nm, vars, cs)
    TH.NewtypeInstD _ nm vars _ c  _ -> (nm, vars, [c])
    a -> error $ "Type info: not supported: " <> show a
    where tv2t = \case
              TH.PlainTV  n   -> var n
              TH.KindedTV n _ -> var n




----------------------------
-- === Class Instance === --
----------------------------

data ClassInstance = ClassInstance
    { __overlap :: Maybe TH.Overlap
    , __ctx     :: TH.Cxt
    , __name    :: Name
    , __tpname  :: Name
    , __params  :: [TH.Type]
    , __decs    :: [TH.Dec]
    }

classInstance :: (Convertible ClassInstance a)
              => Name -> Name -> [TH.Type] -> [TH.Dec] -> a
classInstance n tn ts decs = convert $ ClassInstance Nothing mempty n tn ts decs
{-# INLINE classInstance #-}

classInstanceWithCtx :: (Convertible ClassInstance a)
              => TH.Cxt -> Name -> Name -> [TH.Type] -> [TH.Dec] -> a
classInstanceWithCtx ctx n tn ts decs = convert $ ClassInstance Nothing ctx n tn ts decs
{-# INLINE classInstanceWithCtx #-}

instance Convertible ClassInstance TH.Dec where
    convert (ClassInstance olap cxt n tn ts decs) =
        TH.InstanceD olap cxt instanceT decs
            where instanceT = TH.AppT (TH.ConT n) (foldl' TH.AppT (TH.ConT tn) ts)
    {-# INLINE convert #-}

------------------------
-- === Misc utils === --
------------------------

conNameTypes :: TH.Con -> (Name, [TH.Type])
conNameTypes = \case
    TH.NormalC n fs  -> (n, snd <$> fs)
    TH.RecC    n fs  -> (n, view _3 <$> fs)
    TH.InfixC a n b  -> (n, snd <$> [a, b])
    TH.ForallC _ _ c -> conNameTypes c
    _ -> error "***error*** deriveStorable: GADT constructors not supported"

getBangTypes :: TH.Con -> [TH.BangType]
getBangTypes = \case
    TH.NormalC  _ bt      -> bt
    TH.RecC     _ vbt     -> vbt2bt <$> vbt
    TH.InfixC   bt1 _ bt2 -> [bt1, bt2]
    TH.ForallC  _ _ c     -> getBangTypes c
    TH.RecGadtC _ vbt _   -> vbt2bt <$> vbt
    where vbt2bt (_,b,t) = (b,t)

-- | Extract the name and number of params from the consturctor
conNameArity :: TH.Con -> (Name, Int)
conNameArity c = let (n, fs) = conNameTypes c in (n, fromIntegral $ length fs)

-- | Extract the number of params from the constructor
conArity :: TH.Con -> Int
conArity = snd . conNameArity

conName :: TH.Con -> Name
conName = fst . conNameArity

noArgCon :: TH.Con -> Bool
noArgCon = (== 0) . conArity

inline :: TH.RuleMatch -> Name -> TH.Dec
inline rule n = TH.PragmaD $ TH.InlineP n TH.Inline rule TH.AllPhases

inlineF :: Name -> TH.Dec
inlineF = inline TH.FunLike

inlineC :: Name -> TH.Dec
inlineC = inline TH.ConLike



-------------------
-- === UTILS === --
-------------------

traverseType :: Applicative m => (TH.Type -> m TH.Type) -> TH.Type -> m TH.Type
traverseType f = \case
    TH.ForallT tvs cxt t -> TH.ForallT tvs cxt <$> f t
    TH.AppT    t t2      -> TH.AppT    <$> f t <*> f t2
    TH.SigT    t k       -> TH.SigT    <$> f t <*> pure k
    TH.InfixT  t n t2    -> TH.InfixT  <$> f t <*> pure n <*> f t2
    TH.UInfixT t n t2    -> TH.UInfixT <$> f t <*> pure n <*> f t2
    TH.ParensT t         -> TH.ParensT <$> f t
    a                    -> pure a

-- GHC BUG: https://ghc.haskell.org/trac/ghc/ticket/14848
fixDuplicateRecordNamesGHCBug :: String -> String
fixDuplicateRecordNamesGHCBug s = case splitOn ":" s of
    []  -> error "impossible"
    [a] -> a
    as  -> unsafeLast $ unsafeInit as

op :: Name -> TH.Exp -> TH.Exp -> TH.Exp
op = app2 . var

plus, mul :: TH.Exp -> TH.Exp -> TH.Exp
plus = op '(+)
mul  = op '(*)

intLit :: Integer -> TH.Exp
intLit = TH.LitE . TH.IntegerL

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- TODO: We keep old code for now in case anything will be needed.
--       It should be removed before releasing final version.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--
--
--
-- ----------------------
-- -- === Function === --
-- ----------------------
--
-- data Function = Function { _function_name   :: VarName
--                          , _function_clause :: [Clause]
--                          }
--
--
-- function :: ToVarName n => n -> [Clause] -> Function
-- function n = Function (toVarName n) ; {-# INLINE function #-}
--
-- function' :: ToVarName n => n -> Clause -> Function
-- function' n = function n . return ; {-# INLINE function' #-}
--
--
-- instance IsTH TH.Dec Function where
--     th (Function n cs) = FunD (th n) cs ; {-# INLINE th #-}
--
--
--
-- -----------------
-- -- === Dec === --
-- -----------------
--
-- data Dec = DataDec          { _dec_dataDec          :: Data          }
--          | TypeSynDec       { _dec_typeSynDec       :: TypeSyn       }
--          | TypeInstanceDec  { _dec_typeInstanceDec  :: TypeInstance  }
--          | ClassInstanceDec { _dec_classInstanceDec :: ClassInstance }
--          | FunctionDec      { _dec_functionDec      :: Function      }
--
--
-- class IsDec a where
--     toDec :: a -> Dec
--
--
-- instance IsTH TH.Dec Dec where
--     th = \case
--         DataDec          a -> th a
--         TypeSynDec       a -> th a
--         TypeInstanceDec  a -> th a
--         ClassInstanceDec a -> th a
--         FunctionDec      a -> th a
--     {-# INLINE th #-}
--
-- instance IsDec Data          where toDec = DataDec          ; {-# INLINE toDec #-}
-- instance IsDec TypeSyn       where toDec = TypeSynDec       ; {-# INLINE toDec #-}
-- instance IsDec TypeInstance  where toDec = TypeInstanceDec  ; {-# INLINE toDec #-}
-- instance IsDec ClassInstance where toDec = ClassInstanceDec ; {-# INLINE toDec #-}
-- instance IsDec Function      where toDec = FunctionDec      ; {-# INLINE toDec #-}
--
--
-- makeLenses ''Dec
--
--
--
-- -----------------------
-- -- === THBuilder === --
-- -----------------------
--
-- type    THBuilder      = THBuilderT Q
-- newtype THBuilderT m a = THBuilderT (StateT [Dec] m a) deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix, MonadFail)
-- makeWrapped ''THBuilderT
--
-- class Monad m => MonadTH a m where
--     define :: a -> m ()
--
-- instance {-# OVERLAPPABLE #-} (IsDec a, Monad m) => MonadTH  a          (THBuilderT m) where define a = THBuilderT $ modify (<> [toDec a]) ; {-# INLINE define #-}
-- instance {-# OVERLAPPABLE #-} (IsDec a, Monad m) => MonadTH [a]         (THBuilderT m) where define   = mapM_ define                       ; {-# INLINE define #-}
-- instance {-# OVERLAPPABLE #-} (IsDec a, Monad m) => MonadTH (ZipList a) (THBuilderT m) where define   = define . getZipList                ; {-# INLINE define #-}
--
--
-- -- === Execution === --
--
-- execTHBuilder :: Monad m => THBuilderT m a -> m [Dec]
-- execTHBuilder = flip execStateT mempty . unwrap' ; {-# INLINE execTHBuilder #-}
--
-- build :: Monad m => THBuilderT m a -> m [TH.Dec]
-- build = fmap th . execTHBuilder ; {-# INLINE build #-}
--
--
--
--
-- makeLunaComponents :: String -> String -> [String] -> Q [TH.Dec]
-- makeLunaComponents (typeName -> comp) (typeName -> fam) (ZipList -> typeNames) = build $ do
--     let types  = typeName <$> typeNames
--         idents = toUpper  <$> types
--
--     -- define $ phantom  comp                                     -- data Atom
--     -- define $ phantom' fam                                      -- data Atomic a
--     define $ data' <$> idents                                  -- data STAR; ...
--     define $ alias <$> types <*> (app fam <$> idents)          -- type Star = Atomic STAR; ...
--     define $ typeInstance' "TypeRepr" <$> idents <*> typeNames -- type instance TypeRepr STAR = "Star"; ...
--     define $ typeInstance' "Every" comp types                  -- type instance Every Atom = '[Star, ...]
--
--
--
--
-- makeLensedTerm :: Name -> Q [TH.Dec]
-- makeLensedTerm name = (<>) <$> makeLenses name <*> makeTerm   name
--
-- makeLensedTerms :: String -> [Name] -> Q [TH.Dec]
-- makeLensedTerms famname names = (<>) <$> (concat <$> mapM makeLenses names) <*> makeTerms famname names
--
-- -- | makeTerm used as `makeTerm TermFoo` generates:
-- --     data FOO
-- --     type Foo = TermicType FOO
-- --     type instance TermDef  Foo = TermFoo
-- --     type instance TypeRepr FOO = "Foo"
-- --     type instance Access TermType (TermFoo a) = Foo
-- --     uncheckedFoo <TermFoo args> = uncheckedFromTermDef (Foo <TermFoo args>)
-- makeTerm :: Name -> Q [TH.Dec]
-- makeTerm termName = build $ do
--     let pfxname = nameBase termName
--     strName <- maybe (error "Provided type name must start with 'Term'") return (splitTermName pfxname)
--     let name        = typeName strName
--         boldName    = toUpper name
--         termWrapper = typeName "TermicType"
--         termDef     = typeName "TermDef"
--
--     define $ data' boldName
--     define $ alias name $ app termWrapper boldName
--     define $ typeInstance' termDef name (typeName pfxname)
--     define $ typeInstance' "TypeRepr" boldName strName
--     define $ typeInstance  "Access" ([th $ typeName "TermType", th $ app (th $ typeName pfxname :: Type) (th $ varName "a")] :: [Type]) name
--
--     info <- lift $ reify termName
--     let uname       = varName $ "unchecked" <> strName
--         argnum      = tvarNum info
--         args        = ("arg" <>) . show <$> [1..argnum]
--     let app1 = th $ (apps (th name) (th . varName <$> args) :: App TH.Exp) :: TH.Exp
--         app2 = th $ app (th $ varName "uncheckedFromTermDef") app1
--     define $ function' uname $ Clause (VarP . mkName <$> args) (NormalB app2) []
--     return []
--
-- tvarNum = \case
--     TyConI (NewtypeD {})                    -> 1
--     TyConI (DataD _ _ _ _ [NormalC _ ts] _) -> length ts
--     TyConI (DataD _ _ _ _ [RecC    _ ts] _) -> length ts
--
--
-- splitTermName :: String -> Maybe String
-- splitTermName n = if_ (head == term) $ Just tail where
--     head = take len n
--     tail = drop len n
--     len  = length term
--     term = "Term"
--
-- makeTerms :: String -> [Name] -> Q [TH.Dec]
-- makeTerms famName pns = (<>) <$> (concat <$> mapM makeTerm pns) <*> (build $ do
--     let extractName n = maybe (error "Provided type name must start with 'Term'") return (splitTermName $ nameBase n)
--     ns <- mapM extractName pns
--     define $ alias (typeName famName) (typeName <$> ns)
--     )
