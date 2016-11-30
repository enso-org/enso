{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr.Term.Named (module Luna.IR.Expr.Term.Named, module X) where

import qualified Luna.Prelude as P
import           Luna.Prelude hiding (String, Integer, Rational, Data, product, product')

import Luna.IR.Expr.Term.Class as X
import Luna.IR.Expr.Atom as X (Atom, String, Integer, Rational, Acc, App, Blank, Cons, Lam, Match, Missing, Native, Star, Unify, Var) -- Types only

import Data.Base                 (Base)
import Data.Construction         (Args)
import Luna.IR.Function (Arg)
import qualified Luna.IR.Function.Argument as Arg
import Type.Applicative
import Data.Property
import Data.Phantom
import Luna.IR.Expr.Format
import qualified Luna.IR.Expr.Layout as Layout
import           Luna.IR.Expr.Layout (Layout)

import Data.Construction
import           Data.RTuple (List(Null, (:-:)))
import qualified Data.RTuple as List


type family NameOf a -- FIXME[WD] props?
class HasName a where
    name :: Lens' a (NameOf a)

---------------------
-- === Terms === --
---------------------

-- === Definitions === --

type NamedTerm t n a = Term t (Layout.Named n a)


newtype instance Term Integer  (Layout.Named n a) = Sym_Integer  { _val :: P.Integer                     }
newtype instance Term Rational (Layout.Named n a) = Sym_Rational { _val :: P.Rational                    }
newtype instance Term String   (Layout.Named n a) = Sym_String   { _val :: P.String                      }
data    instance Term Acc      (Layout.Named n a) = Sym_Acc      { _name :: !n      , _base  :: !a       }
data    instance Term App      (Layout.Named n a) = Sym_App      { _base :: a       , _arg   :: !(Arg a) }
data    instance Term Lam      (Layout.Named n a) = Sym_Lam      { _arg  :: !(Arg a), _body  :: !a       }
data    instance Term Match    (Layout.Named n a) = Sym_Match    { _left :: !a      , _right :: !a       }
data    instance Term Unify    (Layout.Named n a) = Sym_Unify    { _left :: !a      , _right :: !a       }
newtype instance Term Cons     (Layout.Named n a) = Sym_Cons     { _name ::  n                           }
data    instance Term Native   (Layout.Named n a) = Sym_Native   { _name :: !n                           }
newtype instance Term Var      (Layout.Named n a) = Sym_Var      { _name ::  n                           }
data    instance Term Blank    (Layout.Named n a) = Sym_Blank
data    instance Term Star     (Layout.Named n a) = Sym_Star
data    instance Term Missing  (Layout.Named n a) = Sym_Missing



type instance NameOf (Term s (Layout.Named n a)) = n
instance HasName (Term Var (Layout.Named n a)) where name = iso (\(Sym_Var n) -> n) Sym_Var




data instance UniTerm (Layout.Named n a) = Integer  P.Integer
                                           | Rational P.Rational
                                           | String   P.String
                                           | Acc      !n       !a
                                           | App      a        !(Arg a)
                                           | Lam      !(Arg a) !a
                                           | Match    !a       !a
                                           | Unify    !a       !a
                                           | Cons      n
                                           | Native   !n
                                           | Var       n
                                           | Blank
                                           | Star
                                           | Missing

instance IsUniTerm Integer  (Layout.Named n a) where uniTerm (Sym_Integer  t1)    = Integer  t1
instance IsUniTerm Rational (Layout.Named n a) where uniTerm (Sym_Rational t1)    = Rational t1
instance IsUniTerm String   (Layout.Named n a) where uniTerm (Sym_String   t1)    = String   t1
instance IsUniTerm Acc      (Layout.Named n a) where uniTerm (Sym_Acc      t1 t2) = Acc      t1 t2
instance IsUniTerm App      (Layout.Named n a) where uniTerm (Sym_App      t1 t2) = App      t1 t2
instance IsUniTerm Lam      (Layout.Named n a) where uniTerm (Sym_Lam      t1 t2) = Lam      t1 t2
instance IsUniTerm Match    (Layout.Named n a) where uniTerm (Sym_Match    t1 t2) = Match    t1 t2
instance IsUniTerm Unify    (Layout.Named n a) where uniTerm (Sym_Unify    t1 t2) = Unify    t1 t2
instance IsUniTerm Cons     (Layout.Named n a) where uniTerm (Sym_Cons     t1)    = Cons     t1
instance IsUniTerm Native   (Layout.Named n a) where uniTerm (Sym_Native   t1)    = Native   t1
instance IsUniTerm Var      (Layout.Named n a) where uniTerm (Sym_Var      t1)    = Var      t1
instance IsUniTerm Blank    (Layout.Named n a) where uniTerm  Sym_Blank           = Blank
instance IsUniTerm Star     (Layout.Named n a) where uniTerm  Sym_Star            = Star
instance IsUniTerm Missing  (Layout.Named n a) where uniTerm  Sym_Missing         = Missing


-- === Instances === --

-- Show

deriving instance (Show n, Show a) => Show (UniTerm (Layout.Named n a))

deriving instance ShowFields (NamedTerm Acc     n a) => Show (NamedTerm Acc     n a)
deriving instance ShowFields (NamedTerm App     n a) => Show (NamedTerm App     n a)
deriving instance ShowFields (NamedTerm Blank   n a) => Show (NamedTerm Blank   n a)
deriving instance ShowFields (NamedTerm Cons    n a) => Show (NamedTerm Cons    n a)
deriving instance ShowFields (NamedTerm Lam     n a) => Show (NamedTerm Lam     n a)
deriving instance ShowFields (NamedTerm Match   n a) => Show (NamedTerm Match   n a)
deriving instance ShowFields (NamedTerm Missing n a) => Show (NamedTerm Missing n a)
deriving instance ShowFields (NamedTerm Native  n a) => Show (NamedTerm Native  n a)
deriving instance ShowFields (NamedTerm Star    n a) => Show (NamedTerm Star    n a)
deriving instance ShowFields (NamedTerm Unify   n a) => Show (NamedTerm Unify   n a)
deriving instance ShowFields (NamedTerm Var     n a) => Show (NamedTerm Var     n a)

-- Args

type instance Fields (NamedTerm Integer  n a) = '[P.Integer]
type instance Fields (NamedTerm Rational n a) = '[P.Rational]
type instance Fields (NamedTerm String   n a) = '[P.String]

type instance Fields (NamedTerm Acc      n a) = '[n, a]
type instance Fields (NamedTerm App      n a) = '[a, Arg a]
type instance Fields (NamedTerm Blank    n a) = '[]
type instance Fields (NamedTerm Cons     n a) = '[n]
type instance Fields (NamedTerm Lam      n a) = '[Arg a, a]
type instance Fields (NamedTerm Match    n a) = '[a, a]
type instance Fields (NamedTerm Missing  n a) = '[]
type instance Fields (NamedTerm Native   n a) = '[n]
type instance Fields (NamedTerm Star     n a) = '[]
type instance Fields (NamedTerm Unify    n a) = '[a, a]
type instance Fields (NamedTerm Var      n a) = '[n]

-- Products

instance Product (NamedTerm Integer  n a) (NamedTerm Integer  n' a') where fields = iso (\(Sym_Integer  t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Integer  t1    ) ; {-# INLINE fields #-}
instance Product (NamedTerm Rational n a) (NamedTerm Rational n' a') where fields = iso (\(Sym_Rational t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Rational t1    ) ; {-# INLINE fields #-}
instance Product (NamedTerm String   n a) (NamedTerm String   n' a') where fields = iso (\(Sym_String   t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_String   t1    ) ; {-# INLINE fields #-}
instance Product (NamedTerm Acc      n a) (NamedTerm Acc      n' a') where fields = iso (\(Sym_Acc      t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_Acc      t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedTerm App      n a) (NamedTerm App      n' a') where fields = iso (\(Sym_App      t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_App      t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedTerm Blank    n a) (NamedTerm Blank    n' a') where fields = iso (\ Sym_Blank           -> Null              ) (\(Null              ) -> Sym_Blank          ) ; {-# INLINE fields #-}
instance Product (NamedTerm Cons     n a) (NamedTerm Cons     n' a') where fields = iso (\(Sym_Cons     t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Cons     t1    ) ; {-# INLINE fields #-}
instance Product (NamedTerm Lam      n a) (NamedTerm Lam      n' a') where fields = iso (\(Sym_Lam      t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_Lam      t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedTerm Match    n a) (NamedTerm Match    n' a') where fields = iso (\(Sym_Match    t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_Match    t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedTerm Missing  n a) (NamedTerm Missing  n' a') where fields = iso (\ Sym_Missing         -> Null              ) (\(Null              ) -> Sym_Missing        ) ; {-# INLINE fields #-}
instance Product (NamedTerm Native   n a) (NamedTerm Native   n' a') where fields = iso (\(Sym_Native   t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Native   t1    ) ; {-# INLINE fields #-}
instance Product (NamedTerm Star     n a) (NamedTerm Star     n' a') where fields = iso (\ Sym_Star            -> Null              ) (\(Null              ) -> Sym_Star           ) ; {-# INLINE fields #-}
instance Product (NamedTerm Unify    n a) (NamedTerm Unify    n' a') where fields = iso (\(Sym_Unify    t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_Unify    t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedTerm Var      n a) (NamedTerm Var      n' a') where fields = iso (\(Sym_Var      t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Var      t1    ) ; {-# INLINE fields #-}

instance Product' (NamedTerm Integer  n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Rational n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm String   n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Acc      n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm App      n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Blank    n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Cons     n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Lam      n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Match    n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Missing  n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Native   n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Star     n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Unify    n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Var      n a) where fields' = fields ; {-# INLINE fields' #-}

-- Field names

instance HasFieldNames (NamedTerm Integer  n a) where fieldNames _ = [ "val"            ]
instance HasFieldNames (NamedTerm Rational n a) where fieldNames _ = [ "val"            ]
instance HasFieldNames (NamedTerm String   n a) where fieldNames _ = [ "val"            ]
instance HasFieldNames (NamedTerm Acc      n a) where fieldNames _ = [ "name" , "base"  ]
instance HasFieldNames (NamedTerm App      n a) where fieldNames _ = [ "base" , "arg"   ]
instance HasFieldNames (NamedTerm Lam      n a) where fieldNames _ = [ "arg"  , "body"  ]
instance HasFieldNames (NamedTerm Match    n a) where fieldNames _ = [ "left" , "right" ]
instance HasFieldNames (NamedTerm Unify    n a) where fieldNames _ = [ "left" , "right" ]
instance HasFieldNames (NamedTerm Cons     n a) where fieldNames _ = [ "name"           ]
instance HasFieldNames (NamedTerm Native   n a) where fieldNames _ = [ "name"           ]
instance HasFieldNames (NamedTerm Var      n a) where fieldNames _ = [ "name"           ]
instance HasFieldNames (NamedTerm Blank    n a) where fieldNames _ = []
instance HasFieldNames (NamedTerm Star     n a) where fieldNames _ = []
instance HasFieldNames (NamedTerm Missing  n a) where fieldNames _ = []

-- FieldList

type instance FieldsType (NamedTerm t a a) = a

instance n ~ a => HasFields (NamedTerm Integer  n a) where fieldList (Sym_Integer  t1   ) = []
instance n ~ a => HasFields (NamedTerm Rational n a) where fieldList (Sym_Rational t1   ) = []
instance n ~ a => HasFields (NamedTerm String   n a) where fieldList (Sym_String   t1   ) = []
instance n ~ a => HasFields (NamedTerm Acc      n a) where fieldList (Sym_Acc      t1 t2) = [t1, t2]
instance n ~ a => HasFields (NamedTerm App      n a) where fieldList (Sym_App      t1 t2) = [t1, t2 ^. Arg._val_]
instance n ~ a => HasFields (NamedTerm Blank    n a) where fieldList (Sym_Blank         ) = []
instance n ~ a => HasFields (NamedTerm Cons     n a) where fieldList (Sym_Cons     t1   ) = [t1]
instance n ~ a => HasFields (NamedTerm Lam      n a) where fieldList (Sym_Lam      t1 t2) = [t1 ^. Arg._val_,t2]
instance n ~ a => HasFields (NamedTerm Match    n a) where fieldList (Sym_Match    t1 t2) = [t1,t2]
instance n ~ a => HasFields (NamedTerm Missing  n a) where fieldList (Sym_Missing       ) = []
instance n ~ a => HasFields (NamedTerm Native   n a) where fieldList (Sym_Native   t1   ) = [t1]
instance n ~ a => HasFields (NamedTerm Star     n a) where fieldList (Sym_Star          ) = []
instance n ~ a => HasFields (NamedTerm Unify    n a) where fieldList (Sym_Unify    t1 t2) = [t1, t2]
instance n ~ a => HasFields (NamedTerm Var      n a) where fieldList (Sym_Var      t1   ) = [t1]


--------------------------
-- === Construction === --
--------------------------

type TermLike          atom s sym = (sym ~ AsTerm s, Product' sym, atom ~ (s # Atom), FromTerm s)
type UncheckedTermLike atom s sym = (sym ~ AsTerm s, Product' sym, atom ~ (s # Atom), UncheckedFromTerm s)


integer' :: (TermLike Integer s sym, Fields sym ~ '[t1]) => t1 -> s
integer' = fromTerm . product' ; {-# INLINE integer' #-}

rational' :: (TermLike Rational s sym, Fields sym ~ '[t1]) => t1 -> s
rational' = fromTerm . product' ; {-# INLINE rational' #-}

string' :: (TermLike String s sym, Fields sym ~ '[t1]) => t1 -> s
string' = fromTerm . product' ; {-# INLINE string' #-}


acc' :: (TermLike Acc s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
acc' = fromTerm .: product' ; {-# INLINE acc' #-}

app' :: (TermLike App s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
app' = fromTerm .: product' ; {-# INLINE app' #-}

blank' :: (TermLike Blank s sym, Fields sym ~ '[]) => s
blank' = fromTerm product' ; {-# INLINE blank' #-}

cons' :: (TermLike Cons s sym, Fields sym ~ '[t1]) => t1 -> s
cons' = fromTerm . product' ; {-# INLINE cons' #-}

lam' :: (TermLike Lam s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
lam' = fromTerm .: product' ; {-# INLINE lam' #-}

-- match' :: (TermLike Match s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
-- match' = fromTerm .: product' ; {-# INLINE match' #-}

missing' :: (TermLike Missing s sym, Fields sym ~ '[]) => s
missing' = fromTerm product' ; {-# INLINE missing' #-}

star' :: (TermLike Star s sym, Fields sym ~ '[]) => s
star' = fromTerm product' ; {-# INLINE star' #-}

unify' :: (TermLike Unify s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
unify' = fromTerm .: product' ; {-# INLINE unify' #-}

var' :: (TermLike Var s sym, Fields sym ~ '[t1]) => t1 -> s
var' = fromTerm . product' ; {-# INLINE var' #-}




uncheckedInteger :: (UncheckedTermLike Integer s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedInteger = uncheckedFromTerm . product' ; {-# INLINE uncheckedInteger #-}

uncheckedRational :: (UncheckedTermLike Rational s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedRational = uncheckedFromTerm . product' ; {-# INLINE uncheckedRational #-}

uncheckedString :: (UncheckedTermLike String s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedString = uncheckedFromTerm . product' ; {-# INLINE uncheckedString #-}


uncheckedAcc :: (UncheckedTermLike Acc s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedAcc = uncheckedFromTerm .: product' ; {-# INLINE uncheckedAcc #-}

uncheckedApp :: (UncheckedTermLike App s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedApp = uncheckedFromTerm .: product' ; {-# INLINE uncheckedApp #-}

uncheckedBlank :: (UncheckedTermLike Blank s sym, Fields sym ~ '[]) => s
uncheckedBlank = uncheckedFromTerm product' ; {-# INLINE uncheckedBlank #-}

uncheckedCons :: (UncheckedTermLike Cons s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedCons = uncheckedFromTerm . product' ; {-# INLINE uncheckedCons #-}

uncheckedLam :: (UncheckedTermLike Lam s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedLam = uncheckedFromTerm .: product' ; {-# INLINE uncheckedLam #-}

-- uncheckedMatch :: (UncheckedTermLike Match s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
-- uncheckedMatch = uncheckedFromTerm .: product' ; {-# INLINE uncheckedMatch #-}

uncheckedMissing :: (UncheckedTermLike Missing s sym, Fields sym ~ '[]) => s
uncheckedMissing = uncheckedFromTerm product' ; {-# INLINE uncheckedMissing #-}

uncheckedStar :: (UncheckedTermLike Star s sym, Fields sym ~ '[]) => s
uncheckedStar = uncheckedFromTerm product' ; {-# INLINE uncheckedStar #-}

uncheckedUnify :: (UncheckedTermLike Unify s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedUnify = uncheckedFromTerm .: product' ; {-# INLINE uncheckedUnify #-}

uncheckedVar :: (UncheckedTermLike Var s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedVar = uncheckedFromTerm . product' ; {-# INLINE uncheckedVar #-}






integer :: P.Integer -> NamedTerm Integer n a
integer = product' ; {-# INLINE integer #-}

rational :: P.Rational -> NamedTerm Rational n a
rational = product' ; {-# INLINE rational #-}

string :: P.String -> NamedTerm String n a
string = product' ; {-# INLINE string #-}


acc :: n -> a -> NamedTerm Acc  n a
acc = product' ; {-# INLINE acc #-}

app :: a -> Arg a -> NamedTerm App n a
app = product' ; {-# INLINE app #-}

blank :: NamedTerm Blank n a
blank = product' ; {-# INLINE blank #-}

cons :: n -> NamedTerm Cons n a
cons = product' ; {-# INLINE cons #-}

lam :: Arg a -> a -> NamedTerm Lam n a
lam = product' ; {-# INLINE lam #-}

match :: a -> a -> NamedTerm Match n a
match = product' ; {-# INLINE match #-}

missing :: NamedTerm Missing n a
missing = product' ; {-# INLINE missing #-}

star :: NamedTerm Star n a
star = product' ; {-# INLINE star #-}

unify :: a -> a -> NamedTerm Unify n a
unify = product' ; {-# INLINE unify #-}

var :: n -> NamedTerm Var n a
var = product' ; {-# INLINE var #-}
