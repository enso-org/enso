{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr.Term.Named (module Luna.IR.Expr.Term.Named, module X) where

import qualified Luna.Prelude as P
import           Luna.Prelude hiding (String, Integer, Rational, Data, product, product')

import Luna.IR.Expr.Term.Class as X
import qualified Luna.IR.Expr.Atom as Atom
import           Luna.IR.Expr.Atom (Atom)

import Data.Base                 (Base)
import Data.Construction         (Args)
import Luna.IR.Function (Arg)
import qualified Luna.IR.Function.Argument as Arg
import Type.Applicative
import Data.Property
import Data.Phantom
import Luna.IR.Expr.Format
import qualified Luna.IR.Expr.Layout as Layout
import           Luna.IR.Expr.Layout (LAYOUT)

import Data.Construction
import           Data.RTuple (List(Null, (:-:)))
import qualified Data.RTuple as List
import Control.Lens.Utils (makePfxLenses)


type family NameOf a -- FIXME[WD] props?
class HasName a where
    name :: Lens' a (NameOf a)

---------------------
-- === Terms === --
---------------------

-- === Definitions === --

type NamedTerm t n a = Term t (Layout.Named n a)



newtype Integer  n a = Integer  { _val :: P.Integer                     }
newtype Rational n a = Rational { _val :: P.Rational                    }
newtype String   n a = String   { _val :: P.String                      }
data    Acc      n a = Acc      { _name :: !n      , _base  :: !a       }
data    App      n a = App      { _base :: a       , _arg   :: !(Arg a) }
data    Lam      n a = Lam      { _arg  :: !(Arg a), _body  :: !a       }
data    Unify    n a = Unify    { _left :: !a      , _right :: !a       }
newtype Cons     n a = Cons     { _name ::  n                           }
newtype Var      n a = Var      { _name ::  n                           }
newtype Grouped  n a = Grouped  { _base ::  a                           }
data    Blank    n a = Blank
data    Star     n a = Star
data    Missing  n a = Missing

type instance Term2 Atom.Integer  (Layout.Named n a) = Integer  n a
type instance Term2 Atom.Rational (Layout.Named n a) = Rational n a
type instance Term2 Atom.String   (Layout.Named n a) = String   n a
type instance Term2 Atom.Acc      (Layout.Named n a) = Acc      n a
type instance Term2 Atom.App      (Layout.Named n a) = App      n a
type instance Term2 Atom.Lam      (Layout.Named n a) = Lam      n a
type instance Term2 Atom.Unify    (Layout.Named n a) = Unify    n a
type instance Term2 Atom.Cons     (Layout.Named n a) = Cons     n a
type instance Term2 Atom.Var      (Layout.Named n a) = Var      n a
type instance Term2 Atom.Blank    (Layout.Named n a) = Blank    n a
type instance Term2 Atom.Star     (Layout.Named n a) = Star     n a
type instance Term2 Atom.Grouped  (Layout.Named n a) = Grouped  n a
type instance Term2 Atom.Missing  (Layout.Named n a) = Missing  n a


newtype instance Term Atom.Integer  (Layout.Named n a) = Sym_Integer  { _val :: P.Integer                     }
newtype instance Term Atom.Rational (Layout.Named n a) = Sym_Rational { _val :: P.Rational                    }
newtype instance Term Atom.String   (Layout.Named n a) = Sym_String   { _val :: P.String                      }
data    instance Term Atom.Acc      (Layout.Named n a) = Sym_Acc      { _name :: !n      , _base  :: !a       }
data    instance Term Atom.App      (Layout.Named n a) = Sym_App      { _base :: a       , _arg   :: !(Arg a) }
data    instance Term Atom.Lam      (Layout.Named n a) = Sym_Lam      { _arg  :: !(Arg a), _body  :: !a       }
data    instance Term Atom.Unify    (Layout.Named n a) = Sym_Unify    { _left :: !a      , _right :: !a       }
newtype instance Term Atom.Cons     (Layout.Named n a) = Sym_Cons     { _name ::  n                           }
newtype instance Term Atom.Var      (Layout.Named n a) = Sym_Var      { _name ::  n                           }
data    instance Term Atom.Grouped  (Layout.Named n a) = Sym_Grouped  { _base ::  a                           }
data    instance Term Atom.Blank    (Layout.Named n a) = Sym_Blank
data    instance Term Atom.Star     (Layout.Named n a) = Sym_Star
data    instance Term Atom.Missing  (Layout.Named n a) = Sym_Missing

-- makePfxLenses ''Term

type instance NameOf (Term s (Layout.Named n a)) = n
instance HasName (Term Atom.Var (Layout.Named n a)) where name = iso (\(Sym_Var n) -> n) Sym_Var









-- === Instances === --

-- Show


deriving instance ShowFields (NamedTerm Atom.Acc     n a) => Show (NamedTerm Atom.Acc     n a)
deriving instance ShowFields (NamedTerm Atom.App     n a) => Show (NamedTerm Atom.App     n a)
deriving instance ShowFields (NamedTerm Atom.Blank   n a) => Show (NamedTerm Atom.Blank   n a)
deriving instance ShowFields (NamedTerm Atom.Cons    n a) => Show (NamedTerm Atom.Cons    n a)
deriving instance ShowFields (NamedTerm Atom.Lam     n a) => Show (NamedTerm Atom.Lam     n a)
deriving instance ShowFields (NamedTerm Atom.Missing n a) => Show (NamedTerm Atom.Missing n a)
deriving instance ShowFields (NamedTerm Atom.Grouped n a) => Show (NamedTerm Atom.Grouped n a)
deriving instance ShowFields (NamedTerm Atom.Star    n a) => Show (NamedTerm Atom.Star    n a)
deriving instance ShowFields (NamedTerm Atom.Unify   n a) => Show (NamedTerm Atom.Unify   n a)
deriving instance ShowFields (NamedTerm Atom.Var     n a) => Show (NamedTerm Atom.Var     n a)

deriving instance ShowFields (Acc     n a) => Show (Acc     n a)
deriving instance ShowFields (App     n a) => Show (App     n a)
deriving instance ShowFields (Blank   n a) => Show (Blank   n a)
deriving instance ShowFields (Cons    n a) => Show (Cons    n a)
deriving instance ShowFields (Lam     n a) => Show (Lam     n a)
deriving instance ShowFields (Missing n a) => Show (Missing n a)
deriving instance ShowFields (Grouped n a) => Show (Grouped n a)
deriving instance ShowFields (Star    n a) => Show (Star    n a)
deriving instance ShowFields (Unify   n a) => Show (Unify   n a)
deriving instance ShowFields (Var     n a) => Show (Var     n a)

-- Args

type instance Fields (NamedTerm Atom.Integer  n a) = '[P.Integer]
type instance Fields (NamedTerm Atom.Rational n a) = '[P.Rational]
type instance Fields (NamedTerm Atom.String   n a) = '[P.String]
type instance Fields (NamedTerm Atom.Acc      n a) = '[n, a]
type instance Fields (NamedTerm Atom.App      n a) = '[a, Arg a]
type instance Fields (NamedTerm Atom.Blank    n a) = '[]
type instance Fields (NamedTerm Atom.Cons     n a) = '[n]
type instance Fields (NamedTerm Atom.Lam      n a) = '[Arg a, a]
type instance Fields (NamedTerm Atom.Grouped  n a) = '[a]
type instance Fields (NamedTerm Atom.Missing  n a) = '[]
type instance Fields (NamedTerm Atom.Star     n a) = '[]
type instance Fields (NamedTerm Atom.Unify    n a) = '[a, a]
type instance Fields (NamedTerm Atom.Var      n a) = '[n]

type instance Fields (Integer  n a) = '[P.Integer]
type instance Fields (Rational n a) = '[P.Rational]
type instance Fields (String   n a) = '[P.String]
type instance Fields (Acc      n a) = '[n, a]
type instance Fields (App      n a) = '[a, Arg a]
type instance Fields (Blank    n a) = '[]
type instance Fields (Cons     n a) = '[n]
type instance Fields (Lam      n a) = '[Arg a, a]
type instance Fields (Missing  n a) = '[]
type instance Fields (Grouped  n a) = '[a]
type instance Fields (Star     n a) = '[]
type instance Fields (Unify    n a) = '[a, a]
type instance Fields (Var      n a) = '[n]

-- Products

instance Product (NamedTerm Atom.Integer  n a) (NamedTerm Atom.Integer  n' a') where fields = iso (\(Sym_Integer  t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Integer  t1    ) ; {-# INLINE fields #-}
instance Product (NamedTerm Atom.Rational n a) (NamedTerm Atom.Rational n' a') where fields = iso (\(Sym_Rational t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Rational t1    ) ; {-# INLINE fields #-}
instance Product (NamedTerm Atom.String   n a) (NamedTerm Atom.String   n' a') where fields = iso (\(Sym_String   t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_String   t1    ) ; {-# INLINE fields #-}
instance Product (NamedTerm Atom.Acc      n a) (NamedTerm Atom.Acc      n' a') where fields = iso (\(Sym_Acc      t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_Acc      t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedTerm Atom.App      n a) (NamedTerm Atom.App      n' a') where fields = iso (\(Sym_App      t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_App      t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedTerm Atom.Blank    n a) (NamedTerm Atom.Blank    n' a') where fields = iso (\ Sym_Blank           -> Null              ) (\(Null              ) -> Sym_Blank          ) ; {-# INLINE fields #-}
instance Product (NamedTerm Atom.Cons     n a) (NamedTerm Atom.Cons     n' a') where fields = iso (\(Sym_Cons     t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Cons     t1    ) ; {-# INLINE fields #-}
instance Product (NamedTerm Atom.Lam      n a) (NamedTerm Atom.Lam      n' a') where fields = iso (\(Sym_Lam      t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_Lam      t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedTerm Atom.Missing  n a) (NamedTerm Atom.Missing  n' a') where fields = iso (\ Sym_Missing         -> Null              ) (\(Null              ) -> Sym_Missing        ) ; {-# INLINE fields #-}
instance Product (NamedTerm Atom.Grouped  n a) (NamedTerm Atom.Grouped  n' a') where fields = iso (\(Sym_Grouped  t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Grouped  t1    ) ; {-# INLINE fields #-}
instance Product (NamedTerm Atom.Star     n a) (NamedTerm Atom.Star     n' a') where fields = iso (\ Sym_Star            -> Null              ) (\(Null              ) -> Sym_Star           ) ; {-# INLINE fields #-}
instance Product (NamedTerm Atom.Unify    n a) (NamedTerm Atom.Unify    n' a') where fields = iso (\(Sym_Unify    t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_Unify    t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedTerm Atom.Var      n a) (NamedTerm Atom.Var      n' a') where fields = iso (\(Sym_Var      t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Var      t1    ) ; {-# INLINE fields #-}

instance Product' (NamedTerm Atom.Integer  n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Atom.Rational n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Atom.String   n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Atom.Acc      n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Atom.App      n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Atom.Blank    n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Atom.Cons     n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Atom.Lam      n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Atom.Missing  n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Atom.Grouped  n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Atom.Star     n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Atom.Unify    n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedTerm Atom.Var      n a) where fields' = fields ; {-# INLINE fields' #-}

-- Field names

instance HasFieldNames (NamedTerm Atom.Integer  n a) where fieldNames _ = [ "val"            ]
instance HasFieldNames (NamedTerm Atom.Rational n a) where fieldNames _ = [ "val"            ]
instance HasFieldNames (NamedTerm Atom.String   n a) where fieldNames _ = [ "val"            ]
instance HasFieldNames (NamedTerm Atom.Acc      n a) where fieldNames _ = [ "name" , "base"  ]
instance HasFieldNames (NamedTerm Atom.App      n a) where fieldNames _ = [ "base" , "arg"   ]
instance HasFieldNames (NamedTerm Atom.Lam      n a) where fieldNames _ = [ "arg"  , "body"  ]
instance HasFieldNames (NamedTerm Atom.Unify    n a) where fieldNames _ = [ "left" , "right" ]
instance HasFieldNames (NamedTerm Atom.Cons     n a) where fieldNames _ = [ "name"           ]
instance HasFieldNames (NamedTerm Atom.Var      n a) where fieldNames _ = [ "name"           ]
instance HasFieldNames (NamedTerm Atom.Grouped  n a) where fieldNames _ = [ "base"           ]
instance HasFieldNames (NamedTerm Atom.Blank    n a) where fieldNames _ = []
instance HasFieldNames (NamedTerm Atom.Star     n a) where fieldNames _ = []
instance HasFieldNames (NamedTerm Atom.Missing  n a) where fieldNames _ = []

-- FieldList

type instance FieldsType (NamedTerm t a a) = a

instance n ~ a => HasFields (NamedTerm Atom.Integer  n a) where fieldList (Sym_Integer  t1   ) = []
instance n ~ a => HasFields (NamedTerm Atom.Rational n a) where fieldList (Sym_Rational t1   ) = []
instance n ~ a => HasFields (NamedTerm Atom.String   n a) where fieldList (Sym_String   t1   ) = []
instance n ~ a => HasFields (NamedTerm Atom.Acc      n a) where fieldList (Sym_Acc      t1 t2) = [t1, t2]
instance n ~ a => HasFields (NamedTerm Atom.App      n a) where fieldList (Sym_App      t1 t2) = [t1, t2 ^. Arg._val_]
instance n ~ a => HasFields (NamedTerm Atom.Blank    n a) where fieldList (Sym_Blank         ) = []
instance n ~ a => HasFields (NamedTerm Atom.Cons     n a) where fieldList (Sym_Cons     t1   ) = [t1]
instance n ~ a => HasFields (NamedTerm Atom.Lam      n a) where fieldList (Sym_Lam      t1 t2) = [t1 ^. Arg._val_,t2]
instance n ~ a => HasFields (NamedTerm Atom.Missing  n a) where fieldList (Sym_Missing       ) = []
instance n ~ a => HasFields (NamedTerm Atom.Grouped  n a) where fieldList (Sym_Grouped  t1   ) = [t1]
instance n ~ a => HasFields (NamedTerm Atom.Star     n a) where fieldList (Sym_Star          ) = []
instance n ~ a => HasFields (NamedTerm Atom.Unify    n a) where fieldList (Sym_Unify    t1 t2) = [t1, t2]
instance n ~ a => HasFields (NamedTerm Atom.Var      n a) where fieldList (Sym_Var      t1   ) = [t1]

-- Inputs

type instance InputsType (NamedTerm t n a) = a

instance HasInputs (NamedTerm Atom.Integer  n a) where inputList (Sym_Integer  t1   ) = []
instance HasInputs (NamedTerm Atom.Rational n a) where inputList (Sym_Rational t1   ) = []
instance HasInputs (NamedTerm Atom.String   n a) where inputList (Sym_String   t1   ) = []
instance HasInputs (NamedTerm Atom.Acc      n a) where inputList (Sym_Acc      t1 t2) = [t2]
instance HasInputs (NamedTerm Atom.App      n a) where inputList (Sym_App      t1 t2) = [t1, t2 ^. Arg._val_]
instance HasInputs (NamedTerm Atom.Blank    n a) where inputList (Sym_Blank         ) = []
instance HasInputs (NamedTerm Atom.Cons     n a) where inputList (Sym_Cons     t1   ) = []
instance HasInputs (NamedTerm Atom.Lam      n a) where inputList (Sym_Lam      t1 t2) = [t1 ^. Arg._val_,t2]
instance HasInputs (NamedTerm Atom.Missing  n a) where inputList (Sym_Missing       ) = []
instance HasInputs (NamedTerm Atom.Grouped  n a) where inputList (Sym_Grouped  t1   ) = [t1]
instance HasInputs (NamedTerm Atom.Star     n a) where inputList (Sym_Star          ) = []
instance HasInputs (NamedTerm Atom.Unify    n a) where inputList (Sym_Unify    t1 t2) = [t1, t2]
instance HasInputs (NamedTerm Atom.Var      n a) where inputList (Sym_Var      t1   ) = []


--------------------------
-- === Construction === --
--------------------------

type TermLike          atom s sym = (sym ~ AsTerm s, Product' sym, atom ~ (s # Atom), FromTerm s)
type UncheckedTermLike atom s sym = (sym ~ AsTerm s, Product' sym, atom ~ (s # Atom), UncheckedFromTerm s)


integer' :: (TermLike Atom.Integer s sym, Fields sym ~ '[t1]) => t1 -> s
integer' = fromTerm . product' ; {-# INLINE integer' #-}

rational' :: (TermLike Atom.Rational s sym, Fields sym ~ '[t1]) => t1 -> s
rational' = fromTerm . product' ; {-# INLINE rational' #-}

string' :: (TermLike Atom.String s sym, Fields sym ~ '[t1]) => t1 -> s
string' = fromTerm . product' ; {-# INLINE string' #-}


acc' :: (TermLike Atom.Acc s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
acc' = fromTerm .: product' ; {-# INLINE acc' #-}

app' :: (TermLike Atom.App s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
app' = fromTerm .: product' ; {-# INLINE app' #-}

blank' :: (TermLike Atom.Blank s sym, Fields sym ~ '[]) => s
blank' = fromTerm product' ; {-# INLINE blank' #-}

cons' :: (TermLike Atom.Cons s sym, Fields sym ~ '[t1]) => t1 -> s
cons' = fromTerm . product' ; {-# INLINE cons' #-}

lam' :: (TermLike Atom.Lam s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
lam' = fromTerm .: product' ; {-# INLINE lam' #-}

missing' :: (TermLike Atom.Missing s sym, Fields sym ~ '[]) => s
missing' = fromTerm product' ; {-# INLINE missing' #-}

star' :: (TermLike Atom.Star s sym, Fields sym ~ '[]) => s
star' = fromTerm product' ; {-# INLINE star' #-}

unify' :: (TermLike Atom.Unify s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
unify' = fromTerm .: product' ; {-# INLINE unify' #-}

var' :: (TermLike Atom.Var s sym, Fields sym ~ '[t1]) => t1 -> s
var' = fromTerm . product' ; {-# INLINE var' #-}

grouped' :: (TermLike Atom.Grouped s sym, Fields sym ~ '[t1]) => t1 -> s
grouped' = fromTerm . product' ; {-# INLINE grouped' #-}




uncheckedInteger :: (UncheckedTermLike Atom.Integer s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedInteger = uncheckedFromTerm . product' ; {-# INLINE uncheckedInteger #-}

uncheckedRational :: (UncheckedTermLike Atom.Rational s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedRational = uncheckedFromTerm . product' ; {-# INLINE uncheckedRational #-}

uncheckedString :: (UncheckedTermLike Atom.String s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedString = uncheckedFromTerm . product' ; {-# INLINE uncheckedString #-}


uncheckedAcc :: (UncheckedTermLike Atom.Acc s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedAcc = uncheckedFromTerm .: product' ; {-# INLINE uncheckedAcc #-}

uncheckedApp :: (UncheckedTermLike Atom.App s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedApp = uncheckedFromTerm .: product' ; {-# INLINE uncheckedApp #-}

uncheckedBlank :: (UncheckedTermLike Atom.Blank s sym, Fields sym ~ '[]) => s
uncheckedBlank = uncheckedFromTerm product' ; {-# INLINE uncheckedBlank #-}

uncheckedCons :: (UncheckedTermLike Atom.Cons s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedCons = uncheckedFromTerm . product' ; {-# INLINE uncheckedCons #-}

uncheckedLam :: (UncheckedTermLike Atom.Lam s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedLam = uncheckedFromTerm .: product' ; {-# INLINE uncheckedLam #-}

uncheckedMissing :: (UncheckedTermLike Atom.Missing s sym, Fields sym ~ '[]) => s
uncheckedMissing = uncheckedFromTerm product' ; {-# INLINE uncheckedMissing #-}

uncheckedStar :: (UncheckedTermLike Atom.Star s sym, Fields sym ~ '[]) => s
uncheckedStar = uncheckedFromTerm product' ; {-# INLINE uncheckedStar #-}

uncheckedUnify :: (UncheckedTermLike Atom.Unify s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedUnify = uncheckedFromTerm .: product' ; {-# INLINE uncheckedUnify #-}

uncheckedVar :: (UncheckedTermLike Atom.Var s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedVar = uncheckedFromTerm . product' ; {-# INLINE uncheckedVar #-}

uncheckedGrouped :: (UncheckedTermLike Atom.Grouped s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedGrouped = uncheckedFromTerm . product' ; {-# INLINE uncheckedGrouped #-}





integer :: P.Integer -> NamedTerm Atom.Integer n a
integer = product' ; {-# INLINE integer #-}

rational :: P.Rational -> NamedTerm Atom.Rational n a
rational = product' ; {-# INLINE rational #-}

string :: P.String -> NamedTerm Atom.String n a
string = product' ; {-# INLINE string #-}


acc :: n -> a -> NamedTerm Atom.Acc  n a
acc = product' ; {-# INLINE acc #-}

app :: a -> Arg a -> NamedTerm Atom.App n a
app = product' ; {-# INLINE app #-}

blank :: NamedTerm Atom.Blank n a
blank = product' ; {-# INLINE blank #-}

cons :: n -> NamedTerm Atom.Cons n a
cons = product' ; {-# INLINE cons #-}

lam :: Arg a -> a -> NamedTerm Atom.Lam n a
lam = product' ; {-# INLINE lam #-}

missing :: NamedTerm Atom.Missing n a
missing = product' ; {-# INLINE missing #-}

star :: NamedTerm Atom.Star n a
star = product' ; {-# INLINE star #-}

unify :: a -> a -> NamedTerm Atom.Unify n a
unify = product' ; {-# INLINE unify #-}

var :: n -> NamedTerm Atom.Var n a
var = product' ; {-# INLINE var #-}

grouped :: a -> NamedTerm Atom.Grouped n a
grouped = product' ; {-# INLINE grouped #-}
