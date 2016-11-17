{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term.Expr.Symbol.Named (module Luna.Syntax.Term.Expr.Symbol.Named, module X) where

import qualified Prelude.Luna as P
import           Prelude.Luna hiding (Symbol, String, Integer, Rational, Data, product, product')

import Luna.Syntax.Term.Expr.Symbol.Class as X
import Luna.Syntax.Term.Expr.Atom as X (Atom, String, Integer, Rational, Acc, App, Blank, Cons, Lam, Match, Missing, Native, Star, Unify, Var) -- Types only

import Data.Base                 (Base)
import Data.Construction         (Args)
import Luna.Runtime.Dynamics     (Dynamics, ByDynamics)
import Luna.Syntax.Term.Function (Arg)
import qualified Luna.Syntax.Term.Function.Argument as Arg
import Type.Applicative
import Data.Property
import Data.Phantom
import Luna.Syntax.Term.Expr.Format
import qualified Luna.Syntax.Term.Expr.Layout as Layout
import           Luna.Syntax.Term.Expr.Layout (Layout)

import Data.Construction
import           Data.RTuple (List(Null, (:-:)))
import qualified Data.RTuple as List


---------------------
-- === Symbols === --
---------------------

-- === Definitions === --

type NamedSymbol t n a = Symbol t (Layout.Named n a)


newtype instance Symbol Integer  (Layout.Named n a) = Sym_Integer  { _val :: P.Integer                    }
newtype instance Symbol Rational (Layout.Named n a) = Sym_Rational { _val :: P.Rational                   }
newtype instance Symbol String   (Layout.Named n a) = Sym_String   { _val :: P.String                     }
data    instance Symbol Acc      (Layout.Named n a) = Sym_Acc      { _name :: !n     , _base  :: !a       }
data    instance Symbol App      (Layout.Named n a) = Sym_App      { _base :: a      , _arg   :: !(Arg a) }
data    instance Symbol Lam      (Layout.Named n a) = Sym_Lam      { _arg :: !(Arg a), _body  :: !a       }
data    instance Symbol Match    (Layout.Named n a) = Sym_Match    { _left :: !a     , _right :: !a       }
data    instance Symbol Unify    (Layout.Named n a) = Sym_Unify    { _left :: !a     , _right :: !a       }
newtype instance Symbol Cons     (Layout.Named n a) = Sym_Cons     { _name ::  n                          }
data    instance Symbol Native   (Layout.Named n a) = Sym_Native   { _name :: !n                          }
newtype instance Symbol Var      (Layout.Named n a) = Sym_Var      { _name ::  n                          }
data    instance Symbol Blank    (Layout.Named n a) = Sym_Blank
data    instance Symbol Star     (Layout.Named n a) = Sym_Star
data    instance Symbol Missing  (Layout.Named n a) = Sym_Missing

data instance UniSymbol (Layout.Named n a) = Integer  P.Integer
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

instance IsUniSymbol Integer  (Layout.Named n a) where uniSymbol (Sym_Integer  t1)    = Integer  t1
instance IsUniSymbol Rational (Layout.Named n a) where uniSymbol (Sym_Rational t1)    = Rational t1
instance IsUniSymbol String   (Layout.Named n a) where uniSymbol (Sym_String   t1)    = String   t1
instance IsUniSymbol Acc      (Layout.Named n a) where uniSymbol (Sym_Acc      t1 t2) = Acc      t1 t2
instance IsUniSymbol App      (Layout.Named n a) where uniSymbol (Sym_App      t1 t2) = App      t1 t2
instance IsUniSymbol Lam      (Layout.Named n a) where uniSymbol (Sym_Lam      t1 t2) = Lam      t1 t2
instance IsUniSymbol Match    (Layout.Named n a) where uniSymbol (Sym_Match    t1 t2) = Match    t1 t2
instance IsUniSymbol Unify    (Layout.Named n a) where uniSymbol (Sym_Unify    t1 t2) = Unify    t1 t2
instance IsUniSymbol Cons     (Layout.Named n a) where uniSymbol (Sym_Cons     t1)    = Cons     t1
instance IsUniSymbol Native   (Layout.Named n a) where uniSymbol (Sym_Native   t1)    = Native   t1
instance IsUniSymbol Var      (Layout.Named n a) where uniSymbol (Sym_Var      t1)    = Var      t1
instance IsUniSymbol Blank    (Layout.Named n a) where uniSymbol  Sym_Blank           = Blank
instance IsUniSymbol Star     (Layout.Named n a) where uniSymbol  Sym_Star            = Star
instance IsUniSymbol Missing  (Layout.Named n a) where uniSymbol  Sym_Missing         = Missing


-- === Instances === --

-- Show

deriving instance (Show n, Show a) => Show (UniSymbol (Layout.Named n a))

deriving instance ShowFields (NamedSymbol Acc     n a) => Show (NamedSymbol Acc     n a)
deriving instance ShowFields (NamedSymbol App     n a) => Show (NamedSymbol App     n a)
deriving instance ShowFields (NamedSymbol Blank   n a) => Show (NamedSymbol Blank   n a)
deriving instance ShowFields (NamedSymbol Cons    n a) => Show (NamedSymbol Cons    n a)
deriving instance ShowFields (NamedSymbol Lam     n a) => Show (NamedSymbol Lam     n a)
deriving instance ShowFields (NamedSymbol Match   n a) => Show (NamedSymbol Match   n a)
deriving instance ShowFields (NamedSymbol Missing n a) => Show (NamedSymbol Missing n a)
deriving instance ShowFields (NamedSymbol Native  n a) => Show (NamedSymbol Native  n a)
deriving instance ShowFields (NamedSymbol Star    n a) => Show (NamedSymbol Star    n a)
deriving instance ShowFields (NamedSymbol Unify   n a) => Show (NamedSymbol Unify   n a)
deriving instance ShowFields (NamedSymbol Var     n a) => Show (NamedSymbol Var     n a)

-- Args

type instance Fields (NamedSymbol Integer  n a) = '[P.Integer]
type instance Fields (NamedSymbol Rational n a) = '[P.Rational]
type instance Fields (NamedSymbol String   n a) = '[P.String]

type instance Fields (NamedSymbol Acc      n a) = '[n, a]
type instance Fields (NamedSymbol App      n a) = '[a, Arg a]
type instance Fields (NamedSymbol Blank    n a) = '[]
type instance Fields (NamedSymbol Cons     n a) = '[n]
type instance Fields (NamedSymbol Lam      n a) = '[Arg a, a]
type instance Fields (NamedSymbol Match    n a) = '[a, a]
type instance Fields (NamedSymbol Missing  n a) = '[]
type instance Fields (NamedSymbol Native   n a) = '[n]
type instance Fields (NamedSymbol Star     n a) = '[]
type instance Fields (NamedSymbol Unify    n a) = '[a, a]
type instance Fields (NamedSymbol Var      n a) = '[n]

-- Products

instance Product (NamedSymbol Integer  n a) (NamedSymbol Integer  n' a') where fields = iso (\(Sym_Integer  t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Integer  t1    ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Rational n a) (NamedSymbol Rational n' a') where fields = iso (\(Sym_Rational t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Rational t1    ) ; {-# INLINE fields #-}
instance Product (NamedSymbol String   n a) (NamedSymbol String   n' a') where fields = iso (\(Sym_String   t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_String   t1    ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Acc      n a) (NamedSymbol Acc      n' a') where fields = iso (\(Sym_Acc      t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_Acc      t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedSymbol App      n a) (NamedSymbol App      n' a') where fields = iso (\(Sym_App      t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_App      t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Blank    n a) (NamedSymbol Blank    n' a') where fields = iso (\ Sym_Blank           -> Null              ) (\(Null              ) -> Sym_Blank          ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Cons     n a) (NamedSymbol Cons     n' a') where fields = iso (\(Sym_Cons     t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Cons     t1    ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Lam      n a) (NamedSymbol Lam      n' a') where fields = iso (\(Sym_Lam      t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_Lam      t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Match    n a) (NamedSymbol Match    n' a') where fields = iso (\(Sym_Match    t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_Match    t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Missing  n a) (NamedSymbol Missing  n' a') where fields = iso (\ Sym_Missing         -> Null              ) (\(Null              ) -> Sym_Missing        ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Native   n a) (NamedSymbol Native   n' a') where fields = iso (\(Sym_Native   t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Native   t1    ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Star     n a) (NamedSymbol Star     n' a') where fields = iso (\ Sym_Star            -> Null              ) (\(Null              ) -> Sym_Star           ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Unify    n a) (NamedSymbol Unify    n' a') where fields = iso (\(Sym_Unify    t1 t2) -> t1 :-: t2 :-: Null) (\(t1 :-: t2 :-: Null) -> Sym_Unify    t1 t2 ) ; {-# INLINE fields #-}
instance Product (NamedSymbol Var      n a) (NamedSymbol Var      n' a') where fields = iso (\(Sym_Var      t1   ) -> t1 :-: Null       ) (\(t1 :-: Null       ) -> Sym_Var      t1    ) ; {-# INLINE fields #-}

instance Product' (NamedSymbol Integer  n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol Rational n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol String   n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol Acc      n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol App      n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol Blank    n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol Cons     n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol Lam      n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol Match    n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol Missing  n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol Native   n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol Star     n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol Unify    n a) where fields' = fields ; {-# INLINE fields' #-}
instance Product' (NamedSymbol Var      n a) where fields' = fields ; {-# INLINE fields' #-}

-- Field names

instance HasFieldNames (NamedSymbol Integer  n a) where fieldNames _ = [ "val"            ]
instance HasFieldNames (NamedSymbol Rational n a) where fieldNames _ = [ "val"            ]
instance HasFieldNames (NamedSymbol String   n a) where fieldNames _ = [ "val"            ]
instance HasFieldNames (NamedSymbol Acc      n a) where fieldNames _ = [ "name" , "base"  ]
instance HasFieldNames (NamedSymbol App      n a) where fieldNames _ = [ "base" , "arg"   ]
instance HasFieldNames (NamedSymbol Lam      n a) where fieldNames _ = [ "arg"  , "body"  ]
instance HasFieldNames (NamedSymbol Match    n a) where fieldNames _ = [ "left" , "right" ]
instance HasFieldNames (NamedSymbol Unify    n a) where fieldNames _ = [ "left" , "right" ]
instance HasFieldNames (NamedSymbol Cons     n a) where fieldNames _ = [ "name"           ]
instance HasFieldNames (NamedSymbol Native   n a) where fieldNames _ = [ "name"           ]
instance HasFieldNames (NamedSymbol Var      n a) where fieldNames _ = [ "name"           ]
instance HasFieldNames (NamedSymbol Blank    n a) where fieldNames _ = []
instance HasFieldNames (NamedSymbol Star     n a) where fieldNames _ = []
instance HasFieldNames (NamedSymbol Missing  n a) where fieldNames _ = []

-- FieldList

type instance FieldsType (NamedSymbol t a a) = a

instance n ~ a => HasFields (NamedSymbol Integer  n a) where fieldList (Sym_Integer  t1   ) = []
instance n ~ a => HasFields (NamedSymbol Rational n a) where fieldList (Sym_Rational t1   ) = []
instance n ~ a => HasFields (NamedSymbol String   n a) where fieldList (Sym_String   t1   ) = []
instance n ~ a => HasFields (NamedSymbol Acc      n a) where fieldList (Sym_Acc      t1 t2) = [t1, t2]
instance n ~ a => HasFields (NamedSymbol App      n a) where fieldList (Sym_App      t1 t2) = [t1, t2 ^. Arg._val_]
instance n ~ a => HasFields (NamedSymbol Blank    n a) where fieldList (Sym_Blank         ) = []
instance n ~ a => HasFields (NamedSymbol Cons     n a) where fieldList (Sym_Cons     t1   ) = [t1]
instance n ~ a => HasFields (NamedSymbol Lam      n a) where fieldList (Sym_Lam      t1 t2) = [t1 ^. Arg._val_,t2]
instance n ~ a => HasFields (NamedSymbol Match    n a) where fieldList (Sym_Match    t1 t2) = [t1,t2]
instance n ~ a => HasFields (NamedSymbol Missing  n a) where fieldList (Sym_Missing       ) = []
instance n ~ a => HasFields (NamedSymbol Native   n a) where fieldList (Sym_Native   t1   ) = [t1]
instance n ~ a => HasFields (NamedSymbol Star     n a) where fieldList (Sym_Star          ) = []
instance n ~ a => HasFields (NamedSymbol Unify    n a) where fieldList (Sym_Unify    t1 t2) = [t1, t2]
instance n ~ a => HasFields (NamedSymbol Var      n a) where fieldList (Sym_Var      t1   ) = [t1]


--------------------------
-- === Construction === --
--------------------------

type Symbolic          atom s sym = (sym ~ AsSymbol s, Product' sym, atom ~ (s # Atom), FromSymbol s)
type UncheckedSymbolic atom s sym = (sym ~ AsSymbol s, Product' sym, atom ~ (s # Atom), UncheckedFromSymbol s)


integer' :: (Symbolic Integer s sym, Fields sym ~ '[t1]) => t1 -> s
integer' = fromSymbol . product' ; {-# INLINE integer' #-}

rational' :: (Symbolic Rational s sym, Fields sym ~ '[t1]) => t1 -> s
rational' = fromSymbol . product' ; {-# INLINE rational' #-}

string' :: (Symbolic String s sym, Fields sym ~ '[t1]) => t1 -> s
string' = fromSymbol . product' ; {-# INLINE string' #-}


acc' :: (Symbolic Acc s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
acc' = fromSymbol .: product' ; {-# INLINE acc' #-}

app' :: (Symbolic App s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
app' = fromSymbol .: product' ; {-# INLINE app' #-}

blank' :: (Symbolic Blank s sym, Fields sym ~ '[]) => s
blank' = fromSymbol product' ; {-# INLINE blank' #-}

cons' :: (Symbolic Cons s sym, Fields sym ~ '[t1]) => t1 -> s
cons' = fromSymbol . product' ; {-# INLINE cons' #-}

lam' :: (Symbolic Lam s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
lam' = fromSymbol .: product' ; {-# INLINE lam' #-}

match' :: (Symbolic Match s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
match' = fromSymbol .: product' ; {-# INLINE match' #-}

missing' :: (Symbolic Missing s sym, Fields sym ~ '[]) => s
missing' = fromSymbol product' ; {-# INLINE missing' #-}

star' :: (Symbolic Star s sym, Fields sym ~ '[]) => s
star' = fromSymbol product' ; {-# INLINE star' #-}

unify' :: (Symbolic Unify s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
unify' = fromSymbol .: product' ; {-# INLINE unify' #-}

var' :: (Symbolic Var s sym, Fields sym ~ '[t1]) => t1 -> s
var' = fromSymbol . product' ; {-# INLINE var' #-}




uncheckedInteger :: (UncheckedSymbolic Integer s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedInteger = uncheckedFromSymbol . product' ; {-# INLINE uncheckedInteger #-}

uncheckedRational :: (UncheckedSymbolic Rational s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedRational = uncheckedFromSymbol . product' ; {-# INLINE uncheckedRational #-}

uncheckedString :: (UncheckedSymbolic String s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedString = uncheckedFromSymbol . product' ; {-# INLINE uncheckedString #-}


uncheckedAcc :: (UncheckedSymbolic Acc s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedAcc = uncheckedFromSymbol .: product' ; {-# INLINE uncheckedAcc #-}

uncheckedApp :: (UncheckedSymbolic App s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedApp = uncheckedFromSymbol .: product' ; {-# INLINE uncheckedApp #-}

uncheckedBlank :: (UncheckedSymbolic Blank s sym, Fields sym ~ '[]) => s
uncheckedBlank = uncheckedFromSymbol product' ; {-# INLINE uncheckedBlank #-}

uncheckedCons :: (UncheckedSymbolic Cons s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedCons = uncheckedFromSymbol . product' ; {-# INLINE uncheckedCons #-}

uncheckedLam :: (UncheckedSymbolic Lam s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedLam = uncheckedFromSymbol .: product' ; {-# INLINE uncheckedLam #-}

uncheckedMatch :: (UncheckedSymbolic Match s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedMatch = uncheckedFromSymbol .: product' ; {-# INLINE uncheckedMatch #-}

uncheckedMissing :: (UncheckedSymbolic Missing s sym, Fields sym ~ '[]) => s
uncheckedMissing = uncheckedFromSymbol product' ; {-# INLINE uncheckedMissing #-}

uncheckedStar :: (UncheckedSymbolic Star s sym, Fields sym ~ '[]) => s
uncheckedStar = uncheckedFromSymbol product' ; {-# INLINE uncheckedStar #-}

uncheckedUnify :: (UncheckedSymbolic Unify s sym, Fields sym ~ '[t1,t2]) => t1 -> t2 -> s
uncheckedUnify = uncheckedFromSymbol .: product' ; {-# INLINE uncheckedUnify #-}

uncheckedVar :: (UncheckedSymbolic Var s sym, Fields sym ~ '[t1]) => t1 -> s
uncheckedVar = uncheckedFromSymbol . product' ; {-# INLINE uncheckedVar #-}






integer :: P.Integer -> NamedSymbol Integer n a
integer = product' ; {-# INLINE integer #-}

rational :: P.Rational -> NamedSymbol Rational n a
rational = product' ; {-# INLINE rational #-}

string :: P.String -> NamedSymbol String n a
string = product' ; {-# INLINE string #-}


acc :: n -> a -> NamedSymbol Acc  n a
acc = product' ; {-# INLINE acc #-}

app :: a -> Arg a -> NamedSymbol App n a
app = product' ; {-# INLINE app #-}

blank :: NamedSymbol Blank n a
blank = product' ; {-# INLINE blank #-}

cons :: n -> NamedSymbol Cons n a
cons = product' ; {-# INLINE cons #-}

lam :: Arg a -> a -> NamedSymbol Lam n a
lam = product' ; {-# INLINE lam #-}

match :: a -> a -> NamedSymbol Match n a
match = product' ; {-# INLINE match #-}

missing :: NamedSymbol Missing n a
missing = product' ; {-# INLINE missing #-}

star :: NamedSymbol Star n a
star = product' ; {-# INLINE star #-}

unify :: a -> a -> NamedSymbol Unify n a
unify = product' ; {-# INLINE unify #-}

var :: n -> NamedSymbol Var n a
var = product' ; {-# INLINE var #-}
