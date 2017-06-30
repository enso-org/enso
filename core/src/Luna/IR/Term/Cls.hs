{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term.Cls where

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
import           Data.TreeSet (TreeSet)
import qualified GHC.Exts as GHC
import qualified Data.Map as Map
import           Data.Map (Map)
import Data.AssocList (AssocList)
import OCI.IR.Layout.Typed (type (>>))
import Luna.IR.Format
import OCI.IR.Layout.Class (generalize)


type ExprCons3' m a = (MonadRef m, Writer Net AnyExpr m, NewElemEvent m (Expr a), TermEncoder a)
type ExprCons3  m a = (ExprCons3' m a, Writer Net AnyExprLink m, NewElemEvent m SomeExprLink)



-------------------
-- === Class === --
-------------------

-- === Definition === --

newtype TermRec a = Rec { _fields  :: AssocList Name a } deriving (Show, Functor, Foldable, Traversable, Mempty)
data    TermCls a = Cls { _params  :: ![a]
                        , _conses  :: !(Map Name a)
                        , _classes :: !(Map Name a)
                        , _methods :: !(Map Name a)
                        } deriving (Show, Functor, Foldable, Traversable)


makeLensedTerm ''TermRec
makeLensedTerm ''TermCls


-- === Construction === --

record' :: ExprCons3 m Rec => AssocList Name (Expr a) -> m SomeExpr
record  :: ExprCons3 m Rec => AssocList Name (Expr a) -> m (Expr $ Rec >> a)
record' = fmap generalize . record
record lst = mdo
    t    <- expr $ uncheckedRec llst
    llst <- mapM (flip link t . unsafeRelayout) lst
    return t

cls' :: ExprCons3 m Cls => [Expr a] -> Map Name (Expr a) -> Map Name (Expr a) -> Map Name (Expr a) -> m SomeExpr
cls  :: ExprCons3 m Cls => [Expr a] -> Map Name (Expr a) -> Map Name (Expr a) -> Map Name (Expr a) -> m (Expr $ Cls >> a)
cls' = fmap generalize .:: cls
cls params conses classes methods = mdo
    t  <- expr $ uncheckedCls lparams lconses lclasses lmethods
    lparams  <- mapM (flip link t . unsafeRelayout) params
    lconses  <- mapM (flip link t . unsafeRelayout) conses
    lclasses <- mapM (flip link t . unsafeRelayout) classes
    lmethods <- mapM (flip link t . unsafeRelayout) methods
    return t

wireCls' :: ExprCons3 m Cls => TermCls (Expr a) -> m SomeExpr
wireCls  :: ExprCons3 m Cls => TermCls (Expr a) -> m (Expr $ Cls >> a)
wireCls' = fmap generalize . wireCls
wireCls (Cls params conses classes methods) = cls params conses classes methods


-- === Instances === --

-- Format
type instance Access Format Cls = Draft
type instance Access Format Rec = Draft

-- Monoids
instance Mempty (TermCls a) where mempty = Cls mempty mempty mempty mempty
