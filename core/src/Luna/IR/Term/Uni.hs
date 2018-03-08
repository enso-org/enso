{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term.Uni where

import           Luna.Prelude hiding (List, Interger, Rational, String, FilePath)
import qualified Luna.Prelude as Prelude

import Data.Map (Map)
import OCI.IR.Name.Qualified
import qualified OCI.IR.Layout         as Layout
import Luna.IR.Term ()
import qualified Luna.IR.Term.Core     as Term
import qualified Luna.IR.Term.Function as Term
import           Luna.IR.Term.Unit     (UnresolvedImport, UnresolvedImportSrc, UnresolvedImportTgt, ImportSource, ForeignImportType)
import qualified Luna.IR.Term.Unit     as Term hiding (World)
import qualified Luna.IR.Term.World    as Term
import qualified Luna.IR.Term.Cls    as Term
import qualified Luna.IR.Term.Literal  as Literal
import OCI.IR.Class as IR hiding (Import)
import OCI.IR.Layer
import OCI.IR.Term (Term(Term), TermDef)
import Data.Event (type(//))
import Data.AssocList (AssocList)
import           Data.Text32 (Text32)



-- === Abstraction === --

data family UniTerm a
class     IsUniTerm t where
    uniTerm :: forall a. t a -> UniTerm a


-- === Implementation === --

data instance UniTerm a = Number    !Literal.Number
                        | String    !Literal.String
                        | FmtString !(Literal.FmtString a)
                        | Acc       !a !Name
                        | App       !a !a
                        | Lam       !a !a
                        | Unify     !a !a
                        | Seq       !a !a
                        | Match     !a ![a]
                        | Cons      !Name ![a]
                        | Var       !Name
                        | Monadic   !a !a
                        | FieldLens !QualName
                        | Grouped   !a
                        | Blank
                        | Star
                        | Missing

                        -- | Function                  !a
                        | ASGFunction               !a ![a] !a
                        | FunctionSig               !a !a
                        | RootedFunction            !(IR.Rooted SomeExpr)
                        | ASGRootedFunction         !a !(IR.Rooted SomeExpr)
                        | ClsASG                    !Bool !Name ![a] ![a] ![a]
                        | RecASG                    !Name ![a]
                        | FieldASG                  ![Name] !a
                        | Typed                     !a !a
                        | Cls                       ![a] !(Map Name a) !(Map Name a) !(Map Name a)
                        | Rec                       !(AssocList Name a)
                        | Unit                      !a ![a] !a
                        | UnitProxy                 !QualName ![a]
                        | ImportHub                 !(Map Name a)
                        | Import                    !a !a
                        | UnresolvedImportHub       ![a]
                        | UnresolvedImport          !a ! UnresolvedImportTgt
                        | UnresolvedImportSrc       !ImportSource
                        | ForeignSymbolImport       !a !a !Name !a
                        | ForeignLocationImportList !a ![a]
                        | ForeignImportList         !Name ![a]
                        | ForeignImportSafety       !ForeignImportType
                        | World                     !(Map QualName a)
                        | Invalid                   !Text32
                        | List                      ![a]
                        | Tuple                     ![a]
                        | Update                    !a ![Name] !a
                        | Modify                    !a ![Name] !Name !a
                        | AccSection                ![Name]
                        | LeftSection               !a !a
                        | RightSection              !a !a

                        | Disabled                  !a
                        | Marker                    !Word64
                        | Marked                    !a !a
                        | Documented                !Text32 !a
                        | Metadata                  !Text32
                        deriving (Show)

instance IsUniTerm (TermDef atom) => IsUniTerm (Term atom) where uniTerm = uniTerm . unwrap

instance IsUniTerm Term.TermNumber    where uniTerm (Term.Number    t1)    = Number    t1
instance IsUniTerm Term.TermString    where uniTerm (Term.String    t1)    = String    t1
instance IsUniTerm Term.TermFmtString where uniTerm (Term.FmtString t1)    = FmtString t1
instance IsUniTerm Term.TermAcc       where uniTerm (Term.Acc       t1 t2) = Acc       t1 t2
instance IsUniTerm Term.TermApp       where uniTerm (Term.App       t1 t2) = App       t1 t2
instance IsUniTerm Term.TermLam       where uniTerm (Term.Lam       t1 t2) = Lam       t1 t2
instance IsUniTerm Term.TermUnify     where uniTerm (Term.Unify     t1 t2) = Unify     t1 t2
instance IsUniTerm Term.TermSeq       where uniTerm (Term.Seq       t1 t2) = Seq       t1 t2
instance IsUniTerm Term.TermCons      where uniTerm (Term.Cons      t1 t2) = Cons      t1 t2
instance IsUniTerm Term.TermMatch     where uniTerm (Term.Match     t1 t2) = Match     t1 t2
instance IsUniTerm Term.TermVar       where uniTerm (Term.Var       t1)    = Var       t1
instance IsUniTerm Term.TermFieldLens where uniTerm (Term.FieldLens t1)    = FieldLens t1
instance IsUniTerm Term.TermGrouped   where uniTerm (Term.Grouped   t1)    = Grouped   t1
instance IsUniTerm Term.TermMonadic   where uniTerm (Term.Monadic   t1 t2) = Monadic   t1 t2
instance IsUniTerm Term.TermBlank     where uniTerm  Term.Blank            = Blank
instance IsUniTerm Term.TermStar      where uniTerm  Term.Star             = Star
instance IsUniTerm Term.TermMissing   where uniTerm  Term.Missing          = Missing

instance IsUniTerm Term.TermASGFunction               where uniTerm (Term.ASGFunction               t1 t2 t3)       = ASGFunction               t1 t2 t3
-- instance IsUniTerm Term.TermFunction                  where uniTerm (Term.Function                  t1)             = Function            t1
instance IsUniTerm Term.TermFunctionSig               where uniTerm (Term.FunctionSig               t1 t2)          = FunctionSig               t1 t2
instance IsUniTerm Term.TermASGRootedFunction         where uniTerm (Term.ASGRootedFunction         t1 t2)          = ASGRootedFunction         t1 t2
instance IsUniTerm Term.TermRootedFunction            where uniTerm (Term.RootedFunction            t1)             = RootedFunction            t1
instance IsUniTerm Term.TermClsASG                    where uniTerm (Term.ClsASG                    t1 t2 t3 t4 t5) = ClsASG                    t1 t2 t3 t4 t5
instance IsUniTerm Term.TermRecASG                    where uniTerm (Term.RecASG                    t1 t2)          = RecASG                    t1 t2
instance IsUniTerm Term.TermFieldASG                  where uniTerm (Term.FieldASG                  t1 t2)          = FieldASG                  t1 t2
instance IsUniTerm Term.TermTyped                     where uniTerm (Term.Typed                     t1 t2)          = Typed                     t1 t2
instance IsUniTerm Term.TermCls                       where uniTerm (Term.Cls                       t1 t2 t3 t4)    = Cls                       t1 t2 t3 t4
instance IsUniTerm Term.TermRec                       where uniTerm (Term.Rec                       t1)             = Rec                       t1
instance IsUniTerm Term.TermUnit                      where uniTerm (Term.Unit                      t1 t2 t3)       = Unit                      t1 t2 t3
instance IsUniTerm Term.TermUnitProxy                 where uniTerm (Term.UnitProxy                 t1 t2)          = UnitProxy                 t1 t2
instance IsUniTerm Term.TermImportHub                 where uniTerm (Term.ImportHub                 t1)             = ImportHub                 t1
instance IsUniTerm Term.TermImport                    where uniTerm (Term.Import                    t1 t2)          = Import                    t1 t2
instance IsUniTerm Term.TermUnresolvedImportHub       where uniTerm (Term.UnresolvedImportHub       t1)             = UnresolvedImportHub       t1
instance IsUniTerm Term.TermUnresolvedImport          where uniTerm (Term.UnresolvedImport          t1 t2)          = UnresolvedImport          t1 t2
instance IsUniTerm Term.TermUnresolvedImportSrc       where uniTerm (Term.UnresolvedImportSrc       t1)             = UnresolvedImportSrc       t1
instance IsUniTerm Term.TermForeignSymbolImport       where uniTerm (Term.ForeignSymbolImport       t1 t2 t3 t4)    = ForeignSymbolImport       t1 t2 t3 t4
instance IsUniTerm Term.TermForeignLocationImportList where uniTerm (Term.ForeignLocationImportList t1 t2)          = ForeignLocationImportList t1 t2
instance IsUniTerm Term.TermForeignImportList         where uniTerm (Term.ForeignImportList         t1 t2)          = ForeignImportList         t1 t2
instance IsUniTerm Term.TermForeignImportSafety       where uniTerm (Term.ForeignImportSafety       t1)             = ForeignImportSafety       t1
instance IsUniTerm Term.TermWorld                     where uniTerm (Term.World                     t1)             = World                     t1
instance IsUniTerm Term.TermInvalid                   where uniTerm (Term.Invalid                   t1)             = Invalid                   t1
instance IsUniTerm Term.TermList                      where uniTerm (Term.List                      t1)             = List                      t1
instance IsUniTerm Term.TermTuple                     where uniTerm (Term.Tuple                     t1)             = Tuple                     t1
instance IsUniTerm Term.TermUpdate                    where uniTerm (Term.Update                    t1 t2 t3)       = Update                    t1 t2 t3
instance IsUniTerm Term.TermModify                    where uniTerm (Term.Modify                    t1 t2 t3 t4)    = Modify                    t1 t2 t3 t4
instance IsUniTerm Term.TermAccSection                where uniTerm (Term.AccSection                t1)             = AccSection                t1
instance IsUniTerm Term.TermLeftSection               where uniTerm (Term.LeftSection               t1 t2)          = LeftSection               t1 t2
instance IsUniTerm Term.TermRightSection              where uniTerm (Term.RightSection              t1 t2)          = RightSection              t1 t2

instance IsUniTerm Term.TermDisabled                  where uniTerm (Term.Disabled                  t1)             = Disabled                  t1
instance IsUniTerm Term.TermMarker                    where uniTerm (Term.Marker                    t1)             = Marker                    t1
instance IsUniTerm Term.TermMarked                    where uniTerm (Term.Marked                    t1 t2)          = Marked                    t1 t2
instance IsUniTerm Term.TermDocumented                where uniTerm (Term.Documented                t1 t2)          = Documented                t1 t2
instance IsUniTerm Term.TermMetadata                  where uniTerm (Term.Metadata                  t1)             = Metadata                  t1




newtype UniTerm'      t = UniTerm' (UniTerm (SubLink (Elem (EXPR ANY)) t))
makeWrapped ''UniTerm'

-- | Expr pattern matching utility
matchExpr :: (MonadRef m, Reader Layer (AnyExpr // Model) m)
      => Expr layout -> (Unwrapped (UniTerm' (Expr layout)) -> m a) -> m a
matchExpr t f = f . unwrap' =<< (exprUniTerm t)


-- | Term unification
exprUniTerm :: (MonadRef m, Reader Layer (AnyExpr // Model) m) => Expr layout -> m (UniTerm' (Expr layout))
exprUniTerm t = UniTerm' <$> symbolMapM_AB @ToUniTerm toUniTerm t

class ToUniTerm a b where toUniTerm :: a -> b
instance (Unwrapped a ~ Term t l, b ~ UniTerm l, IsUniTerm (Term t), Wrapped a)
      => ToUniTerm a b where toUniTerm = uniTerm . unwrap'
