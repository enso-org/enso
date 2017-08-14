{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# EXT      InlineAll                 #-}

module Luna.IR.Term.Core where

import qualified Luna.Prelude as P
import           Luna.Prelude hiding (String, Data, List, product, product')

import           OCI.IR.Name.Path
import           OCI.IR.Name.Qualified
import           OCI.IR.Term
import           OCI.IR.Term   (TermType)
import qualified OCI.IR.Layout as Layout
import qualified Luna.IR.Term.Literal as Literal
import           Luna.IR.Term.Literal (HasLiteral, LiteralOf, literal)
import           Data.Text32 (Text32)

import Data.Property
import Data.Families (makeLunaComponents, makeLensedTerms)


-------------------
-- === Terms === --
-------------------

-- === Definitions === --

newtype TermNumber    a = Number    { __val    :: Literal.Number                  } deriving (Show, Eq, Functor, Foldable, Traversable)
newtype TermString    a = String    { __val    :: Literal.String                  } deriving (Show, Eq, Functor, Foldable, Traversable)
newtype TermFmtString a = FmtString { __val    :: Literal.FmtString a             } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermAcc       a = Acc       { __base   :: !a       , __name    :: !Name   } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermApp       a = App       { __base   :: !a       , __arg     :: !a      } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermLam       a = Lam       { __arg    :: !a       , __body    :: !a      } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermSeq       a = Seq       { __left   :: !a       , __right   :: !a      } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermUnify     a = Unify     { __left   :: !a       , __right   :: !a      } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermCons      a = Cons      { __path   :: !Name    , __fields  :: ![a]    } deriving (Show, Eq, Functor, Foldable, Traversable)
newtype TermVar       a = Var       { __name   :: Name                            } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermMonadic   a = Monadic   { __child  :: !a       , __monad   :: !a      } deriving (Show, Eq, Functor, Foldable, Traversable)
newtype TermGrouped   a = Grouped   { __base   ::  a                              } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermBlank     a = Blank                                                     deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermMissing   a = Missing                                                   deriving (Show, Eq, Functor, Foldable, Traversable)

-- Patterns
data    TermMatch     a = Match     { __arg     :: !a, __clauses :: ![a]        } deriving (Show, Eq, Functor, Foldable, Traversable)

newtype TermFieldLens a = FieldLens { __path :: QualName                        } deriving (Show, Eq, Functor, Foldable, Traversable) -- TODO: remove
data    TermStar      a = Star                                                    deriving (Show, Eq, Functor, Foldable, Traversable) -- TODO: remove

data    TermClsASG   a = ClsASG   { __isNative :: Bool, __name  :: !Name  , __params :: ![a], __conss :: ![a], __decls :: ![a] } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermRecASG   a = RecASG   { __name  :: !Name  , __fields :: ![a]                                                       } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermFieldASG a = FieldASG { __names :: ![Name], __type   :: !a                                                         } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermTyped    a = Typed    { __base  :: !a     , __type   :: !a                                                         } deriving (Show, Eq, Functor, Foldable, Traversable)

data    TermInvalid   a = Invalid { __desc  :: Text32                         } deriving (Show, Eq, Functor, Foldable, Traversable) -- TODO: Text -> Doc
data    TermList      a = List    { __items :: ![a]                           } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermTuple     a = Tuple   { __items :: ![a]                           } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermAccSection   a = AccSection   { __name     :: ![Name]             } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermLeftSection  a = LeftSection  { __operator :: !a   , __body :: !a } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermRightSection a = RightSection { __operator :: !a   , __body :: !a } deriving (Show, Eq, Functor, Foldable, Traversable)
newtype TermDisabled     a = Disabled     { __body     :: a                   } deriving (Show, Eq, Functor, Foldable, Traversable)
newtype TermMarker       a = Marker       { __markerId :: Word64              } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermMarked       a = Marked       { __marker   :: !a   , __body :: !a } deriving (Show, Eq, Functor, Foldable, Traversable)
newtype TermMetadata     a = Metadata     { __content  :: Text32              } deriving (Show, Eq, Functor, Foldable, Traversable)

makeLensedTerms "CoreTerms" [ ''TermNumber, ''TermString, ''TermFmtString, ''TermAcc, ''TermApp, ''TermLam, ''TermSeq, ''TermUnify
                            , ''TermCons, ''TermMatch, ''TermMonadic, ''TermVar, ''TermFieldLens, ''TermGrouped, ''TermBlank, ''TermStar, ''TermMissing, ''TermClsASG
                            , ''TermRecASG, ''TermFieldASG, ''TermTyped, ''TermInvalid, ''TermList, ''TermTuple, ''TermLeftSection, ''TermRightSection, ''TermAccSection
                            , ''TermDisabled, ''TermMarker, ''TermMarked, ''TermMetadata
                            ]


-- === Instances === --

-- HasLiteral

type instance LiteralOf (Term t a) = LiteralOf (TermDef t a)
instance HasLiteral (TermDef t a) => HasLiteral (Term t a) where literal = wrapped . literal

type instance LiteralOf  (TermString    a) = Literal.String
type instance LiteralOf  (TermFmtString a) = Literal.FmtString a
type instance LiteralOf  (TermNumber    a) = Literal.Number
instance      HasLiteral (TermString    a) where literal = wrapped
instance      HasLiteral (TermFmtString a) where literal = wrapped
instance      HasLiteral (TermNumber    a) where literal = wrapped

-- HasName

instance HasName (TermDef t a)
      => HasName (Term    t a) where name = wrapped . name
instance HasName (TermVar   a) where name = termVar_name
instance HasName (TermAcc   a) where name = termAcc_name

-- Generalizable

-- type instance Layout.Generalizable (Clause a) (Clause b) = Layout.Generalizable a b






--
-- -- PROBLEMY:
-- --
-- -- 1. Niejednoznaczne IR:
-- --     a. Node Cons jest niejednoznaczny - przechowuje argumenty ale App pozwala w większości przypadków na to samo
-- --     b. Clause.pattern ma typ `a`, ktory w szczegolności jest Draft'em. To jest złe bo pattern matche nie mogą być typu np. `foo bar`
-- --
-- -- 2. IR nie pokrywa przypadków użycia:
-- --     a. Pattern match `Math.Vector x y z = vec` jest poprawny i obecnie praktycznie niemożliwy do wyrażenia (można użyć Appów, ale potem po optymalizacji ciężko cokolwiek z tym zrobić)
-- --     b. Na warstwach trzymam mapowanie IR <-> Tekst. Przez to że Clause nie jest nodem nie mogę mapować się na linijki w pattern matchach.
-- --        W obecnym designie Clause nie może być nodem (!) albo dopuścimy __clauses jako dowolny Draft :/
-- --
-- -- 3. IR nie jest zaplanowany w szerokiej wizji:
-- --     a. Nie mamy zaplanowanego CORE i nie jesteśmy w stanie łatwo go zrobić. Zaplanować go powinniśmy by wiedzieć jak designować wszystko wkoło.
--
---- FIXME: Do przemyslenia: co z pattern matchami typu: `(foo a).Vector x y z = vec` ? (Luna Dynamic / DepType)
--
--
-- -------------------
-- -- === Terms === --
-- -------------------
--
-- -- === Definitions === --
--
-- data Clause a = Clause { _pattern :: !a
--                        , _result  :: !a
--                        } deriving (Show, Eq, Functor, Foldable, Traversable)
--
--
-- newtype TermNumber    a = Number    { __val  :: Literal.Number                  } deriving (Show, Eq, Functor, Foldable, Traversable)
-- newtype TermString    a = String    { __val  :: Literal.String                  } deriving (Show, Eq, Functor, Foldable, Traversable)
-- newtype TermFmtString a = FmtString { __val  :: Literal.FmtString a             } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermAcc       a = Acc       { __base :: !a   , __name    :: !Name       } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermApp       a = App       { __base :: !a   , __arg     :: !a          } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermLam       a = Lam       { __arg  :: !a   , __body    :: !a          } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermSeq       a = Seq       { __left :: !a   , __right   :: !a          } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermUnify     a = Unify     { __left :: !a   , __right   :: !a          } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermCons      a = Cons      { __name :: !Name, __fields  :: ![a]        } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermMatch     a = Match     { __arg  :: !a   , __clauses :: ![Clause a] } deriving (Show, Eq, Functor, Foldable, Traversable)
-- newtype TermVar       a = Var       { __name :: Name                            } deriving (Show, Eq, Functor, Foldable, Traversable)
-- newtype TermGrouped   a = Grouped   { __base :: a                               } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermBlank     a = Blank                                                   deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermStar      a = Star                                                    deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermMissing   a = Missing                                                 deriving (Show, Eq, Functor, Foldable, Traversable)
--
--
--
--
-- Link a b ~> c = Link a c
--
--
-- -- Dzielenie na sekcje
-- -------------------
-- -- === Terms === --
-- -------------------
--
--
-- -- Pattern matching
-- data Clause a = Clause { _pattern :: !a
--                        , _result  :: !a
--                        } deriving (Show, Eq, Functor, Foldable, Traversable)
-- -- FIXME [WD]: we should pattern match against two things: (Cons with vars) and Literals. Everything else is not "core"
--
-- -- Core
-- newtype TermNumber    a = Number    { __val  :: Literal.Number                } deriving (Show, Eq, Functor, Foldable, Traversable)
-- newtype TermString    a = String    { __val  :: Literal.String                } deriving (Show, Eq, Functor, Foldable, Traversable)
-- newtype TermVar       a = Var       { __name :: Name                          } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermApp       a = App       { __base :: !a , __arg     :: !a          } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermLam       a = Lam       { __arg  :: !a , __body    :: !a          } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermMatch     a = Match     { __arg  :: !a , __clauses :: ![a] } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermAcc       a = Acc       { __base :: !a , __name    :: !Name       } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermSeq       a = Seq       { __left :: !a , __right   :: !a          } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermUnify     a = Unify     { __left :: !a , __right   :: !a          } deriving (Show, Eq, Functor, Foldable, Traversable) -- FIXME[WD]: is that core?
-- newtype TermCons      a = Cons      { __name :: !Name               } deriving (Show, Eq, Functor, Foldable, Traversable) -- FIXME[WD]: can we remove it and leave `Var` only or is it just "sugar"?
--
-- -- Sugar
-- newtype TermFmtString a = FmtString { __val  :: Literal.FmtString a } deriving (Show, Eq, Functor, Foldable, Traversable)
-- newtype TermGrouped   a = Grouped   { __base :: a                   } deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermBlank     a = Blank   deriving (Show, Eq, Functor, Foldable, Traversable)
-- data    TermMissing   a = Missing deriving (Show, Eq, Functor, Foldable, Traversable) -- FIXME[WD]: remove
-- data    TermStar      a = Star    deriving (Show, Eq, Functor, Foldable, Traversable) -- FIXME[WD]: Depraciated, we should use `Cons "Star"`` instead
--
--
--
--
--
--
--
-- -- NOTE: IR Pattern moze byc w grafie nawet jezeli nie nalezy do Draft (!). Wystarczy ze rodzice maja na niego wskazniki otypowane.
--
-- -------------------
-- -- === Terms === --
-- -------------------
--
-- -- === Pattern matching === --
--
-- data Pattern a t = DataPattern    { _name :: !Name.Path, _args :: ![a ~> t] }
--                  | LiteralPattern { _lit  :: !(a ~> Expr Literal)           }
--                  | WildPattern
--
-- data Clause   t a = Clause { _pattern :: !p , _result :: !a } deriving (Show, Eq, Functor, Foldable, Traversable)
-- type CoreClause a = Clause (Pattern a (Expr Var)) a
--
--
-- -- === Core === --
--
-- newtype TermNumber    a = Number    { __val  :: Literal.Number                    } deriving (Show, Eq)
-- newtype TermString    a = String    { __val  :: Literal.String                    } deriving (Show, Eq)
-- newtype TermVar       a = Var       { __name :: Name                              } deriving (Show, Eq)
-- data    TermApp       a = App       { __base :: !a , __arg     :: !a              } deriving (Show, Eq)
-- data    TermLam       a = Lam       { __arg  :: !a , __body    :: !a              } deriving (Show, Eq)
-- data    TermCase      a = Case      { __arg  :: !a , __clauses :: ![CoreClause a] } deriving (Show, Eq)
-- data    TermAcc       a = Acc       { __base :: !a , __name    :: !Name           } deriving (Show, Eq)
-- data    TermSeq       a = Seq       { __left :: !a , __right   :: !a              } deriving (Show, Eq)
-- data    TermUnify     a = Unify     { __left :: !a , __right   :: !a              } deriving (Show, Eq) -- FIXME[WD]: is that core?
--
-- -- data    TermCase      a = Case      { __arg  :: !a   , __clauses :: ![CoreClause a] } deriving (Show, Eq)
--
-- -- === Sugar === --
--
-- newtype SugaredClause      a = SugaredClause (Clause (a ~> SugaredPattern) a)
-- newtype TermSugaredPattern a = VarPattern !(a ~> Expr Var)
--                              | Pattern    !(Pattern (a ~> SugaredPattern))
--
-- newtype TermCons      a = Cons      { __name :: Name                                 } deriving (Show, Eq) -- FIXME[WD]: can we remove it and leave `Var` only or is it just "sugar"?
-- newtype TermFmtString a = FmtString { __val  :: Literal.FmtString a                  } deriving (Show, Eq)
-- data    TermMatch     a = Match     { __arg  :: !a , __matches :: ![SugaredClause a] } deriving (Show, Eq)
-- newtype TermGrouped   a = Grouped   { __base :: a                                    } deriving (Show, Eq)
-- data    TermBlank     a = Blank                                                        deriving (Show, Eq)
--
--
-- -- === Incomplete === --
--
-- data TermMissing a = Missing deriving (Show, Eq)
--
--
-- -- -- Depreciated
-- -- data    TermStar      a = Star                                                        deriving (Show, Eq) -- FIXME[WD]: We should use `Cons "Star"`` instead
--
--
--
--
-- -- NOTE: Skoro nie oczekujemy by elementy byly funktorami, czy parametryzacja sztuczna elementow jest potrzebna?
-- --       TermDef type familia na tym działa, tylko czy obecnie jej to coś daje? Czy po zmianach można to usunąć?
-- --       To think about it.
--
--
-- -------------------
-- -- === Terms === --
-- -------------------
--
-- -- === Pattern matching === --
--
-- data Pattern p = DataPattern    { _name :: !Name.Path, _args :: ![Ptr p] }
--                | LiteralPattern { _lit  :: !(Ptr Literal)                }
--                | WildPattern
--
-- data Clause   p a = Clause { _pattern :: !p, _result :: !(Ptr a) } deriving (Show, Eq)
-- type CoreClause a = Clause (Pattern Var) a
--
--
-- -- === Core === --
--
-- newtype Term_Number    a = Number    { __val  :: Literal.Number                            } deriving (Show, Eq)
-- newtype Term_String    a = String    { __val  :: Literal.String                            } deriving (Show, Eq)
-- newtype Term_Var       a = Var       { __name :: Name                                      } deriving (Show, Eq)
-- newtype Term_Cons      a = Cons      { __name :: Name       , __fields  :: ![Ptr Value]    } deriving (Show, Eq)
-- data    Term_Acc       a = Acc       { __base :: !(Ptr a)   , __name    :: !Name           } deriving (Show, Eq)
-- data    Term_App       a = App       { __base :: !(Ptr a)   , __arg     :: !(Ptr a)        } deriving (Show, Eq)
-- data    Term_Lam       a = Lam       { __arg  :: !(Ptr Var) , __body    :: !(Ptr a)        } deriving (Show, Eq)
-- data    Term_Seq       a = Seq       { __left :: !(Ptr a)   , __right   :: !(Ptr a)        } deriving (Show, Eq)
-- data    Term_Unify     a = Unify     { __left :: !(Ptr a)   , __right   :: !(Ptr a)        } deriving (Show, Eq) -- FIXME[WD]: is that core?
-- data    Term_Match     a = Match     { __arg  :: !(Ptr a)   , __clauses :: ![CoreClause a] } deriving (Show, Eq)
--
--
-- -- FIXME[WD]: Is this textual - only sugar?
-- -- === Sugar === --
--
-- newtype SugaredClause       a = SugaredClause (Clause (Ptr SugaredPattern) a) -- FIXME FIXME FIXME: podwojny skok Ptr (!) Ptr SugaredPattern -> Ptr Var -> Name
-- newtype Term_SugaredPattern a = VarPattern !(Ptr Var)
--                               | Pattern    !(Pattern SugaredPattern)
--
-- newtype Term_FmtString a = FmtString { __val  :: Literal.FmtString a                        } deriving (Show, Eq)
-- data    Term_Lam       a = Lam       { __args :: ![Ptr SugaredPattern] , __body :: !(Ptr a) } deriving (Show, Eq)
-- data    Term_Match     a = Match     { __arg  :: !a , __matches :: ![SugaredClause a]       } deriving (Show, Eq)
-- newtype Term_Grouped   a = Grouped   { __base :: a                                          } deriving (Show, Eq)
-- data    Term_Blank     a = Blank                                                              deriving (Show, Eq)
--
--
-- -- -- Depreciated
-- -- data Term_Missing a = Missing deriving (Show, Eq, Functor, Foldable, Traversable)
-- -- data    TermStar      a = Star                                                        deriving (Show, Eq, Functor, Foldable, Traversable) -- FIXME[WD]: We should use `Cons "Star"`` instead
--
--
--
--
--
-- -------------------
-- -- === Terms === --
-- -------------------
--
-- -- === Pattern matching === --
--
-- data Clause p a = Clause { _pattern :: !(Ptr p), _result :: !(Ptr a) } deriving (Show, Eq)
--
-- type Pattern      = Format '[Cons, Literal, Wildcard]
-- type CoreClause a = Clause (Pattern >> Var) a
--
--
-- -- === Core === --
--
-- newtype Term_Number    a = Number    { __val  :: Literal.Number                            } deriving (Show, Eq)
-- newtype Term_String    a = String    { __val  :: Literal.String                            } deriving (Show, Eq)
-- newtype Term_Var       a = Var       { __name :: Name                                      } deriving (Show, Eq)
-- newtype Term_Cons      a = Cons      { __name :: Name       , __fields  :: ![Ptr Value]    } deriving (Show, Eq)
-- data    Term_Acc       a = Acc       { __base :: !(Ptr a)   , __name    :: !Name           } deriving (Show, Eq)
-- data    Term_App       a = App       { __base :: !(Ptr a)   , __arg     :: !(Ptr a)        } deriving (Show, Eq)
-- data    Term_Lam       a = Lam       { __arg  :: !(Ptr Var) , __body    :: !(Ptr a)        } deriving (Show, Eq)
-- data    Term_Seq       a = Seq       { __left :: !(Ptr a)   , __right   :: !(Ptr a)        } deriving (Show, Eq)
-- data    Term_Unify     a = Unify     { __left :: !(Ptr a)   , __right   :: !(Ptr a)        } deriving (Show, Eq)
-- data    Term_Match     a = Match     { __arg  :: !(Ptr a)   , __clauses :: ![CoreClause a] } deriving (Show, Eq)
--
--
--
-- -- === Sugar === --
--
-- data    Term_Wildcard a = Wildcard  deriving (Show, Eq)
-- newtype Term_Grouped  a = Grouped a deriving (Show, Eq)
--
--
-- -- === Textual (?) Sugar === --
-- -- qualified Module Sugared ?
--
-- type SugaredPattern = Format '[SugaredCons, Var, Literal, Wildcard]
-- type SugaredClause  = Clause SugaredPattern
--
-- newtype Term_FmtString    a = FmtString    { __val  :: Literal.FmtString a                                        } deriving (Show, Eq)
-- data    Term_SugaredLam   a = SugaredLam   { __args :: ![Ptr SugaredPattern] , __body    :: !(Ptr a)              } deriving (Show, Eq)
-- data    Term_SugaredMatch a = SugaredMatch { __arg  :: !a                    , __matches :: ![SugaredClause a]    } deriving (Show, Eq)
-- data    Term_SugaredCons  a = SugaredCons  { _name  :: !Name.Path            , _args     :: ![Ptr SugaredPattern] } deriving (Show, Eq)
--
--
-- -- FIXME[WD]: Depreciated
-- -- data Term_Missing -- NOTE: Not used anymore
-- -- data Term_Star    -- NOTE: We should use `Cons "Star"`` instead
