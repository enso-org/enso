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

import Data.Property hiding (Update)
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

data    TermInvalid   a = Invalid { __desc  :: Text32                              } deriving (Show, Eq, Functor, Foldable, Traversable) -- TODO: Text -> Doc
data    TermList      a = List    { __items :: ![a]                                } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermTuple     a = Tuple   { __items :: ![a]                                } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermAccSection   a = AccSection   { __name     :: ![Name]                  } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermLeftSection  a = LeftSection  { __operator :: !a   , __body :: !a      } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermRightSection a = RightSection { __operator :: !a   , __body :: !a      } deriving (Show, Eq, Functor, Foldable, Traversable)
newtype TermDisabled     a = Disabled     { __body     :: a                        } deriving (Show, Eq, Functor, Foldable, Traversable)
newtype TermMarker       a = Marker       { __markerId :: Word64                   } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermMarked       a = Marked       { __marker   :: !a   , __body :: !a      } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermDocumented   a = Documented   { __doc      :: !Text32, __base :: !a    } deriving (Show, Eq, Functor, Foldable, Traversable)
newtype TermMetadata     a = Metadata     { __content  :: Text32                   } deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermUpdate       a = Update       { __base     :: !a , __names :: ![Name] , __value :: !a }                      deriving (Show, Eq, Functor, Foldable, Traversable)
data    TermModify       a = Modify       { __base     :: !a , __names :: ![Name] , __operator :: !Name, __value :: !a } deriving (Show, Eq, Functor, Foldable, Traversable)

makeLensedTerms "CoreTerms" [ ''TermNumber, ''TermString, ''TermFmtString, ''TermAcc, ''TermApp, ''TermLam, ''TermSeq, ''TermUnify
                            , ''TermCons, ''TermMatch, ''TermMonadic, ''TermVar, ''TermFieldLens, ''TermGrouped, ''TermBlank, ''TermStar, ''TermMissing, ''TermClsASG
                            , ''TermRecASG, ''TermFieldASG, ''TermTyped, ''TermInvalid, ''TermList, ''TermTuple, ''TermLeftSection, ''TermRightSection, ''TermAccSection
                            , ''TermDisabled, ''TermMarker, ''TermMarked, ''TermDocumented, ''TermMetadata, ''TermUpdate, ''TermModify
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
