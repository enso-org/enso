{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Parser.Ast.Class where

import Prologue hiding (Text, span)

import qualified Data.Text32                          as Text
import qualified Luna.IR.Term.Ast.Invalid             as Invalid
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan as CodeSpan

import Data.Text.Position                   (Delta)
import Luna.Syntax.Text.Parser.Ast.CodeSpan (CodeSpan (CodeSpan))
import OCI.Data.Name                        (Name)



-------------------
-- === Types === --
-------------------

type Text = Text.Text32



----------------------------
-- === Ast components === --
----------------------------

-- === Links === --

data Struct
type family Link t a
type Ln t = Link t Struct


-- === Definition === --

-- Identifiers
data Var          t = Var          { name   :: Name                            }
data Cons         t = Cons         { name   :: Name                            }
data Operator     t = Operator     { name   :: Name                            }
data Modifier     t = Modifier     { name   :: Name                            }
data Wildcard     t = Wildcard

-- Literals
data Number       t = Number       { digits :: NonEmpty Word8                  }
data Str          t = Str          { chunks :: [Link t (StrChunk t)]           }

-- Layouting
data Block        t = Block        { lines1 :: NonEmpty (Ln t)                 }
data Tokens       t = Tokens       { lines  :: [Ln t]                          }
data Marker       t = Marker       { mID    :: Int                             }
data LineBreak    t = LineBreak    { indent :: Delta                           }

-- Docs
data Comment      t = Comment      { text   :: Text                            }
data Documented   t = Documented   { doc    :: Ln t, base :: Ln t              }
data Metadata     t = Metadata     { text   :: Text                            }

-- Errors
data Invalid      t = Invalid      { desc   :: Invalid.Symbol                  }

-- Exprs
data App          t = App          { fn     :: Ln t, arg :: Ln t               }
data InfixApp     t = InfixApp     { argl   :: Ln t, fn  :: Ln t, argr :: Ln t }
data SectionLeft  t = SectionLeft  { arg    :: Ln t, fn  :: Ln t               }
data SectionRight t = SectionRight { fn     :: Ln t, arg :: Ln t               }
data Missing      t = Missing
data List         t = List         { items  :: [Ln t]                          }
data Unit         t = Unit         { body   :: Ln t                            }

data StrChunk t
    = StrPlain   Text
    | StrNewLine (LineBreak t)


-- === Instances === --

-- TODO
-- Generate with TH
instance Show (Ln t) => Show (Var          t) where show  (Var          t1      ) = "Var"          <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Cons         t) where show  (Cons         t1      ) = "Cons"         <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Operator     t) where show  (Operator     t1      ) = "Operator"     <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Modifier     t) where show  (Modifier     t1      ) = "Modifier"     <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Wildcard     t) where show  (Wildcard             ) = "Wildcard"
instance Show (Ln t) => Show (Number       t) where show  (Number       t1      ) = "Number"       <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Block        t) where show  (Block        t1      ) = "Block"        <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Tokens       t) where show  (Tokens       t1      ) = "Tokens"       <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Marker       t) where show  (Marker       t1      ) = "Marker"       <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (LineBreak    t) where show  (LineBreak    t1      ) = "LineBreak"    <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Comment      t) where show  (Comment      t1      ) = "Comment"      <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Documented   t) where show  (Documented   t1 t2   ) = "Documented"   <> " (" <> show t1 <> ") (" <> show t2 <> ")"
instance Show (Ln t) => Show (Metadata     t) where show  (Metadata     t1      ) = "Metadata"     <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Invalid      t) where show  (Invalid      t1      ) = "Invalid"      <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (App          t) where show  (App          t1 t2   ) = "App"          <> " (" <> show t1 <> ") (" <> show t2 <> ")"
instance Show (Ln t) => Show (InfixApp     t) where show  (InfixApp     t1 t2 t3) = "InfixApp"     <> " (" <> show t1 <> ") (" <> show t2 <> ") (" <> show t3 <> ")"
instance Show (Ln t) => Show (Missing      t) where show  (Missing              ) = "Missing"
instance Show (Ln t) => Show (List         t) where show  (List         t1      ) = "List"         <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Unit         t) where show  (Unit         t1      ) = "Unit"         <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (SectionLeft  t) where show  (SectionLeft  t1 t2   ) = "SectionLeft"  <> " (" <> show t1 <> ") (" <> show t2 <> ")"
instance Show (Ln t) => Show (SectionRight t) where show  (SectionRight t1 t2   ) = "SectionRight" <> " (" <> show t1 <> ") (" <> show t2 <> ")"
instance (Show (Ln t), Show (Link t (StrChunk t))) => Show (Str t) where show (Str t1) = "Str"     <> " (" <> show t1 <> ")"

-- TODO
-- Generate with TH
deriving instance Eq (Ln t) => Eq (Var          t)
deriving instance Eq (Ln t) => Eq (Cons         t)
deriving instance Eq (Ln t) => Eq (Operator     t)
deriving instance Eq (Ln t) => Eq (Modifier     t)
deriving instance Eq (Ln t) => Eq (Wildcard     t)
deriving instance Eq (Ln t) => Eq (Number       t)
deriving instance Eq (Ln t) => Eq (Block        t)
deriving instance Eq (Ln t) => Eq (Tokens       t)
deriving instance Eq (Ln t) => Eq (Marker       t)
deriving instance Eq (Ln t) => Eq (LineBreak    t)
deriving instance Eq (Ln t) => Eq (Comment      t)
deriving instance Eq (Ln t) => Eq (Documented   t)
deriving instance Eq (Ln t) => Eq (Metadata     t)
deriving instance Eq (Ln t) => Eq (Invalid      t)
deriving instance Eq (Ln t) => Eq (App          t)
deriving instance Eq (Ln t) => Eq (InfixApp     t)
deriving instance Eq (Ln t) => Eq (Missing      t)
deriving instance Eq (Ln t) => Eq (List         t)
deriving instance Eq (Ln t) => Eq (Unit         t)
deriving instance Eq (Ln t) => Eq (SectionLeft  t)
deriving instance Eq (Ln t) => Eq (SectionRight t)
deriving instance Eq (Ln t) => Eq (StrChunk     t)
deriving instance (Eq (Ln t), Eq (Link t (StrChunk t))) => Eq (Str t)
deriving instance Show (Ln t) => Show (StrChunk  t)

deriving instance Ord (Ln t) => Ord (Var          t)
deriving instance Ord (Ln t) => Ord (Cons         t)
deriving instance Ord (Ln t) => Ord (Operator     t)
deriving instance Ord (Ln t) => Ord (Modifier     t)
deriving instance Ord (Ln t) => Ord (Wildcard     t)
deriving instance Ord (Ln t) => Ord (Number       t)
deriving instance Ord (Ln t) => Ord (Block        t)
deriving instance Ord (Ln t) => Ord (Tokens       t)
deriving instance Ord (Ln t) => Ord (Marker       t)
deriving instance Ord (Ln t) => Ord (LineBreak    t)
deriving instance Ord (Ln t) => Ord (Comment      t)
deriving instance Ord (Ln t) => Ord (Documented   t)
deriving instance Ord (Ln t) => Ord (Metadata     t)
deriving instance Ord (Ln t) => Ord (Invalid      t)
deriving instance Ord (Ln t) => Ord (App          t)
deriving instance Ord (Ln t) => Ord (InfixApp     t)
deriving instance Ord (Ln t) => Ord (Missing      t)
deriving instance Ord (Ln t) => Ord (List         t)
deriving instance Ord (Ln t) => Ord (Unit         t)
deriving instance Ord (Ln t) => Ord (SectionLeft  t)
deriving instance Ord (Ln t) => Ord (SectionRight t)
deriving instance Ord (Ln t) => Ord (StrChunk     t)
deriving instance (Ord (Ln t), Ord (Link t (StrChunk t))) => Ord (Str t)

instance Convertible String (StrChunk t) where
    convert = StrPlain . convert
    {-# INLINE convert #-}

instance IsString (StrChunk t) where
    fromString = convert
    {-# INLINE fromString #-}
