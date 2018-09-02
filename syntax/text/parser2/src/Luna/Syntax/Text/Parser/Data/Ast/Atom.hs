{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Parser.Data.Ast.Atom where

import Prologue hiding (Text, span)

import qualified Data.Text32                           as Text
import qualified Luna.IR.Term.Ast.Invalid              as Invalid
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan as CodeSpan

import Data.Text.Position                    (Delta)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan (CodeSpan))
import OCI.Data.Name                         (Name)


type Text = Text.Text32


data Struct
type family Link t a
type Ln t = Link t Struct

-- Identifiers
data Var          t = Atom_Var          { name   :: Name                       }
data Cons         t = Atom_Cons         { name   :: Name                       }
data Operator     t = Atom_Operator     { name   :: Name                       }
data Modifier     t = Atom_Modifier     { name   :: Name                       }
data Wildcard     t = Atom_Wildcard

-- Literals
data Number       t = Atom_Number       { digits :: NonEmpty Word8             }
data Str          t = Atom_Str          { chunks :: [Link t (StrChunk t)]      }

-- Layouting
data Block        t = Atom_Block        { lines1 :: NonEmpty (Ln t)            }
data Tokens       t = Atom_Tokens       { lines  :: [Ln t]                     }
data Marker       t = Atom_Marker       { mID    :: Int                        }
data LineBreak    t = Atom_LineBreak    { indent :: Delta                      }

-- Docs
data Comment      t = Atom_Comment      { text   :: Text                       }
data Documented   t = Atom_Documented   { doc    :: Ln t, base :: Ln t         }

-- Errors
data Invalid      t = Atom_Invalid      { desc   :: Invalid.Symbol             }

-- Exprs
data App          t = Atom_App          { fn  :: Ln t, ar :: Ln t              }
data InfixApp     t = Atom_InfixApp     { arl :: Ln t, fn :: Ln t, arr :: Ln t }
data SectionLeft  t = Atom_SectionLeft  { ar  :: Ln t, fn :: Ln t              }
data SectionRight t = Atom_SectionRight { fn  :: Ln t, ar :: Ln t              }
data Missing      t = Atom_Missing
data List         t = Atom_List         { items :: [Ln t]                      }
data Unit         t = Atom_Unit         { body  :: Ln t                        }

data StrChunk t
    = StrPlain   Text
    | StrNewLine (LineBreak t)


-- TODO
-- Generate with TH
instance Show (Ln t) => Show (Var          t) where show  (Atom_Var          t1      ) = "Var"          <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Cons         t) where show  (Atom_Cons         t1      ) = "Cons"         <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Operator     t) where show  (Atom_Operator     t1      ) = "Operator"     <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Modifier     t) where show  (Atom_Modifier     t1      ) = "Modifier"     <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Wildcard     t) where show  (Atom_Wildcard             ) = "Wildcard"
instance Show (Ln t) => Show (Number       t) where show  (Atom_Number       t1      ) = "Number"       <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Block        t) where show  (Atom_Block        t1      ) = "Block"        <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Tokens       t) where show  (Atom_Tokens       t1      ) = "Tokens"       <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Marker       t) where show  (Atom_Marker       t1      ) = "Marker"       <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (LineBreak    t) where show  (Atom_LineBreak    t1      ) = "LineBreak"    <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Comment      t) where show  (Atom_Comment      t1      ) = "Comment"      <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Documented   t) where show  (Atom_Documented   t1 t2   ) = "Documented"   <> " (" <> show t1 <> ") (" <> show t2 <> ")"
instance Show (Ln t) => Show (Invalid      t) where show  (Atom_Invalid      t1      ) = "Invalid"      <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (App          t) where show  (Atom_App          t1 t2   ) = "App"          <> " (" <> show t1 <> ") (" <> show t2 <> ")"
instance Show (Ln t) => Show (InfixApp     t) where show  (Atom_InfixApp     t1 t2 t3) = "InfixApp"     <> " (" <> show t1 <> ") (" <> show t2 <> ") (" <> show t3 <> ")"
instance Show (Ln t) => Show (Missing      t) where show  (Atom_Missing              ) = "Missing"
instance Show (Ln t) => Show (List         t) where show  (Atom_List         t1      ) = "List"         <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (Unit         t) where show  (Atom_Unit         t1      ) = "Unit"         <> " (" <> show t1 <> ")"
instance Show (Ln t) => Show (SectionLeft  t) where show  (Atom_SectionLeft  t1 t2   ) = "SectionLeft"  <> " (" <> show t1 <> ") (" <> show t2 <> ")"
instance Show (Ln t) => Show (SectionRight t) where show  (Atom_SectionRight t1 t2   ) = "SectionRight" <> " (" <> show t1 <> ") (" <> show t2 <> ")"
instance (Show (Ln t), Show (Link t (StrChunk t))) => Show (Str t) where show (Atom_Str t1) = "Str"     <> " (" <> show t1 <> ")"



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


