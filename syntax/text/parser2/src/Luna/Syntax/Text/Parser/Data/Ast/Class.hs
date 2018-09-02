{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Parser.Data.Ast.Class where

import Prologue hiding (Text, span)

import qualified Data.Text32                           as Text
import qualified Luna.IR.Term.Ast.Invalid              as Invalid
import qualified Luna.Syntax.Text.Parser.Data.Ast.Atom as Atom
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan as CodeSpan

import Data.Text.Position                    (Delta)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan (CodeSpan))
import OCI.Data.Name                         (Name)


type Text = Text.Text32



data Spanned a = Spanned
    { _span :: CodeSpan
    , _ast  :: a
    } deriving (Eq, Functor, Ord, Show)
makeLenses ''Spanned

instance Convertible (Spanned a) a where
    convert = view ast
    {-# INLINE convert #-}


unsafeUnspan :: Spanned a -> a
unsafeUnspan = \(Spanned _ a) -> a
{-# INLINE unsafeUnspan #-}


prependAsOffset :: Spanned a -> (Spanned b -> Spanned b)
prependAsOffset = \t -> span %~ (CodeSpan.prependAsOffset (t ^. span))
{-# INLINE prependAsOffset #-}

prependOffset :: Spanned a -> (Spanned b -> Spanned b)
prependOffset = \t -> span %~ (CodeSpan.prependAsOffset $ CodeSpan.dropLength (t ^. span))
{-# INLINE prependOffset #-}

prependOffset' :: CodeSpan -> (Spanned b -> Spanned b)
prependOffset' = \t -> span %~ (CodeSpan.prependAsOffset $ CodeSpan.dropLength t)
{-# INLINE prependOffset' #-}


-- TODO
-- Generate with TH
data Ast
    = AstVar          (Atom.Var          Ast)
    | AstCons         (Atom.Cons         Ast)
    | AstOperator     (Atom.Operator     Ast)
    | AstModifier     (Atom.Modifier     Ast)
    | AstWildcard     (Atom.Wildcard     Ast)
    | AstNumber       (Atom.Number       Ast)
    | AstStr          (Atom.Str          Ast)
    | AstBlock        (Atom.Block        Ast)
    | AstMarker       (Atom.Marker       Ast)
    | AstLineBreak    (Atom.LineBreak    Ast)
    | AstComment      (Atom.Comment      Ast)
    | AstDocumented   (Atom.Documented   Ast)
    | AstInvalid      (Atom.Invalid      Ast)
    | AstApp          (Atom.App          Ast)
    | AstInfixApp     (Atom.InfixApp     Ast)
    | AstMissing      (Atom.Missing      Ast)
    | AstList         (Atom.List         Ast)
    | AstUnit         (Atom.Unit         Ast)
    | AstSectionLeft  (Atom.SectionLeft  Ast)
    | AstSectionRight (Atom.SectionRight Ast)
    deriving (Eq, Ord, Show)


-- TODO
-- Generate with TH
pattern Var          t1       = AstVar          (Atom.Atom_Var          t1      )
pattern Cons         t1       = AstCons         (Atom.Atom_Cons         t1      )
pattern Operator     t1       = AstOperator     (Atom.Atom_Operator     t1      )
pattern Modifier     t1       = AstModifier     (Atom.Atom_Modifier     t1      )
pattern Wildcard              = AstWildcard     (Atom.Atom_Wildcard             )
pattern Number       t1       = AstNumber       (Atom.Atom_Number       t1      )
pattern Str          t1       = AstStr          (Atom.Atom_Str          t1      )
pattern Block        t1       = AstBlock        (Atom.Atom_Block        t1      )
pattern Marker       t1       = AstMarker       (Atom.Atom_Marker       t1      )
pattern LineBreak    t1       = AstLineBreak    (Atom.Atom_LineBreak    t1      )
pattern Comment      t1       = AstComment      (Atom.Atom_Comment      t1      )
pattern Documented   t1 t2    = AstDocumented   (Atom.Atom_Documented   t1 t2   )
pattern Invalid      t1       = AstInvalid      (Atom.Atom_Invalid      t1      )
pattern App          t1 t2    = AstApp          (Atom.Atom_App          t1 t2   )
pattern InfixApp     t1 t2 t3 = AstInfixApp     (Atom.Atom_InfixApp     t1 t2 t3)
pattern Missing               = AstMissing      (Atom.Atom_Missing              )
pattern List         t1       = AstList         (Atom.Atom_List         t1      )
pattern Unit         t1       = AstUnit         (Atom.Atom_Unit         t1      )
pattern SectionLeft  t1 t2    = AstSectionLeft  (Atom.Atom_SectionLeft  t1 t2   )
pattern SectionRight t1 t2    = AstSectionRight (Atom.Atom_SectionRight t1 t2   )




type instance Atom.Link Ast (Atom.StrChunk s) = Spanned (Atom.StrChunk s)
type instance Atom.Link Ast Atom.Struct = Spanned Ast
