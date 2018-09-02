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
pattern Var          t1       = AstVar          (Atom.Var          t1      )
pattern Cons         t1       = AstCons         (Atom.Cons         t1      )
pattern Operator     t1       = AstOperator     (Atom.Operator     t1      )
pattern Modifier     t1       = AstModifier     (Atom.Modifier     t1      )
pattern Wildcard              = AstWildcard     (Atom.Wildcard             )
pattern Number       t1       = AstNumber       (Atom.Number       t1      )
pattern Str          t1       = AstStr          (Atom.Str          t1      )
pattern Block        t1       = AstBlock        (Atom.Block        t1      )
pattern Marker       t1       = AstMarker       (Atom.Marker       t1      )
pattern LineBreak    t1       = AstLineBreak    (Atom.LineBreak    t1      )
pattern Comment      t1       = AstComment      (Atom.Comment      t1      )
pattern Documented   t1 t2    = AstDocumented   (Atom.Documented   t1 t2   )
pattern Invalid      t1       = AstInvalid      (Atom.Invalid      t1      )
pattern App          t1 t2    = AstApp          (Atom.App          t1 t2   )
pattern InfixApp     t1 t2 t3 = AstInfixApp     (Atom.InfixApp     t1 t2 t3)
pattern Missing               = AstMissing      (Atom.Missing              )
pattern List         t1       = AstList         (Atom.List         t1      )
pattern Unit         t1       = AstUnit         (Atom.Unit         t1      )
pattern SectionLeft  t1 t2    = AstSectionLeft  (Atom.SectionLeft  t1 t2   )
pattern SectionRight t1 t2    = AstSectionRight (Atom.SectionRight t1 t2   )




type instance Atom.Link Ast (Atom.StrChunk s) = Spanned (Atom.StrChunk s)
type instance Atom.Link Ast Atom.Struct = Spanned Ast
