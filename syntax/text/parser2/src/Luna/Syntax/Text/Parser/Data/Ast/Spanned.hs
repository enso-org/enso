{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Parser.Data.Ast.Spanned where

import Prologue hiding (Text, span)

import qualified Data.Text32                            as Text
import qualified Luna.IR.Term.Ast.Invalid               as Invalid
import qualified Luna.Syntax.Text.Parser.Data.Ast.Class as Ast
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan  as CodeSpan

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
    = AstVar          (Ast.Var          Ast)
    | AstCons         (Ast.Cons         Ast)
    | AstOperator     (Ast.Operator     Ast)
    | AstModifier     (Ast.Modifier     Ast)
    | AstWildcard     (Ast.Wildcard     Ast)
    | AstNumber       (Ast.Number       Ast)
    | AstStr          (Ast.Str          Ast)
    | AstBlock        (Ast.Block        Ast)
    | AstMarker       (Ast.Marker       Ast)
    | AstLineBreak    (Ast.LineBreak    Ast)
    | AstComment      (Ast.Comment      Ast)
    | AstDocumented   (Ast.Documented   Ast)
    | AstInvalid      (Ast.Invalid      Ast)
    | AstApp          (Ast.App          Ast)
    | AstInfixApp     (Ast.InfixApp     Ast)
    | AstMissing      (Ast.Missing      Ast)
    | AstList         (Ast.List         Ast)
    | AstUnit         (Ast.Unit         Ast)
    | AstSectionLeft  (Ast.SectionLeft  Ast)
    | AstSectionRight (Ast.SectionRight Ast)
    deriving (Eq, Ord, Show)


-- TODO
-- Generate with TH
pattern Var          t1       = AstVar          (Ast.Var          t1      )
pattern Cons         t1       = AstCons         (Ast.Cons         t1      )
pattern Operator     t1       = AstOperator     (Ast.Operator     t1      )
pattern Modifier     t1       = AstModifier     (Ast.Modifier     t1      )
pattern Wildcard              = AstWildcard     (Ast.Wildcard             )
pattern Number       t1       = AstNumber       (Ast.Number       t1      )
pattern Str          t1       = AstStr          (Ast.Str          t1      )
pattern Block        t1       = AstBlock        (Ast.Block        t1      )
pattern Marker       t1       = AstMarker       (Ast.Marker       t1      )
pattern LineBreak    t1       = AstLineBreak    (Ast.LineBreak    t1      )
pattern Comment      t1       = AstComment      (Ast.Comment      t1      )
pattern Documented   t1 t2    = AstDocumented   (Ast.Documented   t1 t2   )
pattern Invalid      t1       = AstInvalid      (Ast.Invalid      t1      )
pattern App          t1 t2    = AstApp          (Ast.App          t1 t2   )
pattern InfixApp     t1 t2 t3 = AstInfixApp     (Ast.InfixApp     t1 t2 t3)
pattern Missing               = AstMissing      (Ast.Missing              )
pattern List         t1       = AstList         (Ast.List         t1      )
pattern Unit         t1       = AstUnit         (Ast.Unit         t1      )
pattern SectionLeft  t1 t2    = AstSectionLeft  (Ast.SectionLeft  t1 t2   )
pattern SectionRight t1 t2    = AstSectionRight (Ast.SectionRight t1 t2   )




type instance Ast.Link Ast (Ast.StrChunk s) = Spanned (Ast.StrChunk s)
type instance Ast.Link Ast Ast.Struct = Spanned Ast
