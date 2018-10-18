{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Parser.Ast.Simple where

import Prologue hiding (Text, imp, seq, some, span, takeWhile)

import qualified Data.Char                           as Char
import qualified GHC.Exts                            as GHC
import qualified Luna.IR.Term.Ast.Invalid            as Invalid
import qualified Luna.Syntax.Text.Parser.Ast.Class   as Ast
import qualified Luna.Syntax.Text.Parser.Ast.Spanned as Spanned

import Data.Text.Position                  (Delta)
import Luna.Syntax.Text.Parser.Ast.Class   (Text)
import Luna.Syntax.Text.Parser.Ast.Spanned (Spanned)
import OCI.Data.Name                       (Name)



------------------------
-- === Simple Ast === --
------------------------

-- | Simple AST is similar to the one we're getting from parser but it does not
--   have code span information and has an associated DSL for simple building
--   for the needs of the tests.
--
--   Please note that the DSL is created to be easy to use, but it should not be
--   used in production code due to many implementation shortcuts like for
--   example the Num instance, allowing us to use operators but without any
--   implementation of some other functions from Num typeclass.


-- === Definition === --

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
    | AstMetadata     (Ast.Metadata     Ast)
    | AstInvalid      (Ast.Invalid      Ast)
    | AstApp          (Ast.App          Ast)
    | AstInfixApp     (Ast.InfixApp     Ast)
    | AstMissing      (Ast.Missing      Ast)
    | AstList         (Ast.List         Ast)
    | AstUnit         (Ast.Unit         Ast)
    | AstSectionLeft  (Ast.SectionLeft  Ast)
    | AstSectionRight (Ast.SectionRight Ast)
    deriving (Eq, Ord)

type instance Ast.Link Ast (Ast.StrChunk s) = Ast.StrChunk s
type instance Ast.Link Ast Ast.Struct = Ast

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
pattern Metadata     t1       = AstMetadata     (Ast.Metadata     t1      )
pattern Invalid      t1       = AstInvalid      (Ast.Invalid      t1      )
pattern App          t1 t2    = AstApp          (Ast.App          t1 t2   )
pattern InfixApp     t1 t2 t3 = AstInfixApp     (Ast.InfixApp     t1 t2 t3)
pattern Missing               = AstMissing      (Ast.Missing              )
pattern List         t1       = AstList         (Ast.List         t1      )
pattern Unit         t1       = AstUnit         (Ast.Unit         t1      )
pattern SectionLeft  t1 t2    = AstSectionLeft  (Ast.SectionLeft  t1 t2   )
pattern SectionRight t1 t2    = AstSectionRight (Ast.SectionRight t1 t2   )


-- === Instances === --

instance Show Ast where
    show = \case
        AstVar          t -> show t
        AstCons         t -> show t
        AstOperator     t -> show t
        AstModifier     t -> show t
        AstWildcard     t -> show t
        AstNumber       t -> show t
        AstStr          t -> show t
        AstBlock        t -> show t
        AstMarker       t -> show t
        AstLineBreak    t -> show t
        AstComment      t -> show t
        AstDocumented   t -> show t
        AstMetadata     t -> show t
        AstInvalid      t -> show t
        AstApp          t -> show t
        AstInfixApp     t -> show t
        AstMissing      t -> show t
        AstList         t -> show t
        AstUnit         t -> show t
        AstSectionLeft  t -> show t
        AstSectionRight t -> show t


-- === Smart constructors === --

app  :: Ast -> Ast -> Ast
app2 :: Ast -> Ast -> Ast -> Ast
app3 :: Ast -> Ast -> Ast -> Ast -> Ast
app4 :: Ast -> Ast -> Ast -> Ast -> Ast -> Ast
app  = App
app2 = app .:  app
app3 = app .:. app2
app4 = app .:: app3
{-# INLINE app  #-}
{-# INLINE app2 #-}
{-# INLINE app3 #-}
{-# INLINE app4 #-}

apps :: Ast -> [Ast] -> Ast
apps = foldl' app
{-# INLINE apps #-}



-- === String to Ast conversion === --

instance IsString Ast where
    fromString = convert

instance Convertible String Ast where
    convert = let
        isMixfix = ('_' `elem`)
        in \case
            ""  -> Missing
            "_" -> Wildcard
            x@(s:ss) -> let n = convert x in if
                | s == '_'       -> Var  n
                | Char.isLower s -> Var  n
                | isMixfix x     -> Var  n
                | Char.isUpper s -> Cons n
                | Char.isDigit s -> Number $ charsToDigits (s :| ss)
                | otherwise      -> case last ss of
                    Just '=' -> Modifier $ convert (s : unsafeInit ss)
                    _        -> Operator n

instance Convertible String (t1 -> Ast)
      => IsString (t1 -> Ast) where
    fromString = convert

instance Convertible String (t1 -> t2 -> Ast)
      => IsString (t1 -> t2 -> Ast) where
    fromString = convert

instance Convertible String (t1 -> t2 -> t3 -> Ast)
      => IsString (t1 -> t2 -> t3 -> Ast) where
    fromString = convert

instance Convertible String (t1 -> t2 -> t3 -> t4 -> Ast)
      => IsString (t1 -> t2 -> t3 -> t4 -> Ast) where
    fromString = convert

charsToDigits :: NonEmpty Char -> NonEmpty Word8
charsToDigits (c :| cs) = go c :| (go <$> cs) where
    go c = fromIntegral $ Char.ord c - 48


-- === String to constructor conversion === --

instance Convertible Ast (t1 -> Ast)
      => Convertible String (t1 -> Ast) where
    convert = convert . convertTo @Ast

instance Convertible Ast (t1 -> t2 -> Ast)
      => Convertible String (t1 -> t2 -> Ast) where
    convert = convert . convertTo @Ast

instance Convertible Ast (t1 -> t2 -> t3 -> Ast)
      => Convertible String (t1 -> t2 -> t3 -> Ast) where
    convert = convert . convertTo @Ast

instance Convertible Ast (t1 -> t2 -> t3 -> t4 -> Ast)
      => Convertible String (t1 -> t2 -> t3 -> t4 -> Ast) where
    convert = convert . convertTo @Ast


-- === Ast to constructor conversion === --

instance t1 ~ Ast
      => Convertible Ast (t1 -> Ast) where
    convert = app

instance (t1 ~ Ast, t2 ~ Ast)
      => Convertible Ast (t1 -> t2 -> Ast) where
    convert = app2

instance (t1 ~ Ast, t2 ~ Ast, t3 ~ Ast)
      => Convertible Ast (t1 -> t2 -> t3 -> Ast) where
    convert = app3

instance (t1 ~ Ast, t2 ~ Ast, t3 ~ Ast, t4 ~ Ast)
      => Convertible Ast (t1 -> t2 -> t3 -> t4 -> Ast) where
    convert = app4


-- === Invalid symbols conversion === --

instance t1 ~ Ast
      => Convertible Invalid.Symbol (t1 -> Ast) where
    convert = convert . convertTo @Ast

instance (t1 ~ Ast, t2 ~ Ast)
      => Convertible Invalid.Symbol (t1 -> t2 -> Ast) where
    convert = convert . convertTo @Ast

instance Convertible Invalid.Symbol Ast where
    convert = Invalid


-- === Other conversions === --

instance {-# OVERLAPPABLE #-} Convertible' a Ast
      => Convertible [a] Ast where
    convert = List . fmap convert'

instance GHC.IsList Ast where
    type Item Ast = Ast
    fromList = convert

instance Num Ast where
    fromInteger = intToAst
    (-) = flip InfixApp (Operator "-")
    (+) = flip InfixApp (Operator "+")
    (*) = flip InfixApp (Operator "*")

intToAst :: Integral a => a -> Ast
intToAst a = if a < 0
    then undefined
    else Number $ intToDigits a

intToDigits :: Integral a => a -> NonEmpty Word8
intToDigits = go [] where
    go s x = loop (head :| s) tail where
        head = fromIntegral (x`mod` 10)
        tail = x `div` 10
    loop s@(r :| rs) = \case
        0 -> s
        x -> go (r : rs) x

instance a ~ Ast
      => Num (a -> Ast) where
    fromInteger i = App (fromInteger i)
--     (-) = extractOp (-) -- todo when needed
--     (+) = extractOp (+) -- todo when needed
--     (*) = extractOp (*) -- todo when needed



----------------------------
-- === Simplification === --
----------------------------

-- | Simplify defines conversion between two Ast forms, in particular it is
--   used to convert between spanned Ast and simple Ast.

-- === Definition === --

class Simplify a where
    type family Simplified a
    simplify :: a -> Simplified a

    type Simplified a = a
    default simplify :: Simplified a ~ a => a -> Simplified a
    simplify = id

instance Simplify Int
instance Simplify Word8
instance Simplify Word16
instance Simplify Word32
instance Simplify Word64
instance Simplify Invalid.Symbol
instance Simplify Text
instance Simplify Name
instance Simplify Delta

instance Simplify a
      => Simplify   (NonEmpty a) where
    type Simplified (NonEmpty a) = NonEmpty (Simplified a)
    simplify = fmap simplify

instance Simplify a
      => Simplify   (Spanned a) where
    type Simplified (Spanned a) = Simplified a
    simplify = simplify . Spanned.unsafeUnspan

instance Simplify a
      => Simplify   [a] where
    type Simplified [a] = [Simplified a]
    simplify = fmap simplify

instance Simplify (Ast.StrChunk Spanned.Ast) where
    type Simplified (Ast.StrChunk Spanned.Ast) = Ast.StrChunk Ast
    simplify = \case
        Ast.StrPlain   t                  -> Ast.StrPlain   t
        Ast.StrNewLine (Ast.LineBreak t) -> Ast.StrNewLine (Ast.LineBreak t)

instance Simplify   Spanned.Ast where
    type Simplified Spanned.Ast = Ast
    simplify = \case
        Spanned.Var          t1       -> Var          (simplify t1)
        Spanned.Cons         t1       -> Cons         (simplify t1)
        Spanned.Operator     t1       -> Operator     (simplify t1)
        Spanned.Modifier     t1       -> Modifier     (simplify t1)
        Spanned.Wildcard              -> Wildcard
        Spanned.Number       t1       -> Number       (simplify t1)
        Spanned.Str          t1       -> Str          (simplify t1)
        Spanned.Block        t1       -> Block        (simplify t1)
        Spanned.Marker       t1       -> Marker       (simplify t1)
        Spanned.LineBreak    t1       -> LineBreak    (simplify t1)
        Spanned.Comment      t1       -> Comment      (simplify t1)
        Spanned.Documented   t1 t2    -> Documented   (simplify t1) (simplify t2)
        Spanned.Metadata     t1       -> Metadata     (simplify t1)
        Spanned.Invalid      t1       -> Invalid      (simplify t1)
        Spanned.App          t1 t2    -> App          (simplify t1) (simplify t2)
        Spanned.InfixApp     t1 t2 t3 -> InfixApp     (simplify t1) (simplify t2) (simplify t3)
        Spanned.Missing               -> Missing
        Spanned.List         t1       -> List         (simplify t1)
        Spanned.Unit         t1       -> Unit         (simplify t1)
        Spanned.SectionLeft  t1 t2    -> SectionLeft  (simplify t1) (simplify t2)
        Spanned.SectionRight t1 t2    -> SectionRight (simplify t1) (simplify t2)
