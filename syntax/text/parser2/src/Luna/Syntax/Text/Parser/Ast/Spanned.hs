{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Parser.Ast.Spanned where

import Prologue hiding (Text, span)

import qualified Data.Text32                          as Text
import qualified Luna.IR.Term.Ast.Invalid             as Invalid
import qualified Luna.Syntax.Text.Parser.Ast.Class    as Ast
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan as CodeSpan

import Data.Text.Position                   (Delta)
import Luna.Syntax.Text.Parser.Ast.CodeSpan (CodeSpan (CodeSpan))
import OCI.Data.Name                        (Name)


type Text = Text.Text32



---------------------
-- === Spanned === --
---------------------

-- === Definition === --

data Spanned a = Spanned
    { _span :: CodeSpan
    , _ast  :: a
    } deriving (Eq, Functor, Ord, Show)
makeLenses ''Spanned


-- === Instances === --

instance Convertible (Spanned a) a where
    convert = view ast
    {-# INLINE convert #-}


-- === API === --

dropOffset :: Spanned a -> Spanned a
dropOffset = span %~ CodeSpan.dropOffset
{-# INLINE dropOffset #-}

inheritSpan1 :: (Spanned a -> a) -> Spanned a -> Spanned a
inheritSpan1 = \f t1 -> let
    s1 = t1 ^. span
    in Spanned s1 $! f (dropOffset t1)
{-# INLINE inheritSpan1 #-}

inheritSpan2
    :: (Spanned a -> Spanned a -> a)
    -> (Spanned a -> Spanned a -> Spanned a)
inheritSpan2 = \f t1 t2 -> let
    s1 = t1 ^. span
    s2 = t2 ^. span
    in Spanned (s1 <> s2) $! f (dropOffset t1) t2
{-# INLINE inheritSpan2 #-}

inheritSpan3
    :: (Spanned a -> Spanned a -> Spanned a -> a)
    -> (Spanned a -> Spanned a -> Spanned a -> Spanned a)
inheritSpan3 = \f t1 t2 t3 -> let
    s1 = t1 ^. span
    s2 = t2 ^. span
    s3 = t3 ^. span
    in Spanned (s1 <> s2 <> s3) $! f (dropOffset t1) t2 t3
{-# INLINE inheritSpan3 #-}

inheritSpanList1
    :: (NonEmpty (Spanned a) -> b) -> (NonEmpty (Spanned a) -> Spanned b)
inheritSpanList1 = \f (a :| as) -> let
    s  = view span a
    ss = view span <$> as
    in Spanned (foldl' (<>) s ss) $! f (dropOffset a :| as)
{-# INLINE inheritSpanList1 #-}

inheritSpanList
    :: ([Spanned a] -> b) -> ([Spanned a] -> Spanned b)
inheritSpanList = \f args -> case args of
    []     -> Spanned mempty $! f args
    (a:as) -> let
        s  = view span a
        ss = view span <$> as
        in Spanned (foldl' (<>) s ss) $! f (dropOffset a : as)
{-# INLINE inheritSpanList #-}

-- Warning.
-- When using this function you need to handle all child-component spans
-- correctly. Consider three components 'a b c'. If you want to convert them
-- to list of components, you have to remove the offset span from the 'a' child
-- and put it to the newly created parent instead. Use this function only when
-- you create non-hierarchical components and use the public utils otherwise.
computeCodeSpanList1__
    :: NonEmpty (Spanned Ast) -> Ast -> (Spanned Ast)
computeCodeSpanList1__ = \ts -> let
    (s :| ss) = view span <$> ts
    in Spanned (foldl' (<>) s ss)
{-# INLINE computeCodeSpanList1__ #-}

prependAsOffset :: Spanned a -> (Spanned b -> Spanned b)
prependAsOffset = \t -> span %~ (CodeSpan.prependAsOffset (t ^. span))
{-# INLINE prependAsOffset #-}

prependOffset :: Spanned a -> (Spanned b -> Spanned b)
prependOffset = \t -> span %~
    (CodeSpan.prependAsOffset $ CodeSpan.dropLength (t ^. span))
{-# INLINE prependOffset #-}

prependOffset' :: CodeSpan -> (Spanned b -> Spanned b)
prependOffset' = \t -> span %~
    (CodeSpan.prependAsOffset $ CodeSpan.dropLength t)
{-# INLINE prependOffset' #-}



-----------------
-- === Ast === --
-----------------

-- === Definition === --

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
    | AstMetadata     (Ast.Metadata     Ast)
    | AstInvalid      (Ast.Invalid      Ast)
    | AstApp          (Ast.App          Ast)
    | AstInfixApp     (Ast.InfixApp     Ast)
    | AstMissing      (Ast.Missing      Ast)
    | AstList         (Ast.List         Ast)
    | AstUnit         (Ast.Unit         Ast)
    | AstSectionLeft  (Ast.SectionLeft  Ast)
    | AstSectionRight (Ast.SectionRight Ast)
    deriving (Eq, Ord, Show)

type instance Ast.Link Ast (Ast.StrChunk s) = Spanned (Ast.StrChunk s)
type instance Ast.Link Ast Ast.Struct = Spanned Ast


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
pattern Metadata     t1       = AstMetadata     (Ast.Metadata     t1      )
pattern Invalid      t1       = AstInvalid      (Ast.Invalid      t1      )
pattern App          t1 t2    = AstApp          (Ast.App          t1 t2   )
pattern InfixApp     t1 t2 t3 = AstInfixApp     (Ast.InfixApp     t1 t2 t3)
pattern Missing               = AstMissing      (Ast.Missing              )
pattern List         t1       = AstList         (Ast.List         t1      )
pattern Unit         t1       = AstUnit         (Ast.Unit         t1      )
pattern SectionLeft  t1 t2    = AstSectionLeft  (Ast.SectionLeft  t1 t2   )
pattern SectionRight t1 t2    = AstSectionRight (Ast.SectionRight t1 t2   )


-- === Smart Constructors === --

invalid :: Invalid.Symbol -> Spanned Ast
invalid = Spanned mempty . Invalid
{-# INLINE invalid #-}

sectionLeft :: Spanned Ast -> Spanned Ast -> Spanned Ast
sectionLeft = inheritSpan2 $ \arg func -> SectionLeft arg func
{-# INLINE sectionLeft #-}

sectionRight :: Spanned Ast -> Spanned Ast -> Spanned Ast
sectionRight = inheritSpan2 $ \func arg -> SectionRight func arg
{-# INLINE sectionRight #-}

app :: Spanned Ast -> Spanned Ast -> Spanned Ast
app = inheritSpan2 $ \func arg -> App func arg
{-# INLINE app #-}

app2 :: Spanned Ast -> Spanned Ast -> Spanned Ast -> Spanned Ast
app2 = \f a b -> app (app f a) b
{-# INLINE app2 #-}

infixApp :: Spanned Ast -> Spanned Ast -> Spanned Ast -> Spanned Ast
infixApp = inheritSpan3 $ \l f r -> InfixApp l f r
{-# INLINE infixApp #-}

apps :: Spanned Ast -> [Spanned Ast] -> Spanned Ast
apps = foldl' app
{-# INLINE apps #-}

missing :: Spanned Ast
missing = Spanned mempty Missing
{-# INLINE missing #-}

list :: [Spanned Ast] -> Spanned Ast
list = inheritSpanList $ \items -> List items
{-# INLINE list #-}

unit :: Spanned Ast -> Spanned Ast
unit = inheritSpan1 $ \items -> Unit items
{-# INLINE unit #-}

block :: NonEmpty (Spanned Ast) -> Spanned Ast
block = inheritSpanList1 $ \items -> Block items
{-# INLINE block #-}

isOperator :: Ast -> Bool
isOperator = \case
    AstOperator {} -> True
    _ -> False
{-# INLINE isOperator #-}

documented :: Spanned Ast -> Spanned Ast -> Spanned Ast
documented = inheritSpan2 $ \doc base -> Documented doc base
{-# INLINE documented #-}

concatComments :: NonEmpty (Spanned Ast) -> Spanned Ast
concatComments = inheritSpanList1 $ \docs ->
    Comment $ unlines $ fmap (\(unspan -> Comment t) -> t) docs
{-# INLINE concatComments #-}



-----------------------------
-- === Unwrapping span === --
-----------------------------

-- | See docs of unspan.
unsafeUnspan :: Spanned a -> a
unsafeUnspan = \(Spanned _ a) -> a
{-# INLINE unsafeUnspan #-}

-- | Removing span is not always secure as it causes the child spans to be
--   incorrect if used with different parrent. Thus the secure way to remove
--   span is to embed the parent offset span into first child offset span while
--   unwrapping.
unspan :: Spanned Ast -> Ast
unspan = \(Spanned cs a) ->
    let span = CodeSpan.dropLength cs
    in case a of
        AstVar          a -> AstVar         $ prependSpan span a
        AstCons         a -> AstCons        $ prependSpan span a
        AstOperator     a -> AstOperator    $ prependSpan span a
        AstModifier     a -> AstModifier    $ prependSpan span a
        AstWildcard     a -> AstWildcard    $ prependSpan span a
        AstNumber       a -> AstNumber      $ prependSpan span a
        AstStr          a -> AstStr         $ prependSpan span a
        AstBlock        a -> AstBlock       $ prependSpan span a
        AstMarker       a -> AstMarker      $ prependSpan span a
        AstLineBreak    a -> AstLineBreak   $ prependSpan span a
        AstComment      a -> AstComment     $ prependSpan span a
        AstDocumented   a -> AstDocumented  $ prependSpan span a
        AstMetadata     a -> AstMetadata    $ prependSpan span a
        AstInvalid      a -> AstInvalid     $ prependSpan span a
        AstApp          a -> AstApp         $ prependSpan span a
        AstInfixApp     a -> AstInfixApp    $ prependSpan span a
        AstMissing      a -> AstMissing     $ prependSpan span a
        AstList         a -> AstList        $ prependSpan span a
        AstUnit         a -> AstUnit        $ prependSpan span a
        AstSectionLeft  a -> AstSectionLeft $ prependSpan span a
        AstSectionRight a -> AstSectionRight$ prependSpan span a
{-# INLINE unspan #-}



-------------------------
-- === PrependSpan === --
-------------------------

-- === Definition === --

class PrependSpan a where
    prependSpan :: CodeSpan -> a -> a
    prependSpan = \_ -> id


-- === Utils === --

prepSpan :: CodeSpan -> (Spanned b -> Spanned b)
prepSpan = \t -> span %~ (CodeSpan.prependAsOffset t)
{-# INLINE prepSpan #-}

prepSpanToNonEmpty :: CodeSpan -> (NonEmpty (Spanned b) -> NonEmpty (Spanned b))
prepSpanToNonEmpty = \t (a :| as)
    -> (a & span %~ (CodeSpan.prependAsOffset t)) :| as
{-# INLINE prepSpanToNonEmpty #-}

prepSpanToList :: CodeSpan -> ([Spanned b] -> [Spanned b])
prepSpanToList = \t -> \case
    [] -> []
    (a:as) -> (a & span %~ (CodeSpan.prependAsOffset t)) : as
{-# INLINE prepSpanToList #-}


-- === Instances === --

instance PrependSpan (Ast.Var       Ast)
instance PrependSpan (Ast.Cons      Ast)
instance PrependSpan (Ast.Operator  Ast)
instance PrependSpan (Ast.Modifier  Ast)
instance PrependSpan (Ast.Wildcard  Ast)
instance PrependSpan (Ast.Number    Ast)
instance PrependSpan (Ast.Str       Ast)
instance PrependSpan (Ast.Marker    Ast)
instance PrependSpan (Ast.LineBreak Ast)
instance PrependSpan (Ast.Comment   Ast)
instance PrependSpan (Ast.Metadata  Ast)
instance PrependSpan (Ast.Invalid   Ast)
instance PrependSpan (Ast.Missing   Ast)

instance PrependSpan (Ast.Block Ast) where
    prependSpan = \s (Ast.Block a) -> Ast.Block $ prepSpanToNonEmpty s a
    {-# INLINE prependSpan #-}

instance PrependSpan (Ast.Tokens Ast) where
    prependSpan = \s (Ast.Tokens a) -> Ast.Tokens $ prepSpanToList s a
    {-# INLINE prependSpan #-}

instance PrependSpan (Ast.App Ast) where
    prependSpan = \s (Ast.App a b) -> Ast.App (prepSpan s a) b
    {-# INLINE prependSpan #-}

instance PrependSpan (Ast.InfixApp Ast) where
    prependSpan = \s (Ast.InfixApp a b c) -> Ast.InfixApp (prepSpan s a) b c
    {-# INLINE prependSpan #-}

instance PrependSpan (Ast.SectionLeft Ast) where
    prependSpan = \s (Ast.SectionLeft a b) -> Ast.SectionLeft (prepSpan s a) b
    {-# INLINE prependSpan #-}

instance PrependSpan (Ast.SectionRight Ast) where
    prependSpan = \s (Ast.SectionRight a b) -> Ast.SectionRight (prepSpan s a) b
    {-# INLINE prependSpan #-}

instance PrependSpan (Ast.List Ast) where
    prependSpan = \s (Ast.List a) -> Ast.List (prepSpanToList s a)
    {-# INLINE prependSpan #-}

instance PrependSpan (Ast.Unit Ast) where
    prependSpan = \s (Ast.Unit a) -> Ast.Unit (prepSpan s a)
    {-# INLINE prependSpan #-}

instance PrependSpan (Ast.Documented Ast) where
    prependSpan = \s (Ast.Documented a b) -> Ast.Documented (prepSpan s a) b
    {-# INLINE prependSpan #-}
