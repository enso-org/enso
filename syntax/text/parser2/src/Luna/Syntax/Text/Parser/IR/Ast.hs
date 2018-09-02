{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.IR.Ast (module Luna.Syntax.Text.Parser.IR.Ast, module X) where

import qualified Prelude  as P
import           Prologue hiding (Text, imp, seq, some, span, takeWhile)

import qualified Data.Text32                           as Text
import qualified Luna.IR.Term.Ast.Invalid              as Invalid
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan as CodeSpan

import Luna.IR                               (SomeTerm, Term)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan (CodeSpan),
                                              CodeSpanRange (CodeSpanRange))
import OCI.Data.Name                         (Name)


--

import qualified Control.Monad.State.Layered           as State
import qualified Data.Attoparsec.Internal.Types        as AttoParsec
import qualified Data.Attoparsec.Text32                as Parsec
import qualified Data.Char                             as Char
import qualified Data.Set                              as Set
import qualified Data.Text.Position                    as Position
import qualified Data.Text.Span                        as Span
import qualified GHC.Exts                              as GHC
import qualified Luna.Syntax.Text.Lexer                as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol         as Lexer
import qualified Luna.Syntax.Text.Parser.Data.Ast.Atom as Atom
import qualified Luna.Syntax.Text.Parser.State.Marker  as Marker
import qualified Luna.Syntax.Text.Scope                as Scope

import Control.Monad.State.Layered              (StateT, StatesT)
import Data.Set                                 (Set)
import Data.Text.Position                       (FileOffset (..))
import Data.Text.Position                       (Delta, Position)
import Luna.Syntax.Text.Parser.State.LastOffset (LastOffset (LastOffset))
import Text.Parser.State.Indent                 (Indent)

import Data.Parser             hiding (Result, Token, Tokens, endOfInput)
import Text.Parser.Combinators (some)

import Luna.Syntax.Text.Parser.Data.Ast.Class as X



-- type Text = Text.Text32
data SyntaxVersion = Syntax1 | Syntax2 deriving (Show)






type Parser = StatesT
   '[ Result
    , SyntaxVersion
    , Indent
    , Position
    , LastOffset
    , CodeSpanRange
    , Marker.State
    , FileOffset
    , Scope.Scope
    ] Parsec.Parser



class Monad m => KnownParserOffset m where
    getParserOffset :: m Delta

instance KnownParserOffset Parsec.Parser where
    getParserOffset = AttoParsec.Parser $ \t pos more _ succ ->
        succ t pos more (convert $! AttoParsec.fromPos pos)
    {-# INLINE getParserOffset #-}

instance (MonadTrans t, Monad (t m), KnownParserOffset m)
      => KnownParserOffset (t m) where
    getParserOffset = lift getParserOffset
    {-# INLINE getParserOffset #-}




newtype Result = Result [Spanned Ast] deriving (Default, Mempty, Show)

register :: State.Monad Result m => Spanned Ast -> m ()
register = \a -> State.modify_ @Result $ wrapped %~ (a:)
{-# INLINE register #-}

lookupLastToken :: State.Getter Result m => m (Maybe (Spanned Ast))
lookupLastToken = head . unwrap <$> State.get @Result
{-# INLINE lookupLastToken #-}

lookupLastSymbol :: State.Getter Result m => m (Maybe Ast)
lookupLastSymbol = unspan <<$>> lookupLastToken
{-# INLINE lookupLastSymbol #-}

evalResult :: Monad m => StateT Result m a -> m [Spanned Ast]
evalResult = fmap (reverse . unwrap) . State.execDefT
{-# INLINE evalResult #-}





type family ExpandField t a
data Simple




pattern SVar       t1    = SimpleAstVar       (Atom.Atom_Var       t1)
pattern SCons      t1    = SimpleAstCons      (Atom.Atom_Cons      t1)
pattern SOperator  t1    = SimpleAstOperator  (Atom.Atom_Operator  t1)
pattern SModifier  t1    = SimpleAstModifier  (Atom.Atom_Modifier  t1)
pattern SWildcard        = SimpleAstWildcard  (Atom.Atom_Wildcard)
pattern SNumber    t1    = SimpleAstNumber    (Atom.Atom_Number    t1)
pattern SStr       t1    = SimpleAstStr       (Atom.Atom_Str       t1)
pattern SBlock    t1    = SimpleAstBlock    (Atom.Atom_Block    t1)
-- -- pattern STokens    t1    = SimpleAstTokens    (Atom.Atom_Tokens    t1)
pattern SMarker    t1    = SimpleAstMarker    (Atom.Atom_Marker    t1)
pattern SLineBreak t1    = SimpleAstLineBreak (Atom.Atom_LineBreak t1)
pattern SComment   t1    = SimpleAstComment   (Atom.Atom_Comment   t1)
pattern SDocumented   t1 t2    = SimpleAstDocumented   (Atom.Atom_Documented   t1 t2)
pattern SInvalid   t1    = SimpleAstInvalid   (Atom.Atom_Invalid   t1)
pattern SApp       t1 t2 = SimpleAstApp       (Atom.Atom_App       t1 t2)
pattern SInfixApp       t1 t2 t3 = SimpleAstInfixApp       (Atom.Atom_InfixApp       t1 t2 t3)
pattern SMissing         = SimpleAstMissing   (Atom.Atom_Missing)
pattern SList      t1    = SimpleAstList      (Atom.Atom_List      t1)
pattern SUnit      t1    = SimpleAstUnit      (Atom.Atom_Unit      t1)
pattern SSectionLeft  t1 t2 = SimpleAstSectionLeft  (Atom.Atom_SectionLeft  t1 t2)
pattern SSectionRight t1 t2 = SimpleAstSectionRight (Atom.Atom_SectionRight t1 t2)


pattern XList t <- Spanned s (AstList      (Atom.Atom_List      (prependOffsetToHead s -> t)))

prependOffsetToHead t = \case
    []     -> []
    (p:ps) -> prependOffset' t p : ps



class PrependSpan a where
    prependSpan :: CodeSpan -> a -> a
    prependSpan = \_ -> id



prepSpan :: CodeSpan -> (Spanned b -> Spanned b)
prepSpan = \t -> span %~ (CodeSpan.prependAsOffset t)
{-# INLINE prepSpan #-}

prepSpanToNonEmpty :: CodeSpan -> (NonEmpty (Spanned b) -> NonEmpty (Spanned b))
prepSpanToNonEmpty = \t (a :| as) -> (a & span %~ (CodeSpan.prependAsOffset t)) :| as
{-# INLINE prepSpanToNonEmpty #-}

prepSpanToList :: CodeSpan -> ([Spanned b] -> [Spanned b])
prepSpanToList = \t -> \case
    [] -> []
    (a:as) -> (a & span %~ (CodeSpan.prependAsOffset t)) : as
{-# INLINE prepSpanToList #-}

instance PrependSpan (Atom.Var Ast)
instance PrependSpan (Atom.Cons Ast)
instance PrependSpan (Atom.Operator Ast)
instance PrependSpan (Atom.Modifier Ast)
instance PrependSpan (Atom.Wildcard Ast)
instance PrependSpan (Atom.Number Ast)
instance PrependSpan (Atom.Str Ast)
instance PrependSpan (Atom.Marker Ast)
instance PrependSpan (Atom.LineBreak Ast)
instance PrependSpan (Atom.Comment Ast)
instance PrependSpan (Atom.Invalid Ast)
instance PrependSpan (Atom.Missing Ast)


instance PrependSpan (Atom.Block Ast) where
    prependSpan = \span (Atom.Atom_Block a) -> Atom.Atom_Block $ prepSpanToNonEmpty span a
    {-# INLINE prependSpan #-}

instance PrependSpan (Atom.Tokens Ast) where
    prependSpan = \span (Atom.Atom_Tokens a) -> Atom.Atom_Tokens $ prepSpanToList span a
    {-# INLINE prependSpan #-}

instance PrependSpan (Atom.App Ast) where
    prependSpan = \span (Atom.Atom_App a b) -> Atom.Atom_App (prepSpan span a) b
    {-# INLINE prependSpan #-}

instance PrependSpan (Atom.InfixApp Ast) where
    prependSpan = \span (Atom.Atom_InfixApp a b c) -> Atom.Atom_InfixApp (prepSpan span a) b c
    {-# INLINE prependSpan #-}

instance PrependSpan (Atom.SectionLeft Ast) where
    prependSpan = \span (Atom.Atom_SectionLeft a b) -> Atom.Atom_SectionLeft (prepSpan span a) b
    {-# INLINE prependSpan #-}

instance PrependSpan (Atom.SectionRight Ast) where
    prependSpan = \span (Atom.Atom_SectionRight a b) -> Atom.Atom_SectionRight (prepSpan span a) b
    {-# INLINE prependSpan #-}

instance PrependSpan (Atom.List Ast) where
    prependSpan = \span (Atom.Atom_List a) -> Atom.Atom_List (prepSpanToList span a)
    {-# INLINE prependSpan #-}

instance PrependSpan (Atom.Unit Ast) where
    prependSpan = \span (Atom.Atom_Unit a) -> Atom.Atom_Unit (prepSpan span a)
    {-# INLINE prependSpan #-}

instance PrependSpan (Atom.Documented Ast) where
    prependSpan = \span (Atom.Atom_Documented a b) -> Atom.Atom_Documented (prepSpan span a) b
    {-# INLINE prependSpan #-}


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
        AstBlock        a -> AstBlock      $ prependSpan span a
        AstMarker       a -> AstMarker      $ prependSpan span a
        AstLineBreak    a -> AstLineBreak   $ prependSpan span a
        AstComment      a -> AstComment     $ prependSpan span a
        AstDocumented   a -> AstDocumented  $ prependSpan span a
        AstInvalid      a -> AstInvalid     $ prependSpan span a
        AstApp          a -> AstApp         $ prependSpan span a
        AstInfixApp     a -> AstInfixApp    $ prependSpan span a
        AstMissing      a -> AstMissing     $ prependSpan span a
        AstList         a -> AstList        $ prependSpan span a
        AstUnit         a -> AstUnit        $ prependSpan span a
        AstSectionLeft  a -> AstSectionLeft $ prependSpan span a
        AstSectionRight a -> AstSectionRight$ prependSpan span a
{-# INLINE unspan #-}


type instance Atom.Link SimpleAst (Atom.StrChunk s) = Atom.StrChunk s
type instance Atom.Link SimpleAst Atom.Struct = SimpleAst

data SimpleAst
    = SimpleAstVar          (Atom.Var          SimpleAst)
    | SimpleAstCons         (Atom.Cons         SimpleAst)
    | SimpleAstOperator     (Atom.Operator     SimpleAst)
    | SimpleAstModifier     (Atom.Modifier     SimpleAst)
    | SimpleAstWildcard     (Atom.Wildcard     SimpleAst)
    | SimpleAstNumber       (Atom.Number       SimpleAst)
    | SimpleAstStr          (Atom.Str          SimpleAst)
    | SimpleAstBlock        (Atom.Block        SimpleAst)
    | SimpleAstMarker       (Atom.Marker       SimpleAst)
    | SimpleAstLineBreak    (Atom.LineBreak    SimpleAst)
    | SimpleAstComment      (Atom.Comment      SimpleAst)
    | SimpleAstDocumented   (Atom.Documented   SimpleAst)
    | SimpleAstInvalid      (Atom.Invalid      SimpleAst)
    | SimpleAstApp          (Atom.App          SimpleAst)
    | SimpleAstInfixApp     (Atom.InfixApp     SimpleAst)
    | SimpleAstMissing      (Atom.Missing      SimpleAst)
    | SimpleAstList         (Atom.List         SimpleAst)
    | SimpleAstUnit         (Atom.Unit         SimpleAst)
    | SimpleAstSectionLeft  (Atom.SectionLeft  SimpleAst)
    | SimpleAstSectionRight (Atom.SectionRight SimpleAst)
    deriving (Eq, Ord)

instance Show SimpleAst where
    show = \case
        SimpleAstVar          t -> show t
        SimpleAstCons         t -> show t
        SimpleAstOperator     t -> show t
        SimpleAstModifier     t -> show t
        SimpleAstWildcard     t -> show t
        SimpleAstNumber       t -> show t
        SimpleAstStr          t -> show t
        SimpleAstBlock        t -> show t
        SimpleAstMarker       t -> show t
        SimpleAstLineBreak    t -> show t
        SimpleAstComment      t -> show t
        SimpleAstDocumented   t -> show t
        SimpleAstInvalid      t -> show t
        SimpleAstApp          t -> show t
        SimpleAstInfixApp     t -> show t
        SimpleAstMissing      t -> show t
        SimpleAstList         t -> show t
        SimpleAstUnit         t -> show t
        SimpleAstSectionLeft  t -> show t
        SimpleAstSectionRight t -> show t


sapp :: SimpleAst -> SimpleAst -> SimpleAst
sapp = SApp
{-# INLINE sapp #-}

sapp2 :: SimpleAst -> SimpleAst -> SimpleAst -> SimpleAst
sapp2 = \f -> sapp . sapp f
{-# INLINE sapp2 #-}

sapp3 :: SimpleAst -> SimpleAst -> SimpleAst -> SimpleAst -> SimpleAst
sapp3 = \f -> sapp .: sapp2 f
{-# INLINE sapp3 #-}

sapp4 :: SimpleAst -> SimpleAst -> SimpleAst -> SimpleAst -> SimpleAst
      -> SimpleAst
sapp4 = \f -> sapp .:. sapp3 f
{-# INLINE sapp4 #-}

sapps :: SimpleAst -> [SimpleAst] -> SimpleAst
sapps = foldl' sapp
{-# INLINE sapps #-}


instance IsString SimpleAst where
    fromString = convert

instance Convertible String (t1 -> SimpleAst)
      => IsString (t1 -> SimpleAst) where
    fromString = convert

instance Convertible String (t1 -> t2 -> SimpleAst)
      => IsString (t1 -> t2 -> SimpleAst) where
    fromString = convert

instance Convertible String (t1 -> t2 -> t3 -> SimpleAst)
      => IsString (t1 -> t2 -> t3 -> SimpleAst) where
    fromString = convert

-- instance Convertible String (t1 -> t2 -> t3 -> t4 -> SimpleAst)
--       => IsString (t1 -> t2 -> t3 -> t4 -> SimpleAst) where
--     fromString = convert


appMany :: SimpleAst -> [SimpleAst] -> SimpleAst
appMany = \a -> \case
    []     -> a
    (t:ts) -> appMany (sapp a t) ts
{-# appMany #-}

instance Convertible String SimpleAst where
    convert = let
        isMixfix = ('_' `elem`)
        in \case
            ""  -> SMissing
            "_" -> SWildcard
            x@(s:ss) -> let n = convert x in if
                | s == '_'       -> SVar      n
                | Char.isLower s -> SVar      n
                | Char.isUpper s -> SCons     n
                | Char.isDigit s -> SNumber $ charsToDigits (s :| ss)
                | isMixfix x     -> SVar      n
                | otherwise      -> case last ss of
                    Just '=' -> SModifier $ convert (s : unsafeInit ss)
                    _        -> SOperator n

charsToDigits :: NonEmpty Char -> NonEmpty Word8
charsToDigits (c :| cs) = go c :| (go <$> cs) where
    go c = fromIntegral $ Char.ord c - 48

instance Convertible SimpleAst (t1 -> SimpleAst)
      => Convertible String (t1 -> SimpleAst) where
    convert = convert . convertTo @SimpleAst

instance Convertible SimpleAst (t1 -> t2 -> SimpleAst)
      => Convertible String (t1 -> t2 -> SimpleAst) where
    convert = convert . convertTo @SimpleAst

instance Convertible SimpleAst (t1 -> t2 -> t3 -> SimpleAst)
      => Convertible String (t1 -> t2 -> t3 -> SimpleAst) where
    convert = convert . convertTo @SimpleAst

instance Convertible SimpleAst (t1 -> t2 -> t3 -> t4 -> SimpleAst)
      => Convertible String (t1 -> t2 -> t3 -> t4 -> SimpleAst) where
    convert = convert . convertTo @SimpleAst



instance t1 ~ SimpleAst
      => Convertible SimpleAst (t1 -> SimpleAst) where
    convert = sapp

instance (t1 ~ SimpleAst, t2 ~ SimpleAst)
      => Convertible SimpleAst (t1 -> t2 -> SimpleAst) where
    convert = sapp2

instance (t1 ~ SimpleAst, t2 ~ SimpleAst, t3 ~ SimpleAst)
      => Convertible SimpleAst (t1 -> t2 -> t3 -> SimpleAst) where
    convert = sapp3

instance (t1 ~ SimpleAst, t2 ~ SimpleAst, t3 ~ SimpleAst, t4 ~ SimpleAst)
      => Convertible SimpleAst (t1 -> t2 -> t3 -> t4 -> SimpleAst) where
    convert = sapp4

instance t1 ~ SimpleAst
      => Convertible Invalid.Symbol (t1 -> SimpleAst) where
    convert = convert . convertTo @SimpleAst

instance (t1 ~ SimpleAst, t2 ~ SimpleAst)
      => Convertible Invalid.Symbol (t1 -> t2 -> SimpleAst) where
    convert = convert . convertTo @SimpleAst

instance Convertible Invalid.Symbol SimpleAst where
    convert = SInvalid

instance {-# OVERLAPPABLE #-} Convertible' a SimpleAst
      => Convertible [a] SimpleAst where
    convert = SList . fmap convert'

instance GHC.IsList SimpleAst where
    type Item SimpleAst = SimpleAst
    fromList = convert

instance Num SimpleAst where
    fromInteger = intToSimpleAst
    (-) = flip SInfixApp (SOperator "-")
    (+) = flip SInfixApp (SOperator "+")
    (*) = flip SInfixApp (SOperator "*")

intToSimpleAst :: Integral a => a -> SimpleAst
intToSimpleAst a = if a < 0
    then undefined
    else SNumber $ intToDigits a

intToDigits :: Integral a => a -> NonEmpty Word8
intToDigits = go [] where
    go s x = loop (head :| s) tail where
        head = fromIntegral (x`mod` 10)
        tail = x `div` 10
    loop s@(r :| rs) = \case
        0 -> s
        x -> go (r : rs) x

instance a ~ SimpleAst
      => Num (a -> SimpleAst) where
    fromInteger i = SApp (fromInteger i)
--     (-) = extractOp (-)
--     (+) = extractOp (+)
--     (*) = extractOp (*)

-- extractOp op a b = sapp $ op (extract a) (extract b) where
--     extract f = let SApp x _ = f SMissing in x

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
    simplify = simplify . unsafeUnspan

instance Simplify a
      => Simplify   [a] where
    type Simplified [a] = [Simplified a]
    simplify = fmap simplify

instance Simplify (Atom.StrChunk Ast) where
    type Simplified (Atom.StrChunk Ast) = Atom.StrChunk SimpleAst
    simplify = \case
        Atom.StrPlain   t                  -> Atom.StrPlain   t
        Atom.StrNewLine (Atom.Atom_LineBreak t) -> Atom.StrNewLine (Atom.Atom_LineBreak t)

instance Simplify   Ast where
    type Simplified Ast = SimpleAst
    simplify = \case
        Var       t1    -> SVar       (simplify t1)
        Cons      t1    -> SCons      (simplify t1)
        Operator  t1    -> SOperator  (simplify t1)
        Modifier  t1    -> SModifier  (simplify t1)
        Wildcard        -> SWildcard
        Number    t1    -> SNumber    (simplify t1)
        Str       t1    -> SStr       (simplify t1)
        Block    t1    -> SBlock    (simplify t1)
        -- Tokens    t1    -> STokens    (simplify t1)
        Marker    t1    -> SMarker    (simplify t1)
        LineBreak t1    -> SLineBreak (simplify t1)
        Comment   t1    -> SComment   (simplify t1)
        Documented   t1 t2   -> SDocumented   (simplify t1) (simplify t2)
        Invalid   t1    -> SInvalid   (simplify t1)
        App       t1 t2 -> SApp       (simplify t1) (simplify t2)
        InfixApp  t1 t2 t3 -> SInfixApp  (simplify t1) (simplify t2) (simplify t3)
        Missing         -> SMissing
        List      t1    -> SList      (simplify t1)
        Unit      t1    -> SUnit      (simplify t1)
        SectionLeft  t1 t2 -> SSectionLeft  (simplify t1) (simplify t2)
        SectionRight t1 t2 -> SSectionRight (simplify t1) (simplify t2)



------ FIXME vvv
makeLenses ''Result


dropOffset :: Spanned a -> Spanned a
dropOffset = span %~ CodeSpan.dropOffset
{-# INLINE dropOffset #-}

inheritCodeSpan1 :: (Spanned Ast -> Ast) -> Spanned Ast -> Spanned Ast
inheritCodeSpan1 = \f t1 -> let
    s1 = t1 ^. span
    in Spanned s1 $! f (dropOffset t1)
{-# INLINE inheritCodeSpan1 #-}

inheritCodeSpan2
    :: (Spanned Ast -> Spanned Ast -> Ast)
    -> (Spanned Ast -> Spanned Ast -> Spanned Ast)
inheritCodeSpan2 = \f t1 t2 -> let
    s1 = t1 ^. span
    s2 = t2 ^. span
    in Spanned (s1 <> s2) $! f (dropOffset t1) t2
{-# INLINE inheritCodeSpan2 #-}

inheritCodeSpan3
    :: (Spanned Ast -> Spanned Ast -> Spanned Ast -> Ast)
    -> (Spanned Ast -> Spanned Ast -> Spanned Ast -> Spanned Ast)
inheritCodeSpan3 = \f t1 t2 t3 -> let
    s1 = t1 ^. span
    s2 = t2 ^. span
    s3 = t3 ^. span
    in Spanned (s1 <> s2 <> s3) $! f (dropOffset t1) t2 t3
{-# INLINE inheritCodeSpan3 #-}

inheritCodeSpanList1
    :: (NonEmpty (Spanned a) -> b) -> (NonEmpty (Spanned a) -> Spanned b)
inheritCodeSpanList1 = \f (a :| as) -> let
    s  = view span a
    ss = view span <$> as
    in Spanned (foldl' (<>) s ss) $! f (dropOffset a :| as)
{-# INLINE inheritCodeSpanList1 #-}

inheritCodeSpanList
    :: ([Spanned a] -> b) -> ([Spanned a] -> Spanned b)
inheritCodeSpanList = \f args -> case args of
    []     -> Spanned mempty $! f args
    (a:as) -> let
        s  = view span a
        ss = view span <$> as
        in Spanned (foldl' (<>) s ss) $! f (dropOffset a : as)
{-# INLINE inheritCodeSpanList #-}

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



-------------------
-- === Spans === --
-------------------

getLastOffset :: State.Getter LastOffset m => m Delta
getLastOffset = unwrap <$> State.get @LastOffset
{-# INLINE getLastOffset #-}

putLastOffset :: State.Setter LastOffset m => Delta -> m ()
putLastOffset = State.put @LastOffset . wrap
{-# INLINE putLastOffset #-}

spanned :: Parser a -> Parser (CodeSpan, a)
spanned = \parser -> do
    lastEndOff <- getLastOffset
    startOff   <- getParserOffset
    out        <- parser
    endOff     <- getParserOffset
    nextOff    <- whiteSpace
    putLastOffset endOff

    let spacing   = startOff - lastEndOff
        bodyLen   = endOff - startOff
        totalSpan = bodyLen + nextOff
        realSpan  = Span.leftSpacedSpan spacing bodyLen

    Position.incColumn totalSpan
    pure (CodeSpan realSpan realSpan, out)
{-# INLINE spanned #-}

newline :: Parser ()
newline = do
    len <- eol
    off <- whiteSpace
    Position.succLine
    Position.incColumn off
{-# INLINE newline #-}

lexeme :: Parser a -> Parser a
lexeme = \p -> p <* whiteSpace
{-# INLINE lexeme #-}

whiteSpace :: Parser Delta
whiteSpace = convert . Text.length <$> takeMany ' '
{-# INLINE whiteSpace #-}

computeSpan :: Parser a -> Parser (Spanned a)
computeSpan = \p -> uncurry Spanned <$> spanned p
{-# INLINE computeSpan #-}

eol :: Parser Delta
eol = (1 <$ n) <|> (2 <$ rn) <|> (1 <$ r) where
    n  = token '\n'
    r  = token '\r'
    rn = r >> n
{-# INLINE eol #-}

isEolBeginChar :: Char -> Bool
isEolBeginChar = (`elem` eolStartChars)
{-# INLINE isEolBeginChar #-}

eolStartChars :: [Char]
eolStartChars = ['\n', '\r', '\ETX']
{-# INLINE eolStartChars #-}



-- === Smart constructors === --

-- app :: Spanned Ast -> Spanned Ast -> Spanned Ast
-- app = inheritCodeSpan2 $ \base arg -> AstApp $ App base arg
-- {-# INLINE app #-}

-- cons' :: Name -> Ast
-- cons' = \name -> Cons name
-- {-# INLINE cons' #-}

-- invalid' :: Invalid.Symbol -> Ast
-- invalid' = \desc -> Invalid desc
-- {-# INLINE invalid' #-}

invalid :: Invalid.Symbol -> Spanned Ast
invalid = Spanned mempty . Invalid
{-# INLINE invalid #-}

-- marker' :: Int -> Ast
-- marker' = \id -> AstMarker $ Marker id
-- {-# INLINE marker' #-}

-- modifier' :: Name -> Ast
-- modifier' = \name -> AstModifier $ Modifier name
-- {-# INLINE modifier' #-}

-- operator' :: Name -> Ast
-- operator' = \name -> AstOperator $ Operator name
-- {-# INLINE operator' #-}

-- var' :: Name -> Ast
-- var' = \name -> AstVar $ Var name
-- {-# INLINE var' #-}

-- lineBreak' :: Delta -> Ast
-- lineBreak' = \indent -> AstLineBreak $ LineBreak indent
-- {-# INLINE lineBreak' #-}

-- comment' :: Text -> Ast
-- comment' = \txt -> AstComment $ Comment txt
-- {-# INLINE comment' #-}

-- wildcard' :: Ast
-- wildcard' = AstWildcard Wildcard
-- {-# INLINE wildcard' #-}

-- number' :: NonEmpty Word8 -> Ast
-- number' = \digits -> AstNumber $ Number digits
-- {-# INLINE number' #-}

-- str' :: [Spanned (StrChunk Ast)] -> Ast
-- str' = \chunks -> AstStr $ Str chunks
-- {-# INLINE str' #-}

-- tokens' :: [Spanned Ast] -> Ast
-- tokens' = \toks -> AstTokens $ Tokens toks
-- {-# INLINE tokens' #-}


--

sectionLeft :: Spanned Ast -> Spanned Ast -> Spanned Ast
sectionLeft = inheritCodeSpan2 $ \arg func -> SectionLeft arg func
{-# INLINE sectionLeft #-}

sectionRight :: Spanned Ast -> Spanned Ast -> Spanned Ast
sectionRight = inheritCodeSpan2 $ \func arg -> SectionRight func arg
{-# INLINE sectionRight #-}

app :: Spanned Ast -> Spanned Ast -> Spanned Ast
app = inheritCodeSpan2 $ \func arg -> App func arg
{-# INLINE app #-}

app2 :: Spanned Ast -> Spanned Ast -> Spanned Ast -> Spanned Ast
app2 = \f a b -> app (app f a) b
{-# INLINE app2 #-}

infixApp :: Spanned Ast -> Spanned Ast -> Spanned Ast -> Spanned Ast
infixApp = inheritCodeSpan3 $ \l f r -> InfixApp l f r
{-# INLINE infixApp #-}

apps :: Spanned Ast -> [Spanned Ast] -> Spanned Ast
apps = foldl' app
{-# INLINE apps #-}

-- missing' :: Ast
-- missing' = AstMissing Missing
-- {-# INLINE missing' #-}

missing :: Spanned Ast
missing = Spanned mempty Missing
{-# INLINE missing #-}

-- list' :: [Spanned Ast] -> Ast
-- list' = \items -> AstList $ List items
-- {-# INLINE list' #-}

list :: [Spanned Ast] -> Spanned Ast
list = inheritCodeSpanList $ \items -> List items
{-# INLINE list #-}

unit :: Spanned Ast -> Spanned Ast
unit = inheritCodeSpan1 $ \items -> Unit items
{-# INLINE unit #-}

block :: NonEmpty (Spanned Ast) -> Spanned Ast
block = inheritCodeSpanList1 $ \items -> Block items
{-# INLINE block #-}

isOperator :: Ast -> Bool
isOperator = \case
    AstOperator {} -> True
    _ -> False
{-# INLINE isOperator #-}

documented :: Spanned Ast -> Spanned Ast -> Spanned Ast
documented = inheritCodeSpan2 $ \doc base -> Documented doc base
{-# INLINE documented #-}

-- data Ast
--     -- Identifiers
--     = AstVar       Var
--     | AstCons      Cons
--     | AstOperator  Operator
--     | AstModifier  Modifier
--     | AstWildcard  Wildcard

--     -- Literals
--     | AstNumber    Number
--     | AstStr       Str

--     -- Layouting
--     | AstTokens    Tokens
--     | AstMarker    Marker
--     | AstLineBreak LineBreak

--     -- Docs
--     | AstComment   Comment

--     -- Errors
--     | AstInvalid   Invalid

--     deriving (Show)





-- isDigitCharAtBase :: Word8 -> Char -> Bool
-- isDigitCharAtBase = \base char -> case charToDigit char of
--     Just n  -> n < base
--     Nothing -> False
-- {-# INLINE isDigitCharAtBase #-}

-- isDecDigitChar :: Char -> Bool
-- isDecDigitChar = isDigitCharAtBase 10 ; {-# INLINE isDecDigitChar #-}

-- charToDigit :: Char -> Maybe Word8
-- charToDigit = \char -> let
--     n = Char.ord char
--     in unsafeConvert <$> if
--         | n >= 48 && n <= 57  -> Just $ n - 48      -- 0 to 9
--         | n >= 65 && n <= 90  -> Just $ n - 65 + 10 -- A to Z
--         | n >= 97 && n <= 122 -> Just $ n - 97 + 10 -- a to z
--         | otherwise           -> Nothing
-- {-# INLINE charToDigit #-}

-- unsafeCharToDigit :: Char -> Word8
-- unsafeCharToDigit = \c -> case charToDigit c of
--     Just t  -> t
--     Nothing -> error $ "Cannot convert char " <> [c] <> " to digit."
-- {-# INLINE unsafeCharToDigit #-}

-- lexNumber :: Lexer
-- lexNumber = addSymbol =<< checkInvalidSuffix number where
--     number  = Symbol.Number <$> (special <|> dec)
--     special = token '0' *> choice [p0 'x' 16, p0 'o' 8, p0 'b' 2]
--     dec     = Symbol.NumRep 10            <$> body 10 <*> frac 10
--     p0 s n  = Symbol.NumRep n  <$ token s <*> body n  <*> frac n
--     body n  = Txt.toList . Txt.map unsafeCharToDigit
--           <$> takeWhile1 (isDigitCharAtBase n)
--     frac  n = option mempty $ token '.' *> body n
-- {-# INLINE lexNumber #-}
