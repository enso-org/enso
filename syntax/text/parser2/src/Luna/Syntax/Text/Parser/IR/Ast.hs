{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.IR.Ast where

import qualified Prelude  as P
import           Prologue hiding (Text, imp, seq, some, span, takeWhile)

import qualified Data.Text32                           as Text
import qualified Luna.IR.Term.Ast.Invalid              as Invalid
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan as CodeSpan
import qualified Luna.Syntax.Text.Parser.Pass.Class    as Parser

import Luna.IR                               (SomeTerm, Term)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan (CodeSpan),
                                              CodeSpanRange (CodeSpanRange))
import OCI.Data.Name                         (Name)


--

import qualified Control.Monad.State.Layered          as State
import qualified Data.Attoparsec.Internal.Types       as AttoParsec
import qualified Data.Attoparsec.Text32               as Parsec
import qualified Data.Char                            as Char
import qualified Data.Set                             as Set
import qualified Data.Text.Position                   as Position
import qualified Data.Text.Span                       as Span
import qualified Luna.Syntax.Text.Lexer               as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol        as Lexer
import qualified Luna.Syntax.Text.Parser.State.Marker as Marker
import qualified Luna.Syntax.Text.Scope               as Scope

import Control.Monad.State.Layered              (StateT, StatesT)
import Data.Set                                 (Set)
import Data.Text.Position                       (FileOffset (..))
import Data.Text.Position                       (Delta, Position)
import Luna.Syntax.Text.Parser.State.LastOffset (LastOffset (LastOffset))
import Text.Parser.State.Indent                 (Indent)

import Data.Parser             hiding (Result, Token, Tokens, endOfInput)
import Text.Parser.Combinators (some)



type Text = Text.Text32
data SyntaxVersion = Syntax1 | Syntax2 deriving (Show)




data Spanned a = Spanned
    { _span :: CodeSpan
    , _ast  :: a
    } deriving (Eq, Functor, Ord, Show)
makeLenses ''Spanned

instance Convertible (Spanned a) a where
    convert = view ast
    {-# INLINE convert #-}


unspan :: Spanned a -> a
unspan = \(Spanned _ a) -> a
{-# INLINE unspan #-}


-----------------------
-- === Blacklist === --
-----------------------

-- === Definition === --

-- data Blacklist = Blacklist
--     { _unknownBlacklist :: Set Char
--     , _operators        :: Set Name
--     }
-- makeLenses ''Blacklist


-- -- === API === --

-- withBlacklistedUnknown :: State.Monad Blacklist m => Char -> m a -> m a
-- withBlacklistedUnknown = \a
--      -> State.withModified @Blacklist (unknownBlacklist %~ Set.insert a)
-- {-# INLINE withBlacklistedUnknown #-}


-- withBlacklistedOperator :: State.Monad Blacklist m => Name -> m a -> m a
-- withBlacklistedOperator = \a -> State.withModified @Blacklist (operators %~ Set.insert a)
-- {-# INLINE withBlacklistedOperator #-}


-- checkBlacklistedUnknown :: Char -> Parser ()
-- checkBlacklistedUnknown = \a -> do
--     blacklisted <- Set.member a . view unknownBlacklist <$> State.get @Blacklist
--     when_ blacklisted $ fail "Blacklisted"
-- {-# INLINE checkBlacklistedUnknown #-}

-- checkBlacklistedOperator :: Name -> Parser ()
-- checkBlacklistedOperator = \a -> do
--     blacklisted <- Set.member a . view operators <$> State.get @Blacklist
--     when_ blacklisted $ fail "Blacklisted"
-- {-# INLINE checkBlacklistedOperator #-}



-- -- === Instances === --

-- instance Mempty Blacklist where
--     mempty = Blacklist mempty mempty
--     {-# INLINE mempty #-}

-- instance Default Blacklist where
--     def = mempty
--     {-# INLINE def #-}





    -- data Location = Location
--     { _offset :: Delta
--     , _column :: Delta
--     , _row    :: Delta
--     } deriving (Eq, Generic, Ord, Show)
-- makeLenses ''Location


-- === Instances === --

-- instance NFData Location
-- instance Default Location where
--     def = Location 0 0 0
--     {-# INLINE def #-}


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



-----------------
-- === AST === --
-----------------

-- data Ast a
--     = Var { name :: Name }
    -- = AccSection   { path     :: [Name]                               }
    -- | Cons         { name     :: Name    , args  :: [Ast]             }
    -- | Disabled     { body     :: Ast                                  }
    -- | Documented   { doc      :: Txt     , base   :: Ast              }
    -- | Function     { name     :: Ast     , args   :: [Ast]
    --                , body     :: Ast                                  }
    -- | DefHeader    { tp       :: Ast     , unis   :: [Ast]
    --                , accs     :: [Ast]   , apps   :: [Ast]            }
    -- | Grouped      { body     :: Ast                                  }
    -- | Imp          { source   :: Ast     , target :: ImportTargetData }
    -- | ImportHub    { imps     :: [Ast]                                }
    -- | ImportSource { body     :: ImportSourceData                     }
    -- | Invalid      { desc     :: Invalid.Symbol                       }
    -- | List         { items    :: [Ast]                                }
    -- | Marked       { marker   :: Ast     , body   :: Ast              }
    -- | Marker       { id       :: Word64                               }
    -- | SectionLeft  { operator :: Ast     , body   :: Ast              }
    -- | SectionRight { operator :: Ast     , body   :: Ast              }
    -- | Modify       { base     :: Ast     , path   :: [Name]
    --                , operator :: Name    , value  :: Ast              }
    -- | Metadata     { content  :: Txt                                  }
    -- | Record       { isNative :: Bool    , name   :: Name
    --                , params   :: [Ast]   , conss  :: [Ast]
    --                , decls    :: [Ast]                                }
    -- | RecordCons   { name     :: Name    , fields :: [Ast]            }
    -- | RecordFields { names    :: [Name]  , tp     :: Ast              }
    -- | Seq          { former   :: Ast     , later  :: Ast              }
    -- | Tuple        { items    :: [Ast]                                }
    -- | Typed        { base     :: Ast     , tp     :: Ast              }
    -- | Unit         { imps     :: Ast     , units  :: [Ast]
    --                , cls      :: Ast                                  }
    -- DEPRECATED:
    -- | FunctionSig  { name     :: Ast     , sig    :: Ast         }

-- data Foo a = Foo {-# UNPACK #-} !a
-- data Spanned2 a = Spanned2
--     { _span2 :: CodeSpan
--     , _ast2  :: a
--     } deriving (Show)

-- type family Link t a

-- data TypeDef t = TypeDef { name2 :: {-# Unpack #-} !(Maybe Int) , body :: {-# UNPACK #-} !(Link t (Maybe (Block2 t))) }
-- data Block2  t = Block2  { lines2 :: Link t (NonEmpty (Ast2 t)) }

-- data Ast2 t
--     = AstTypeDef (TypeDef t)
--     | AstBlock2  (Block2 t)


-- type Unspanned = Ast Spanned


type family ExpandField t a
data Simple

-- type instance Link Parsed a = Spanned2 (LinkParsed a)
-- type family LinkParsed a where
--     LinkParsed (Maybe a) = Maybe (LinkParsed a)

-- type S a = ExpandField Simple a

-- type instance ExpandField Simple a = ExpandFieldSimple a

-- type family ExpandFieldSimple a where
--     ExpandFieldSimple (NonEmpty a)   = NonEmpty (ExpandFieldSimple a)
--     ExpandFieldSimple [a]            = [ExpandFieldSimple a]
--     ExpandFieldSimple Delta          = Delta
--     ExpandFieldSimple Int            = Int
--     ExpandFieldSimple Bool           = Bool
--     ExpandFieldSimple Name           = Name
--     ExpandFieldSimple Text           = Text
--     ExpandFieldSimple Invalid.Symbol = Invalid.Symbol

--     ExpandFieldSimple Ast            = Spanned Ast
--     ExpandFieldSimple Block          = Spanned Block
--     ExpandFieldSimple LineBreak      = Spanned LineBreak
--     ExpandFieldSimple StrChunk       = Spanned StrChunk

data Struct
type family Link t a
type Link' t = Link t Struct

data Var       t = Atom_Var       { name     :: Name                    }
data Cons      t = Atom_Cons      { name     :: Name                    }
data Operator  t = Atom_Operator  { name     :: Name                    }
data Modifier  t = Atom_Modifier  { name     :: Name                    }
data Wildcard  t = Atom_Wildcard

data Number    t = Atom_Number    { digits   :: NonEmpty Word8          }
data Str       t = Atom_Str       { chunks   :: [Link t (StrChunk t)]   }

data Block     t = Atom_Block     { lines    :: (NonEmpty (Link' t))    }
data Tokens    t = Atom_Tokens    { lines    :: [Link' t]               }
data Marker    t = Atom_Marker    { markerID :: Int                     }
data LineBreak t = Atom_LineBreak { indent   :: Delta                   }

data Comment   t = Atom_Comment   { text     :: Text                    }

data Invalid   t = Atom_Invalid   { desc     :: Invalid.Symbol          }

data App       t = Atom_App       { func     :: Link' t, arg :: Link' t }
data Missing   t = Atom_Missing
data List      t = Atom_List      { items    :: [Link' t]               }

data StrChunk t
    = StrPlain   Text
    | StrNewLine (LineBreak t)
    | StrExpr    (Block t)

pattern Var       t1    = AstVar       (Atom_Var       t1)
pattern Cons      t1    = AstCons      (Atom_Cons      t1)
pattern Operator  t1    = AstOperator  (Atom_Operator  t1)
pattern Modifier  t1    = AstModifier  (Atom_Modifier  t1)
pattern Wildcard        = AstWildcard  (Atom_Wildcard)
pattern Number    t1    = AstNumber    (Atom_Number    t1)
pattern Str       t1    = AstStr       (Atom_Str       t1)
pattern Block     t1    = AstBlock     (Atom_Block     t1)
pattern Tokens    t1    = AstTokens    (Atom_Tokens    t1)
pattern Marker    t1    = AstMarker    (Atom_Marker    t1)
pattern LineBreak t1    = AstLineBreak (Atom_LineBreak t1)
pattern Comment   t1    = AstComment   (Atom_Comment   t1)
pattern Invalid   t1    = AstInvalid   (Atom_Invalid   t1)
pattern App       t1 t2 = AstApp       (Atom_App       t1 t2)
pattern Missing         = AstMissing   (Atom_Missing)
pattern List      t1    = AstList      (Atom_List      t1)

pattern SVar       t1    = SimpleAstVar       (Atom_Var       t1)
pattern SCons      t1    = SimpleAstCons      (Atom_Cons      t1)
pattern SOperator  t1    = SimpleAstOperator  (Atom_Operator  t1)
pattern SModifier  t1    = SimpleAstModifier  (Atom_Modifier  t1)
pattern SWildcard        = SimpleAstWildcard  (Atom_Wildcard)
pattern SNumber    t1    = SimpleAstNumber    (Atom_Number    t1)
pattern SStr       t1    = SimpleAstStr       (Atom_Str       t1)
pattern SBlock     t1    = SimpleAstBlock     (Atom_Block     t1)
pattern STokens    t1    = SimpleAstTokens    (Atom_Tokens    t1)
pattern SMarker    t1    = SimpleAstMarker    (Atom_Marker    t1)
pattern SLineBreak t1    = SimpleAstLineBreak (Atom_LineBreak t1)
pattern SComment   t1    = SimpleAstComment   (Atom_Comment   t1)
pattern SInvalid   t1    = SimpleAstInvalid   (Atom_Invalid   t1)
pattern SApp       t1 t2 = SimpleAstApp       (Atom_App       t1 t2)
pattern SMissing         = SimpleAstMissing   (Atom_Missing)
pattern SList      t1    = SimpleAstList      (Atom_List      t1)

deriving instance Show (Link' t) => Show (Var       t)
deriving instance Show (Link' t) => Show (Cons      t)
deriving instance Show (Link' t) => Show (Operator  t)
deriving instance Show (Link' t) => Show (Modifier  t)
deriving instance Show (Link' t) => Show (Wildcard  t)
deriving instance Show (Link' t) => Show (Number    t)
deriving instance Show (Link' t) => Show (Block     t)
deriving instance Show (Link' t) => Show (Tokens    t)
deriving instance Show (Link' t) => Show (Marker    t)
deriving instance Show (Link' t) => Show (LineBreak t)
deriving instance Show (Link' t) => Show (Comment   t)
deriving instance Show (Link' t) => Show (Invalid   t)
deriving instance Show (Link' t) => Show (App       t)
deriving instance Show (Link' t) => Show (Missing   t)
deriving instance Show (Link' t) => Show (List      t)
deriving instance Show (Link' t) => Show (StrChunk  t)
deriving instance (Show (Link' t), Show (Link t (StrChunk t))) => Show (Str t)

deriving instance Eq (Link' t) => Eq (Var       t)
deriving instance Eq (Link' t) => Eq (Cons      t)
deriving instance Eq (Link' t) => Eq (Operator  t)
deriving instance Eq (Link' t) => Eq (Modifier  t)
deriving instance Eq (Link' t) => Eq (Wildcard  t)
deriving instance Eq (Link' t) => Eq (Number    t)
deriving instance Eq (Link' t) => Eq (Block     t)
deriving instance Eq (Link' t) => Eq (Tokens    t)
deriving instance Eq (Link' t) => Eq (Marker    t)
deriving instance Eq (Link' t) => Eq (LineBreak t)
deriving instance Eq (Link' t) => Eq (Comment   t)
deriving instance Eq (Link' t) => Eq (Invalid   t)
deriving instance Eq (Link' t) => Eq (App       t)
deriving instance Eq (Link' t) => Eq (Missing   t)
deriving instance Eq (Link' t) => Eq (List      t)
deriving instance Eq (Link' t) => Eq (StrChunk  t)
deriving instance (Eq (Link' t), Eq (Link t (StrChunk t))) => Eq (Str t)

deriving instance Ord (Link' t) => Ord (Var       t)
deriving instance Ord (Link' t) => Ord (Cons      t)
deriving instance Ord (Link' t) => Ord (Operator  t)
deriving instance Ord (Link' t) => Ord (Modifier  t)
deriving instance Ord (Link' t) => Ord (Wildcard  t)
deriving instance Ord (Link' t) => Ord (Number    t)
deriving instance Ord (Link' t) => Ord (Block     t)
deriving instance Ord (Link' t) => Ord (Tokens    t)
deriving instance Ord (Link' t) => Ord (Marker    t)
deriving instance Ord (Link' t) => Ord (LineBreak t)
deriving instance Ord (Link' t) => Ord (Comment   t)
deriving instance Ord (Link' t) => Ord (Invalid   t)
deriving instance Ord (Link' t) => Ord (App       t)
deriving instance Ord (Link' t) => Ord (Missing   t)
deriving instance Ord (Link' t) => Ord (List      t)
deriving instance Ord (Link' t) => Ord (StrChunk  t)
deriving instance (Ord (Link' t), Ord (Link t (StrChunk t))) => Ord (Str t)


type instance Link Ast (StrChunk s) = Spanned (StrChunk s)
type instance Link Ast Struct = Spanned Ast

data Ast
    -- Identifiers
    = AstVar       (Var Ast)
    | AstCons      (Cons Ast)
    | AstOperator  (Operator Ast)
    | AstModifier  (Modifier Ast)
    | AstWildcard  (Wildcard Ast)

    -- Literals
    | AstNumber    (Number Ast)
    | AstStr       (Str Ast)

    -- Layouting
    | AstBlock     (Block Ast)
    | AstTokens    (Tokens Ast)
    | AstMarker    (Marker Ast)
    | AstLineBreak (LineBreak Ast)

    -- Docs
    | AstComment   (Comment Ast)

    -- Errors
    | AstInvalid   (Invalid Ast)

    | AstApp       (App Ast)
    | AstMissing   (Missing Ast)
    | AstList      (List Ast)
    deriving (Eq, Ord, Show)




type instance Link SimpleAst (StrChunk s) = StrChunk s
type instance Link SimpleAst Struct = SimpleAst

data SimpleAst
    -- Identifiers
    = SimpleAstVar      (Var SimpleAst)
    | SimpleAstCons      (Cons SimpleAst)
    | SimpleAstOperator  (Operator SimpleAst)
    | SimpleAstModifier  (Modifier SimpleAst)
    | SimpleAstWildcard  (Wildcard SimpleAst)

    -- Literals
    | SimpleAstNumber    (Number SimpleAst)
    | SimpleAstStr       (Str SimpleAst)

    -- Layouting
    | SimpleAstBlock     (Block SimpleAst)
    | SimpleAstTokens    (Tokens SimpleAst)
    | SimpleAstMarker    (Marker SimpleAst)
    | SimpleAstLineBreak (LineBreak SimpleAst)

    -- Docs
    | SimpleAstComment   (Comment SimpleAst)

    -- Errors
    | SimpleAstInvalid   (Invalid SimpleAst)

    | SimpleAstApp       (App SimpleAst)
    | SimpleAstMissing   (Missing SimpleAst)
    | SimpleAstList      (List SimpleAst)
    deriving (Eq, Ord)

instance Show SimpleAst where
    show = \case
        SimpleAstVar       t -> show t
        SimpleAstCons      t -> show t
        SimpleAstOperator  t -> show t
        SimpleAstModifier  t -> show t
        SimpleAstWildcard  t -> show t
        SimpleAstNumber    t -> show t
        SimpleAstStr       t -> show t
        SimpleAstBlock     t -> show t
        SimpleAstTokens    t -> show t
        SimpleAstMarker    t -> show t
        SimpleAstLineBreak t -> show t
        SimpleAstComment   t -> show t
        SimpleAstInvalid   t -> show t
        SimpleAstApp       t -> show t
        SimpleAstMissing   t -> show t
        SimpleAstList      t -> show t


simplify :: Ast -> SimpleAst
simplify = \case
    Var       t1    -> SVar       t1
    Cons      t1    -> SCons      t1
    Operator  t1    -> SOperator  t1
    Modifier  t1    -> SModifier  t1
    Wildcard        -> SWildcard
    Number    t1    -> SNumber    t1
    Str       t1    -> SStr       (simplifyStrChunk . unspan <$> t1)
    Block     t1    -> SBlock     (simplify . unspan <$> t1)
    Tokens    t1    -> STokens    (simplify . unspan <$> t1)
    Marker    t1    -> SMarker    t1
    LineBreak t1    -> SLineBreak t1
    Comment   t1    -> SComment   t1
    Invalid   t1    -> SInvalid   t1
    App       t1 t2 -> SApp       (simplify . unspan $ t1) (simplify . unspan $ t2)
    Missing         -> SMissing
    List      t1    -> SList      (simplify . unspan <$> t1)

simplifyStrChunk :: StrChunk Ast -> StrChunk SimpleAst
simplifyStrChunk = \case
    StrPlain   t                  -> StrPlain   t
    StrNewLine (Atom_LineBreak t) -> StrNewLine (Atom_LineBreak t)
    StrExpr    (Atom_Block t)     -> StrExpr    (Atom_Block (simplify . unspan <$> t))


------ FIXME vvv
makeLenses ''Result


dropOffset :: Spanned a -> Spanned a
dropOffset = span %~ CodeSpan.dropOffset
{-# INLINE dropOffset #-}


inheritCodeSpan2
    :: (Spanned Ast -> Spanned Ast -> Ast)
    -> (Spanned Ast -> Spanned Ast -> Spanned Ast)
inheritCodeSpan2 = \f t1 t2 -> let
    s1 = t1 ^. span
    s2 = t2 ^. span
    in Spanned (s1 <> s2) $! f (dropOffset t1) t2
{-# INLINE inheritCodeSpan2 #-}

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

app :: Spanned Ast -> Spanned Ast -> Spanned Ast
app = inheritCodeSpan2 $ \func arg -> App func arg
{-# INLINE app #-}

app2 :: Spanned Ast -> Spanned Ast -> Spanned Ast -> Spanned Ast
app2 = \f a b -> app (app f a) b
{-# INLINE app2 #-}

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

isOperator :: Ast -> Bool
isOperator = \case
    AstOperator {} -> True
    _ -> False
{-# INLINE isOperator #-}

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
--     | AstBlock     Block
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
