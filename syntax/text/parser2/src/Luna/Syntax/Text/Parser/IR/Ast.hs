{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.IR.Ast where

import qualified Prelude  as P
import           Prologue hiding (Text, imp, seq, some, span, takeWhile)

import qualified Data.Text32                        as Text
import qualified Luna.IR                            as IR
import qualified Luna.IR.Term.Ast.Invalid           as Invalid
import qualified Luna.Syntax.Text.Parser.Pass.Class as Parser

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

import Data.Parser             hiding (Result, Token, endOfInput)
import Text.Parser.Combinators (some)



type Text = Text.Text32
data SyntaxVersion = Syntax1 | Syntax2 deriving (Show)




data Spanned a = Spanned
    { _span :: CodeSpan
    , _ast  :: a
    } deriving (Functor, Show)
makeLenses ''Spanned


unspan :: Spanned a -> a
unspan = \(Spanned _ a) -> a
{-# INLINE unspan #-}


-----------------------
-- === Blacklist === --
-----------------------

-- === Definition === --

data Blacklist = Blacklist
    { _unknownBlacklist :: Set Char
    , _operators        :: Set Name
    }
makeLenses ''Blacklist


-- === API === --

withBlacklistedUnknown :: State.Monad Blacklist m => Char -> m a -> m a
withBlacklistedUnknown = \a
     -> State.withModified @Blacklist (unknownBlacklist %~ Set.insert a)
{-# INLINE withBlacklistedUnknown #-}


withBlacklistedOperator :: State.Monad Blacklist m => Name -> m a -> m a
withBlacklistedOperator = \a -> State.withModified @Blacklist (operators %~ Set.insert a)
{-# INLINE withBlacklistedOperator #-}


checkBlacklistedUnknown :: Char -> Parser ()
checkBlacklistedUnknown = \a -> do
    blacklisted <- Set.member a . view unknownBlacklist <$> State.get @Blacklist
    when_ blacklisted $ fail "Blacklisted"
{-# INLINE checkBlacklistedUnknown #-}

checkBlacklistedOperator :: Name -> Parser ()
checkBlacklistedOperator = \a -> do
    blacklisted <- Set.member a . view operators <$> State.get @Blacklist
    when_ blacklisted $ fail "Blacklisted"
{-# INLINE checkBlacklistedOperator #-}



-- === Instances === --

instance Mempty Blacklist where
    mempty = Blacklist mempty mempty
    {-# INLINE mempty #-}

instance Default Blacklist where
    def = mempty
    {-# INLINE def #-}





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


type Parser = StatesT '[Result, SyntaxVersion, Blacklist, Indent, Position, LastOffset, CodeSpanRange, Marker.State, FileOffset, Scope.Scope] Parsec.Parser



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

type S a = ExpandField Simple a

type instance ExpandField Simple a = ExpandFieldSimple a

type family ExpandFieldSimple a where
    ExpandFieldSimple (NonEmpty a)   = NonEmpty (ExpandFieldSimple a)
    ExpandFieldSimple [a]            = [ExpandFieldSimple a]
    ExpandFieldSimple Delta          = Delta
    ExpandFieldSimple Int            = Int
    ExpandFieldSimple Bool           = Bool
    ExpandFieldSimple Name           = Name
    ExpandFieldSimple Text           = Text
    ExpandFieldSimple Invalid.Symbol = Invalid.Symbol

    ExpandFieldSimple Ast            = Spanned Ast
    ExpandFieldSimple Expr           = Spanned Expr


data App      = App      { base  :: S Ast, arg :: S Ast         } deriving (Show)
data Block    = Block    { lines :: S (NonEmpty Ast)            } deriving (Show)
data Cons     = Cons     { name  :: S Name                      } deriving (Show)
data Expr     = Expr     { line  :: S (NonEmpty Ast)            } deriving (Show)
data Grouped  = Grouped  { body  :: S Ast                       } deriving (Show)
data Invalid  = Invalid  { desc  :: S Invalid.Symbol            } deriving (Show)
data List     = List     { elems :: S [Ast]                     } deriving (Show)
data Modifier = Modifier { name  :: S Name                      } deriving (Show)
data Operator = Operator { name  :: S Name                      } deriving (Show)
data Unify    = Unify    { left  :: S Ast, right :: S Ast       } deriving (Show)
data Var      = Var      { name  :: S Name                      } deriving (Show)
data Wildcard = Wildcard                                          deriving (Show)
data Missing  = Missing                                           deriving (Show)
data Record   = Record   { isNative :: S Bool , name  :: S Name
                         , params   :: S [Ast], conss :: S [Ast]
                         , decls    :: S [Ast]                  } deriving (Show)

data NewLine  = NewLine  { indent   :: S Delta                  } deriving (Show)
data Comment  = Comment  { text     :: S Text                   } deriving (Show)
data Marker   = Marker   { markerID :: S Int                    } deriving (Show)


-- OLD
data Function = Function { nm   :: S Ast, args :: S [Ast], body :: S Ast } deriving (Show)

data Ast
    = AstApp      App
    | AstBlock    Block
    | AstCons     Cons
    | AstExpr     Expr
    | AstGrouped  Grouped
    | AstInvalid  Invalid
    | AstList     List
    | AstModifier Modifier
    | AstOperator Operator
    | AstUnify    Unify
    | AstVar      Var
    | AstWildcard Wildcard
    | AstMissing  Missing

    | AstNewLine  NewLine
    | AstComment  Comment
    | AstMarker   Marker

    -- OLD
    | AstFunction Function
    deriving (Show)


------ FIXME vvv
makeLenses ''Result




inheritCodeSpan2
    :: (Spanned Ast -> Spanned Ast -> Ast) -> (Spanned Ast -> Spanned Ast -> Spanned Ast)
inheritCodeSpan2 = \f t1 t2 -> let
    s1 = t1 ^. span
    s2 = t2 ^. span
    in Spanned (s1 <> s2) $ f t1 t2
{-# INLINE inheritCodeSpan2 #-}

inheritCodeSpanList
    :: (NonEmpty (Spanned a) -> b) -> (NonEmpty (Spanned a) -> (Spanned b))
inheritCodeSpanList = \f ts -> let
    (s :| ss) = view span <$> ts
    in Spanned (foldl' (<>) s ss) $ f ts
{-# INLINE inheritCodeSpanList #-}

inheritCodeSpanList'
    :: NonEmpty (Spanned Ast) -> Ast -> (Spanned Ast)
inheritCodeSpanList' = \ts -> let
    (s :| ss) = view span <$> ts
    in Spanned (foldl' (<>) s ss)
{-# INLINE inheritCodeSpanList' #-}


neLast :: NonEmpty a -> a
neLast = \case
    (a :| []) -> a
    (_ :| (a : as)) -> neLast $ a :| as
{-# INLINE neLast #-}

neHead :: NonEmpty a -> a
neHead = \(a :| _) -> a
{-# INLINE neHead #-}


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

computeSpan :: Parser Ast -> Parser (Spanned Ast)
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

app :: Spanned Ast -> Spanned Ast -> Spanned Ast
app = inheritCodeSpan2 $ \base arg -> AstApp $ App base arg
{-# INLINE app #-}

-- block :: NonEmpty (Spanned Ast) -> Spanned Ast
-- block = inheritCodeSpanList $ \lines -> AstBlock $ Block lines
-- {-# INLINE block #-}

cons' :: Name -> Ast
cons' = \name -> AstCons $ Cons name
{-# INLINE cons' #-}

-- expr :: NonEmpty (Spanned Ast) -> Spanned Ast
-- expr = inheritCodeSpanList $ \toks -> AstExpr $ Expr toks
-- {-# INLINE expr #-}

-- grouped' :: Spanned Ast -> Ast
-- grouped' = \body -> AstGrouped $ Grouped body
-- {-# INLINE grouped' #-}

invalid' :: Invalid.Symbol -> Ast
invalid' = \desc -> AstInvalid $ Invalid desc
{-# INLINE invalid' #-}

-- list' :: [Spanned Ast] -> Ast
-- list' = \elems -> AstList $ List elems
-- {-# INLINE list' #-}

marker' :: Int -> Ast
marker' = \id -> AstMarker $ Marker id
{-# INLINE marker' #-}

modifier' :: Name -> Ast
modifier' = \name -> AstModifier $ Modifier name
{-# INLINE modifier' #-}

operator' :: Name -> Ast
operator' = \name -> AstOperator $ Operator name
{-# INLINE operator' #-}

-- unify :: Spanned Ast -> Spanned Ast -> Spanned Ast
-- unify = inheritCodeSpan2 $ \left right -> AstUnify $ Unify left right
-- {-# INLINE unify #-}

var' :: Name -> Ast
var' = \name -> AstVar $ Var name
{-# INLINE var' #-}


nl' :: Delta -> Ast
nl' = \indent -> AstNewLine $ NewLine indent
{-# INLINE nl' #-}

comment' :: Text -> Ast
comment' = \txt -> AstComment $ Comment txt
{-# INLINE comment' #-}


-- function' :: Spanned Ast -> [Spanned Ast] -> Spanned Ast -> Ast
-- function' = \name args block -> let
--     lst = case args of
--         []       -> block :| []
--         (a : as) -> a :| (as <> [block])
--     in AstFunction $ Function name args block
-- {-# INLINE function' #-}



wildcard' :: Ast
wildcard' = AstWildcard Wildcard
{-# INLINE wildcard' #-}

-- missing' :: Ast
-- missing' = AstMissing Missing
-- {-# INLINE missing' #-}

-- missing :: Spanned Ast
-- missing = Spanned mempty $ AstMissing Missing
-- {-# INLINE missing #-}



buildIR :: forall m. Parser.IRBMonad m => Spanned Ast -> m SomeTerm
buildIR = \(Spanned cs ast)  -> case ast of
    AstVar  (Var  name)      -> IR.var'  name
    AstCons (Cons name)      -> IR.cons' name []
    AstWildcard {}           -> IR.blank'
    AstInvalid (Invalid a)   -> IR.invalid' a
    AstExpr    (Expr a)      -> IR.exprList' =<< mapM buildIR (convert a)
    --     (els :: [SomeTerm]) <- ((:) <$> run s <*> mapM runX ss)
    --     IR.exprList' els
    --     where
    --     run  (Spanned _ (Expr     a)) =                 IR.exprList' =<< mapM buildIR (convert a)
    --     runX (Spanned _ (Expr     a)) = IR.grouped' =<< IR.exprList  =<< mapM buildIR (convert a)
    AstApp  (App  base arg) -> do
        base' <- buildIR base
        arg'  <- buildIR arg
        IR.app' base' arg'
    AstFunction (Function name args body) -> do
        name' <- buildIR name
        args' <- mapM buildIR args
        body' <- buildIR body
        IR.function' name' args' body'
    AstMissing _ -> IR.missing'
    AstGrouped (Grouped a) -> IR.grouped' =<< buildIR a
    AstOperator (Operator a) -> IR.var' a

    x -> error $ "TODO: " <> show x
{-# INLINE buildIR #-}





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
