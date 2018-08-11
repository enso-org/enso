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
import qualified Data.Text.Position                   as Position
import qualified Data.Text.Span                       as Span
import qualified Luna.Syntax.Text.Lexer               as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol        as Lexer
import qualified Luna.Syntax.Text.Parser.State.Marker as Marker
import qualified Luna.Syntax.Text.Scope               as Scope

import Control.Monad.State.Layered              (StatesT)
import Data.Text.Position                       (FileOffset (..))
import Data.Text.Position                       (Delta, Position)
import Luna.Syntax.Text.Parser.State.LastOffset (LastOffset (LastOffset))
import Text.Parser.State.Indent                 (Indent)

import Data.Parser             hiding (Result, Token, endOfInput)
import Text.Parser.Combinators (some)



type Text = Text.Text32
data SyntaxVersion = Syntax1 | Syntax2 deriving (Show)


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


type Parser = StatesT '[SyntaxVersion, Indent, Position, LastOffset, CodeSpanRange, Marker.State, FileOffset, Scope.Scope] Parsec.Parser



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




data App      a = App      { base  :: a, arg :: a           } deriving (Show)
data Block    a = Block    { lines :: NonEmpty a            } deriving (Show)
data Cons     a = Cons     { name  :: Name                  } deriving (Show)
data ExprList a = ExprList { exprs :: NonEmpty (NonEmpty a) } deriving (Show)
data Invalid  a = Invalid  { desc  :: Invalid.Symbol        } deriving (Show)
data Marker   a = Marker   { mid   :: Int                   } deriving (Show)
data Modifier a = Modifier { name  :: Name                  } deriving (Show)
data Operator a = Operator { name  :: Name                  } deriving (Show)
data Unify    a = Unify    { left  :: a, right :: a         } deriving (Show)
data Var      a = Var      { name  :: Name                  } deriving (Show)
data Wildcard a = Wildcard                                    deriving (Show)
data Missing  a = Missing                                     deriving (Show)

-- OLD
data Function a = Function { nm   :: a, args :: [a], body :: a } deriving (Show)

data Ast a
    = AstApp      (App a)
    | AstBlock    (Block a)
    | AstCons     (Cons a)
    | AstExprList (ExprList a)
    | AstInvalid  (Invalid a)
    | AstMarker   (Marker a)
    | AstModifier (Modifier a)
    | AstOperator (Operator a)
    | AstUnify    (Unify a)
    | AstVar      (Var a)
    | AstWildcard (Wildcard a)
    | AstMissing  (Missing a)

    -- OLD
    | AstFunction (Function a)
    deriving (Show)


type Unspanned = Ast Spanned
data Spanned   = Spanned
    { _span :: CodeSpan
    , _ast  :: Unspanned
    } deriving (Show)
makeLenses ''Spanned


inheritCodeSpan2
    :: (Spanned -> Spanned -> Unspanned) -> (Spanned -> Spanned -> Spanned)
inheritCodeSpan2 = \f t1 t2 -> let
    s1 = t1 ^. span
    s2 = t2 ^. span
    in Spanned (s1 <> s2) $ f t1 t2
{-# INLINE inheritCodeSpan2 #-}

inheritCodeSpanList
    :: (NonEmpty Spanned -> Unspanned) -> (NonEmpty Spanned -> Spanned)
inheritCodeSpanList = \f ts -> let
    (s :| ss) = view span <$> ts
    in Spanned (foldl' (<>) s ss) $ f ts
{-# INLINE inheritCodeSpanList #-}

inheritCodeSpanList'
    :: NonEmpty Spanned -> Unspanned -> Spanned
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

-- spanned2 :: Parser a -> Parser (CodeSpan, a)
-- spanned2 = \parser -> do
--     start   <- getParserOffset
--     out     <- parser
--     end     <- getParserOffset
--     lastOff <- getLastOffset

--     WhiteSpace wst nextOff <- whiteSpace2
--     putLastOffset nextOff

--     let bodyLen   = end - start
--         totalSpan = bodyLen + nextOff
--         realSpan  = Span.leftSpacedSpan lastOff bodyLen

--     case wst of
--         SingleLine  -> Position.incColumn totalSpan
--         MultiLine i -> Position.succLine >> Position.incColumn i

--     pure (CodeSpan realSpan realSpan, out)
-- {-# INLINE spanned2 #-}

spanned :: Parser a -> Parser (CodeSpan, a)
spanned = \parser -> do
    start   <- getParserOffset
    out     <- parser
    end     <- getParserOffset
    lastOff <- getLastOffset
    nextOff <- whiteSpace

    let bodyLen   = end - start
        totalSpan = bodyLen + nextOff
        realSpan  = Span.leftSpacedSpan lastOff bodyLen

    Position.incColumn totalSpan
    pure (CodeSpan realSpan realSpan, out)
{-# INLINE spanned #-}

newline :: Parser ()
newline = do
    len <- eol
    off <- whiteSpace
    lastOff <- getLastOffset
    putLastOffset $! lastOff + len + off
    Position.succLine
    Position.incColumn off

{-# INLINE newline #-}

    -- lastCodeSpan <- unwrap <$> State.get @CodeSpanRange
    -- fileOffStart <- unwrap <$> State.get @FileOffset
    -- marker       <- Marker.getLast
    -- State.put @CodeSpanRange $ wrap fileOffStart
    -- out          <- p
    -- fileOffEnd   <- unwrap <$> State.get @FileOffset
    -- lastOffset   <- getLastOffset
    -- _ <- lexeme -- TODO
    -- let end       = fileOffEnd   - lastOffset
    --     off       = fileOffStart - lastCodeSpan
    --     emptySpan = Span.leftSpacedSpan mempty mempty
    --     realLen   = max 0 (end - fileOffStart)
    --     newRange  = max end fileOffStart
    --     realSpan  = Span.leftSpacedSpan off realLen
    --     viewSpan  = case marker of
    --         Nothing -> realSpan
    --         Just m  -> Span.leftSpacedSpan
    --                    (off - m ^. Lexer.span - m ^. Lexer.offset) realLen
    -- State.put @CodeSpanRange $ convert newRange
    -- pure (CodeSpan realSpan viewSpan, out)

-- data WhiteSpace = WhiteSpace
--     { _wsType      :: WhiteSpaceType
--     , _totalOffset :: Delta
--     }

-- data WhiteSpaceType
--     = SingleLine
--     | MultiLine {- last line offset -} Delta
-- data WhiteSpace
--     = SingleLine {- size -} Int
--     | MultiLine  {- size -} Int {- last line offset -} Int

-- whiteSpace2 :: Parser WhiteSpace
-- whiteSpace2 = parser where
--     line       = convert . Text.length <$> takeMany ' '
--     otherLines = multiple ((,) <$> eol <*> line)
--     multiple p = p <**> option id (mergeTups <$> multiple p)
--     mergeTups  = \(!a, !b) (!a', !b') -> (a + a' + b', b)
--     parser     = line <**> option (WhiteSpace SingleLine)
--                  ((\(!b, !c) a -> WhiteSpace (MultiLine c) (a + b)) <$> otherLines)
-- {-# INLINE whiteSpace2 #-}

lexeme :: Parser a -> Parser a
lexeme = \p -> p <* whiteSpace
{-# INLINE lexeme #-}

whiteSpace :: Parser Delta
whiteSpace = do
    len <- convert . Text.length <$> takeMany ' '
    putLastOffset len
    pure len
{-# INLINE whiteSpace #-}

computeSpan :: Parser Unspanned -> Parser Spanned
computeSpan = \p -> uncurry Spanned <$> spanned p
{-# INLINE computeSpan #-}

eol :: Parser Delta
eol = (1 <$ n) <|> (2 <$ rn) where
    n  = token '\n'
    r  = token '\r'
    rn = r >> n
{-# INLINE eol #-}

eolStartChars :: [Char]
eolStartChars = ['\n', '\r', '\ETX']
{-# INLINE eolStartChars #-}


-- === Smart constructors === --

app :: Spanned -> Spanned -> Spanned
app = inheritCodeSpan2 $ \base arg -> AstApp $ App base arg
{-# INLINE app #-}

block :: NonEmpty Spanned -> Spanned
block = inheritCodeSpanList $ \lines -> AstBlock $ Block lines
{-# INLINE block #-}

cons' :: Name -> Unspanned
cons' = \name -> AstCons $ Cons name
{-# INLINE cons' #-}

exprList :: NonEmpty (NonEmpty Spanned) -> Spanned
exprList = inheritCodeSpanListx $ \exprs -> AstExprList $ ExprList exprs
{-# INLINE exprList #-}

inheritCodeSpanListx
    :: (NonEmpty (NonEmpty Spanned) -> Unspanned) -> (NonEmpty (NonEmpty Spanned) -> Spanned)
inheritCodeSpanListx = \f ts -> let
    start = view span (neHead $ neHead ts)
    end   = view span (neLast $ neLast ts)
    in Spanned (start <> end) $ f ts
{-# INLINE inheritCodeSpanListx #-}


invalid' :: Invalid.Symbol -> Unspanned
invalid' = \desc -> AstInvalid $ Invalid desc
{-# INLINE invalid' #-}

marker' :: Int -> Unspanned
marker' = \id -> AstMarker $ Marker id
{-# INLINE marker' #-}

modifier' :: Name -> Unspanned
modifier' = \name -> AstModifier $ Modifier name
{-# INLINE modifier' #-}

operator' :: Name -> Unspanned
operator' = \name -> AstOperator $ Operator name
{-# INLINE operator' #-}

unify :: Spanned -> Spanned -> Spanned
unify = inheritCodeSpan2 $ \left right -> AstUnify $ Unify left right
{-# INLINE unify #-}

var' :: Name -> Unspanned
var' = \name -> AstVar $ Var name
{-# INLINE var' #-}



-- function :: Name -> [Spanned] -> Spanned -> Spanned
-- function = \name args block -> let
--     lst = case args of
--         []       -> block :| []
--         (a : as) -> a :| (as <> [block])
--     in inheritCodeSpanList' lst $ AstFunction $ Function name args block
-- {-# INLINE function #-}

function' :: Spanned -> [Spanned] -> Spanned -> Unspanned
function' = \name args block -> let
    lst = case args of
        []       -> block :| []
        (a : as) -> a :| (as <> [block])
    in AstFunction $ Function name args block
{-# INLINE function' #-}



wildcard' :: Unspanned
wildcard' = AstWildcard Wildcard
{-# INLINE wildcard' #-}

missing' :: Unspanned
missing' = AstMissing Missing
{-# INLINE missing' #-}

missing :: Spanned
missing = Spanned mempty $ AstMissing Missing
{-# INLINE missing #-}




buildIR :: Parser.IRBMonad m => Spanned -> m SomeTerm
buildIR = \(Spanned cs ast)   -> case ast of
    AstVar  (Var  name)       -> IR.var'  name
    AstCons (Cons name)       -> IR.cons' name []
    AstWildcard {}            -> IR.blank'
    AstInvalid (Invalid a)    -> IR.invalid' a
    AstExprList (ExprList ss) -> IR.exprList' =<< mapM buildIR (neFlatten ss)
    AstApp  (App  base arg) -> do
        base' <- buildIR base
        arg'  <- buildIR arg
        IR.app' base' arg'
    AstFunction (Function name args body) -> do
        name' <- buildIR name
        args' <- mapM buildIR args
        body' <- buildIR body
        IR.function' name' args' body'

    x -> error $ "TODO: " <> show x
{-# INLINE buildIR #-}

neFlatten :: NonEmpty (NonEmpty a) -> [a]
neFlatten = \(t :| ts) -> flt t <> (concat $ flt <$> ts) where
    flt (a :| as) = a : as
{-# INLINE neFlatten #-}



-- === Markers === --

markerBegin, markerEnd :: Char
markerBegin = '«'
markerEnd   = '»'
{-# INLINE markerBegin #-}
{-# INLINE markerEnd   #-}

parseMarker :: Parser Spanned
parseMarker = computeSpan parser where
    correct   = AstMarker . Marker <$> decimal
    incorrect = invalid' Invalid.InvalidMarker <$ takeTill (== markerEnd)
    parser    = token markerBegin *> (correct <|> incorrect) <* token markerEnd
{-# INLINE parseMarker #-}



digitChar :: Parser Int
digitChar = do
    char <- anyToken
    let n = Char.ord char
    if n >= 48 && n <= 57 then pure (n - 48) else fail "not digit"
{-# INLINE digitChar #-}

decimal :: Parser Int
decimal = digitsToDec <$> some digitChar
{-# INLINE decimal #-}

digitsToDec :: NonEmpty Int -> Int
digitsToDec = \(s :| ss) -> digitsToDec' s ss
{-# INLINE digitsToDec #-}

digitsToDec' :: Int -> [Int] -> Int
digitsToDec' = \i -> \case
    []     -> i
    (s:ss) -> digitsToDec' (i * 10 + s) ss


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
