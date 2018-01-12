{-# LANGUAGE OverloadedStrings #-}

module Text.Parser.Expr where


import Luna.Prelude hiding (Symbol, UniSymbol)

import OCI.IR hiding (Expr, Atom, Term, State, IRBuilder, Var, number, string, var, expr, app, acc, grouped, blank, get, put)

import qualified Control.Monad.State as S
import           Control.Monad.State.Dependent
import           Text.Megaparsec      (SourcePos, getPosition, setPosition, unexpected)
import           Text.Megaparsec.Prim (MonadParsec)
import           Text.Megaparsec.Ext
import           Text.Parser.Combinators
import qualified Language.Symbol.Operator.Assoc as Assoc
import qualified Language.Symbol.Operator.Prec  as Prec
import           Language.Symbol
import           Data.Text.Position
import qualified Control.Monad as M


type Term s l a = Labeled l (ExprSymbol s a)

------------------------------
-- === Expression trees === --
------------------------------

-- === Definitions === --

data ExprTree n a = InfixNode  (Term Infix  n a) (ExprTree n a) (ExprTree n a)
                  | PrefixNode (Term Prefix n a) (ExprTree n a)
                  | SuffixNode (Term Suffix n a) (ExprTree n a)
                  | AtomNode   a

data Expregment n a = InfixSegment   (Term Infix  n a) (ExprTree n a)
                    | SuffixSegment (Term Suffix n a)


-- === Modification === --


-- FIXME[WD]: There should be a way to simplify the following code?
-- FIXME[WD]: Remove MonadFail dep
insertSegment :: (MonadFail m, Prec.RelReader n m, Assoc.Reader n m) => Expregment n a -> ExprTree n a -> m (ExprTree n a)
insertSegment seg tree = case seg of
    InfixSegment op stree -> case tree of
        AtomNode  _       -> return $ InfixNode op tree stree
        InfixNode op' l r -> Prec.readRel op' op >>= \case
            LT -> rightApp
            GT -> leftApp
            EQ -> ((,) <$> Assoc.read op <*> Assoc.read op') >>= \case
                (Assoc.Left , Assoc.Left ) -> leftApp
                (Assoc.Right, Assoc.Right) -> rightApp
                _                          -> M.fail "oh no!" -- TODO: err message
            where rightApp = InfixNode op' l <$> insertSegment seg r
                  leftApp  = return $ InfixNode op tree stree

        PrefixNode op' r -> Prec.readRel op' op >>= \case
            LT -> rightApp
            GT -> leftApp
            EQ -> ((,) <$> Assoc.read op <*> Assoc.read op') >>= \case
                (Assoc.Left , Assoc.Left ) -> leftApp
                (Assoc.Right, Assoc.Right) -> rightApp
                _                          -> M.fail "oh no!" -- TODO: err message
            where rightApp = PrefixNode op' <$> insertSegment seg r
                  leftApp  = return $ InfixNode op tree stree

        SuffixNode _ _ -> return $ InfixNode op tree stree
    SuffixSegment op -> case tree of
        AtomNode  _       -> return $ SuffixNode op tree
        InfixNode op' l r -> Prec.readRel op' op >>= \case
            LT -> InfixNode op' l <$> insertSegment seg r
            _  -> return $ SuffixNode op tree
        PrefixNode op' r -> Prec.readRel op' op >>= \case
            LT -> PrefixNode op' <$> insertSegment seg r
            _  -> return $ SuffixNode op tree
        SuffixNode {} -> return $ SuffixNode op tree


assembleExpr :: ExprTree n a -> a
assembleExpr = \case
    AtomNode   t      -> t
    InfixNode  op l r -> (op ^. body) (assembleExpr l) (assembleExpr r)
    PrefixNode op   r -> (op ^. body) (assembleExpr r)
    SuffixNode op   l -> (op ^. body) (assembleExpr l)



------------------
-- === UniSymbol === --
------------------

-- === Parser matches === --

matchPrefixM :: MonadParsec e s m => (Term Prefix n a -> m out) -> Labeled n (UniSymbol Expr a) -> m out
matchSuffixM :: MonadParsec e s m => (Term Suffix n a -> m out) -> Labeled n (UniSymbol Expr a) -> m out
matchInfixM  :: MonadParsec e s m => (Term Infix  n a -> m out) -> Labeled n (UniSymbol Expr a) -> m out
matchAtomM   :: MonadParsec e s m => (Term Atom   n a -> m out) -> Labeled n (UniSymbol Expr a) -> m out
matchPrefixM f (Labeled l s) = case s of Prefix a -> f (Labeled l a) ; _ -> expected "prefix operator"
matchSuffixM f (Labeled l s) = case s of Suffix a -> f (Labeled l a) ; _ -> expected "suffix operator"
matchInfixM  f (Labeled l s) = case s of Infix  a -> f (Labeled l a) ; _ -> expected "infix operator"
matchAtomM   f (Labeled l s) = case s of Atom   a -> f (Labeled l a) ; _ -> expected "term"

matchPrefix :: MonadParsec e s m => (Term Prefix n a -> out) -> Labeled n (UniSymbol Expr a) -> m out
matchSuffix :: MonadParsec e s m => (Term Suffix n a -> out) -> Labeled n (UniSymbol Expr a) -> m out
matchInfix  :: MonadParsec e s m => (Term Infix  n a -> out) -> Labeled n (UniSymbol Expr a) -> m out
matchAtom   :: MonadParsec e s m => (Term Atom   n a -> out) -> Labeled n (UniSymbol Expr a) -> m out
matchPrefix = matchPrefixM . fmap return
matchSuffix = matchSuffixM . fmap return
matchInfix  = matchInfixM  . fmap return
matchAtom   = matchAtomM   . fmap return



-------------------
-- === Token === --
-------------------

-- === Definition === --

data Token  t = Token { __pos :: FileOffset, __elem :: t } deriving (Show, Functor, Traversable, Foldable)
type Tokens t = [Token t]
makeLenses ''Token


-- === Construction === --

token :: Monad m => m a -> m (Token a)
token p = Token <$> pure mempty <*> p -- FIXME [WD]: we've been checking for FileOffset here and replaced that for now with mempty

tokenx :: a -> Token a
tokenx = Token mempty -- FIXME [WD]: we've been checking for FileOffset here and replaced that for now with mempty

untoken :: Token t -> t
untoken (Token _ t) = t


-- === Instances === --

instance Copointed Token where
    copoint = view token_elem



-------------------------
-- === TokenStream === --
-------------------------

-- === Definition === --

newtype TokenStream t m a = TokenStream (S.StateT (Tokens t) m a) deriving (Functor, Applicative, Monad, MonadTrans, Alternative, MonadPlus, MonadIO, MonadParsec e s, MonadFail)
makeWrapped ''TokenStream


-- === Running === --
evalTokenStreamT :: Monad m => TokenStream t m a -> Tokens t -> m a
evalTokenStreamT = S.evalStateT . unwrap

runTokenStreamT :: Monad m => TokenStream t m a -> Tokens t -> m (a, Tokens t)
runTokenStreamT = S.runStateT . unwrap


-- === MonadTokenStream === --

class MonadPlus m => MonadTokenStream t m | m -> t where
    getTokenStream :: m (Tokens t)
    putTokenStream :: Tokens t -> m ()

instance MonadPlus m => MonadTokenStream t (TokenStream t m) where
    getTokenStream = wrap   S.get
    putTokenStream = wrap . S.put


-- === Modification === --

modifyTokenStreamM  :: MonadTokenStream t m => (Tokens t -> m (a, Tokens t)) -> m a
modifyTokenStreamM_ :: MonadTokenStream t m => (Tokens t -> m    (Tokens t)) -> m ()
modifyTokenStream   :: MonadTokenStream t m => (Tokens t ->   (a, Tokens t)) -> m a
modifyTokenStream_  :: MonadTokenStream t m => (Tokens t ->       Tokens t)  -> m ()
modifyTokenStreamM_  = modifyTokenStreamM  . (fmap.fmap) ((),)
modifyTokenStream    = modifyTokenStreamM  . fmap return
modifyTokenStream_   = modifyTokenStreamM_ . fmap return
modifyTokenStreamM f = do
    s       <- getTokenStream
    (a, s') <- f s
    putTokenStream s'
    return a

pushToken :: MonadTokenStream t m => Token t -> m ()
pushToken = modifyTokenStream_ . (:)

popToken :: (MonadTokenStream t m) => m (Token t)
popToken = modifyTokenStreamM $ \case
    Token p s : ss -> pure (Token p s, ss) -- <$ put @FileOffset p -- FIXME [WD]: what to do?
    []             -> M.fail "No more tokens"


-- === Instances === --

instance Prec.RelReader name m => Prec.RelReader name (TokenStream t m)
instance Prec.RelWriter name m => Prec.RelWriter name (TokenStream t m)
instance Assoc.Writer   name m => Assoc.Writer   name (TokenStream t m)
instance Assoc.Reader   name m => Assoc.Reader   name (TokenStream t m)

instance MonadGetter s m => MonadGetter s (TokenStream t m)
instance MonadSetter s m => MonadSetter s (TokenStream t m)


--------------------------------
-- === Expression builder === --
--------------------------------

-- === Definition === --

type MonadSymStream n a = MonadTokenStream (Labeled n (UniSymbol Expr a))


-- === Syment accessing === --

popSym :: (MonadSymStream n a m) => m (Labeled n (UniSymbol Expr a))
popSym = untoken <$> popToken

nextSym :: (MonadSymStream n a m) => m (Labeled n (UniSymbol Expr a))
nextSym = popSym
-- nextSym = handleAffix =<< popSym

tryNextSym :: (MonadSymStream n a m) => r -> (Labeled n (UniSymbol Expr a) -> m r) -> m r
tryNextSym a f = tryBind a nextSym f

maybeToken :: (MonadTokenStream t m) => a -> (Token t -> m a) -> m a
maybeToken def f = (<|> pure def) $ do
    t <- popToken
    f t <|> (def <$ pushToken t)

maybeSym :: (MonadSymStream n a m) => r -> (Labeled n (UniSymbol Expr a) -> m r) -> m r
maybeSym def f = maybeToken def (f . untoken)


-- === Expression building === --


-- FIXME[WD]: Remove MonadFail dep
buildExpr :: (MonadFail m, Prec.RelReader n m, Assoc.Reader n m, MonadParsec e s m) => Tokens (Labeled n (UniSymbol Expr a)) -> m a
buildExpr = assembleExpr .: evalTokenStreamT buildExprTree

-- FIXME[WD]: Remove MonadFail dep
buildExprTree :: (MonadFail m, MonadSymStream n a m, Prec.RelReader n m, Assoc.Reader n m, MonadParsec e s m) => m (ExprTree n a)
buildExprTree = nextSym >>= \case
    Labeled _ (Atom (Symbol t)) -> buildExprTreeBody $ AtomNode t
    _                           -> error "TODO1"

-- FIXME[WD]: Remove MonadFail dep
buildExprTreeBody :: (MonadFail m, MonadSymStream n a m, Prec.RelReader n m, Assoc.Reader n m, MonadParsec e s m) => ExprTree n a -> m (ExprTree n a)
buildExprTreeBody tree = tryNextSym tree . matchInfixM $ \op ->
                         (nextSym >>=)   . matchAtomM  $ \t  ->
                         buildExprTreeBody =<< insertSegment (InfixSegment op $ AtomNode $ t ^. body) tree



-- FIXME[WD]: Remove MonadFail dep
buildExprTree_termApp :: (MonadFail m, MonadSymStream n a m, Prec.RelReader n m, Assoc.Reader n m, MonadParsec e s m) => Labeled n (ExprSymbol Infix a) -> m (ExprTree n a)
buildExprTree_termApp app = nextSym >>= \case
    Labeled _ (Atom (Symbol t)) -> buildExprTreeBody_termApp app $ AtomNode t
    Labeled l (Prefix op)       -> (nextSym >>=) . matchAtomM $ \t -> buildExprTreeBody_termApp app $ PrefixNode (Labeled l op) (AtomNode . unwrap $ unlabel t)
    _                           -> error "TODO2"

-- FIXME[WD]: Remove MonadFail dep
buildExprTreeBody_termApp :: (MonadFail m, MonadSymStream n a m, Prec.RelReader n m, Assoc.Reader n m, MonadParsec e s m) => Labeled n (ExprSymbol Infix a) -> ExprTree n a -> m (ExprTree n a)
buildExprTreeBody_termApp app tree = tryNextSym tree $ \(Labeled l s) -> case s of
    Atom   t  -> insertSegment (InfixSegment app . AtomNode $ t ^. body) tree >>= buildExprTreeBody_termApp app
    Infix  op -> (nextSym >>=) . matchAtomM $ \t -> go $ insertSegment (InfixSegment (Labeled l op) . AtomNode $ t ^. body)
    Prefix op -> (nextSym >>=) . matchAtomM $ \t -> go $ insertSegment (InfixSegment app . PrefixNode (Labeled l op) . AtomNode $ t ^. body)
    Suffix op -> go $ insertSegment (SuffixSegment $ Labeled l op)
    el        -> unexpected "TODO3"
    where go f = f tree >>= buildExprTreeBody_termApp app


-- FIXME[WD]: Remove MonadFail dep
buildExpr_termApp :: (MonadFail m, Prec.RelReader n m, Assoc.Reader n m, MonadParsec e s m) => Labeled n (ExprSymbol Infix a) -> Tokens (Labeled n (UniSymbol Expr a)) -> m a
buildExpr_termApp app = assembleExpr .: evalTokenStreamT (buildExprTree_termApp app)
