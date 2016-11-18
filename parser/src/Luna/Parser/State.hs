{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Parser.State where

import           Luna.Prelude
--import qualified Luna.Data.ASTInfo    as ASTInfo
--import           Luna.Data.ASTInfo    (ASTInfo)
--import           Luna.Data.SourceMap  (SourceMap)
--import qualified Luna.Data.SourceMap  as SourceMap
--import           Luna.Parser.Operator (OperatorMap)
--import qualified Luna.IR.Namespace  as Namespace
--import           Luna.IR.Namespace  (Namespace, NamespaceMonad)
import qualified Data.List            as List
--import qualified Data.Maps            as Map
--import           Luna.DEP.AST.Comment     (Comment(..))
import qualified Control.Monad.State as State
--import qualified Luna.Data.StructInfo        as StructInfo
--import           Luna.IR.Name.Path       (QualPath)
import Luna.IR.Name
import qualified Control.Monad.Trans.State.Lazy as Lazy

import           Text.Parser.Char          (CharParsing)
import           Text.Parser.Combinators   (Parsing)
import           Text.Parser.Token         (TokenParsing, nesting, someSpace, semi, highlight, token)
import           Text.Trifecta.Combinators (DeltaParsing)

data State = State { --_info          :: ASTInfo
                                --_opFixity      :: OperatorMap
                              --, _sourceMap     :: SourceMap
                              --, _namespace     :: Namespace
                               _adhocReserved :: [MultiName]
                              --, _modPath       :: QualPath
                              } deriving (Show)

makeLenses ''State





---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Declarations === --

type    ParserState      = ParserStateT Identity
newtype ParserStateT m a = ParserStateT (State.StateT State m a)
                              deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                                       , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''ParserStateT


-- === Utils === --

runT  ::            ParserStateT m a -> State -> m (a, State)
evalT :: Monad m => ParserStateT m a -> State -> m a
execT :: Monad m => ParserStateT m a -> State -> m State

runT  = State.runStateT  . unwrap' ; {-# INLINE runT  #-}
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}
execT = State.execStateT . unwrap' ; {-# INLINE execT #-}

run  :: ParserState a -> State -> (a, State)
eval :: ParserState a -> State -> a
exec :: ParserState a -> State -> State

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: MonadParserState m => (State -> State) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadParserState m => (State -> (a, State)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modifyM :: MonadParserState m => (State -> m (a, State)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modify_ :: MonadParserState m => (State -> State) -> m ()
modify_ = modify . fmap ((),)
{-# INLINE modify_ #-}


-- === Instances === --

class Monad m => MonadParserState m where
    get :: m State
    put :: State -> m ()

instance Monad m => MonadParserState (ParserStateT m) where
    get = ParserStateT   State.get ; {-# INLINE get #-}
    put = ParserStateT . State.put ; {-# INLINE put #-}

instance State.MonadState s m => State.MonadState s (ParserStateT m) where
    get = ParserStateT $ lift   State.get ; {-# INLINE get #-}
    put = ParserStateT . lift . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadParserState m, MonadTrans t, Monad (t m)) => MonadParserState (t m) where
    get = lift get   ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<


deriving instance (MonadPlus m, Parsing      m) => Parsing      (ParserStateT m)
deriving instance (MonadPlus m, CharParsing  m) => CharParsing  (ParserStateT m)
deriving instance DeltaParsing m                => DeltaParsing (ParserStateT m)
--deriving instance (MonadPlus m, TokenParsing m) => TokenParsing (ParserStateT m)


-- FIXME[WD]: original implementation of monad transformers for TokenParsing are broken (they do not lift `token` method!)
instance (MonadPlus m, TokenParsing m) => TokenParsing (ParserStateT m) where
    nesting (ParserStateT p) = ParserStateT $ nesting p
    someSpace = ParserStateT someSpace
    semi      = ParserStateT semi
    highlight h (ParserStateT p) = ParserStateT $ highlight h p
    token (ParserStateT (Lazy.StateT m)) = ParserStateT $ Lazy.StateT $ token . m


--nesting (Lazy.StateT m) = Lazy.StateT $ nesting . m

--instance (TokenParsing m, MonadPlus m) => TokenParsing (Lazy.StateT s m) where
--  nesting (Lazy.StateT m) = Lazy.StateT $ nesting . m
--  {-# INLINE nesting #-}
--  someSpace = lift someSpace
--  {-# INLINE someSpace #-}
--  semi      = lift semi
--  {-# INLINE semi #-}
--  highlight h (Lazy.StateT m) = Lazy.StateT $ highlight h . m
--  {-# INLINE highlight #-}



------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------

--mk :: ASTInfo -> State
--mk i = def & info .~ i

addReserved words = adhocReserved %~ (++words)
delReserved words = adhocReserved %~ (flip (foldl (flip List.delete)) words)

--lastID            = view (info . ASTInfo.lastID)



--regParent id pid = mapStateVal $ namespace %~ Namespace.regParent id pid

--pushNewScope id = mapStateVal $ namespace %~ Namespace.pushNewScope id
--pushScope    id = mapStateVal $ namespace %~ Namespace.pushScope id
--popScope        = mapStateVal $ namespace %~ Namespace.popScope

--regVarName id name = do
--    pid <- getPid
--    withStructInfo $ StructInfo.regVarName pid id name

--regTypeName id name = do
--    pid <- getPid
--    withStructInfo $ StructInfo.regTypeName pid id name

--withStructInfo f = mapStateVal (namespace . Namespace.info %~ f)

--getStructInfo = view (namespace . Namespace.info) <$> get

withReserved words p = do
    s <- get
    let reserved = view adhocReserved s
    put $ (addReserved words s)
    ret <- p
    s   <- get
    put (s & adhocReserved .~ reserved)
    return ret

withCurrying p = p


--withNewScope id p = do
--    pushNewScope id
--    ret <- p
--    popScope
--    return ret


--withScope id p = do
--    pushScope id
--    ret <- p
--    popScope
--    return ret

--getModPath = view modPath <$> get

--setModPath mp = do
--    s <- get
--    put $ set modPath mp s

--getPid = do
--    mpid <- Namespace.head . view namespace <$> get
--    case mpid of
--        Nothing  -> fail "Internal parser error. Cannot optain pid."
--        Just pid -> return pid

--getScope  = view (namespace . Namespace.info . StructInfo.scope) <$> get
--getASTMap = view (namespace . Namespace.info . StructInfo.ast) <$> get



--registerID id = do
--    pid <- getPid
--    regParent id pid

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

-- FIXME[wd]: "Unnamed" string is an ugly hack for now
instance conf~() => Default State where
        def = State def  -- "Unnamed"


--instance (Functor m, Monad m) => NamespaceMonad (StateT State m) where
--    get = view namespace <$> State.get
--    put ns = do
--        s <- get
--        State.put $ set namespace ns s


