{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances            #-}


module Luna.Parser.Indent where

import Control.Lens
import GHC.Int
import Prelude.Luna

import           Control.Monad.Catch       (MonadMask, MonadCatch, MonadThrow)
import qualified Control.Monad.State       (State)
import qualified Control.Monad.State       as State
import           Control.Monad.State       (MonadState)
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Trifecta.Combinators
import           Text.Trifecta.Delta       (column)
import           Text.Parser.LookAhead


-------------------------
-- === IndentState === --
-------------------------

data IndentState = IndentState { _col :: Int64, _blocks :: [Int64] } deriving (Show)

makeLenses ''IndentState



---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Declarations === --

type    Indent      = IndentT Identity
newtype IndentT m a = IndentT (State.StateT IndentState m a)
                              deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                                       , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''IndentT


-- === Utils === --

runT  ::            IndentT m a -> IndentState -> m (a, IndentState)
evalT :: Monad m => IndentT m a -> IndentState -> m a
execT :: Monad m => IndentT m a -> IndentState -> m IndentState

runT  = State.runStateT  . unwrap' ; {-# INLINE runT  #-}
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}
execT = State.execStateT . unwrap' ; {-# INLINE execT #-}

run  :: Indent a -> IndentState -> (a, IndentState)
eval :: Indent a -> IndentState -> a
exec :: Indent a -> IndentState -> IndentState

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: MonadIndent m => (IndentState -> IndentState) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadIndent m => (IndentState -> (a, IndentState)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modifyM :: MonadIndent m => (IndentState -> m (a, IndentState)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modify_ :: MonadIndent m => (IndentState -> IndentState) -> m ()
modify_ = modify . fmap ((),)
{-# INLINE modify_ #-}


-- === Instances === --

class Monad m => MonadIndent m where
    get :: m IndentState
    put :: IndentState -> m ()

instance Monad m => MonadIndent (IndentT m) where
    get = IndentT   State.get ; {-# INLINE get #-}
    put = IndentT . State.put ; {-# INLINE put #-}

instance State.MonadState s m => State.MonadState s (IndentT m) where
    get = IndentT $ lift   State.get ; {-# INLINE get #-}
    put = IndentT . lift . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadIndent m, MonadTrans t, Monad (t m)) => MonadIndent (t m) where
    get = lift get   ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<




-- === Utils === --


evalT' :: Monad m => IndentT m a -> m a
evalT' = flip evalT def


----with f m = do
----  s   <- get
----  put $ f s
----  ret <- m
----  put s
----  return ret

--with f p = pushBlockWith f *> p <* popBlock

--currentIdent = view col <$> get

--pushBlockWith f = do
--    i <- view col <$> get
--    pushBlock $ f i

--pushBlock i = do
--    s <- get
--    let cIdent = s^.col
--    put $ s & col   .~ i
--            & stack %~ (cIdent:)

--popBlock = do
--    s <- get
--    let (x:xs) = s^.stack
--    put $ s & col   .~ x
--            & stack .~ xs

--startBlock = pushBlock =<< getIndent
--endBlock   = popBlock

--withPos p = do
--  c <- getIndent
--  with (const c) p

--withDiscarded = with (const 0)

--indentSegment p = many (checkIndent >> p)

--indentBlock p = spaces *> indented *> withPos (indentSegment p)


--block = string "a" <|> (foldl (++) "" <$> (char ':' *> spaces *> indentBlock block))


--getIndent = column <$> position

--mapIndent f err = do
--  c <- getIndent
--  s <- get
--  when (not $ c `f` view col s) $ fail err


--indented          = mapIndent (>)  "not indented"
--indentedOrEq      = mapIndent (>=) "not indented"
--checkIndent       = mapIndent (==) "indentation doesn't match"
--checkIndented     = mapIndent (>)  "indentation doesn't match"
--checkIndentedOrEq = mapIndent (>=) "indentation doesn't match"

------------------------------------------------------------------------
---- IndentStateT
------------------------------------------------------------------------

--newtype IndentStateT s m a = IndentStateT { getState :: State.StateT s m a }
--        deriving (Monad, MonadPlus, Applicative, Alternative, Functor, DeltaParsing,
--                  TokenParsing, CharParsing, Parsing, MonadIO, LookAheadParsing)


--instance MonadState x m => MonadState x (IndentStateT s m) where
--  get = IndentStateT . lift $ State.get
--  put = IndentStateT . lift . State.put


--instance MonadTrans (IndentStateT s) where
--  lift a = IndentStateT $ lift a

----class MonadTrans t where
----    -- | Lift a computation from the argument monad to the constructed monad.
----    lift :: (Monad m) => m a -> t m a


--class MonadIndentState s m | m -> s where
--    get :: m s
--    put :: s -> m ()

--runIndentStateT m s = State.runStateT (getState m) s


--instance Monad m => MonadIndentState s (IndentStateT s m) where
--    get = IndentStateT $ State.get
--    put = IndentStateT . State.put

--type MonadIndent m = MonadIndentState State m



-- === Instances === --

instance Default IndentState where
    def = IndentState 0 def





