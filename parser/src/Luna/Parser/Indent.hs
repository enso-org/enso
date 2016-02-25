{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances            #-}


module Luna.Parser.Indent where

--import Control.Lens
--import Data.Default
--import GHC.Int
--import Prelude

--import           Control.Applicative
--import           Control.Monad
--import           Control.Monad.Trans
--import qualified Control.Monad.State       as State
--import           Control.Monad.State (MonadState)
--import           Text.Parser.Char
--import           Text.Parser.Combinators
--import           Text.Parser.Token
--import           Text.Trifecta.Combinators
--import           Text.Trifecta.Delta (column)
--import           Text.Parser.LookAhead


------------------------------------------------------------------------
---- Inner state
------------------------------------------------------------------------

--data State = State { _col :: Int64, _stack :: [Int64] } deriving Show

--makeLenses ''State


------------------------------------------------------------------------
---- Utils
------------------------------------------------------------------------

--parser p = evalIndentStateT p (def :: State)

--evalIndentStateT m s = do
--    ~(a, _) <- runIndentStateT m s
--    return a


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

------------------------------------------------------------------------
---- Instances
------------------------------------------------------------------------

--instance Default State where
--    def = State 0 def

