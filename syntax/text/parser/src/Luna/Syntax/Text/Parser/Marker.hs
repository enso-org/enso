{-# LANGUAGE UndecidableInstances #-}
{-# EXT      InlineAll            #-}

module Luna.Syntax.Text.Parser.Marker where

import Prologue hiding (Span, String, Type, span)

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified Luna.IR                     as IR
import qualified Luna.Syntax.Text.Lexer      as Lexer

import Data.Map.Strict (Map)



---------------------------------------
-- === Marker expression mapping === --
---------------------------------------

-- === Definition === --

type MarkerId = Word64

newtype MarkedExprMap = MarkedExprMap (Map MarkerId IR.SomeTerm)
    deriving (Show, Mempty, Default)

newtype UnmarkedExprs = UnmarkedExprs [IR.SomeTerm]
    deriving (Show, Mempty, Default)

makeLenses ''MarkedExprMap
makeLenses ''UnmarkedExprs


-- === Utils === --

addMarkedExpr :: State.Monad MarkedExprMap m => MarkerId -> IR.SomeTerm -> m ()
addMarkedExpr gid expr = State.modify_ @MarkedExprMap
                       $ wrapped . at gid .~ Just expr

addUnmarkedExpr :: State.Monad UnmarkedExprs m => IR.SomeTerm -> m ()
addUnmarkedExpr expr = State.modify_ @UnmarkedExprs $ wrapped %~ (expr :)



-------------------------
-- === TokenMarker === --
-------------------------

-- === Definition === --

type MarkerToken = Lexer.Token MarkerId
data MarkerState = MarkerState
    { _lastTokenMarker :: Maybe MarkerToken
    , _allMarkers      :: [MarkerId]
    } deriving (Show)
makeLenses ''MarkerState


-- === Utils === --

cleanLastTokenMarker :: State.Monad MarkerState m => m ()
cleanLastTokenMarker = State.modify_ @MarkerState $ lastTokenMarker .~ Nothing

newLastTokenMarker :: State.Monad MarkerState m => MarkerToken -> m ()
newLastTokenMarker m = State.modify_ @MarkerState
                     $ \s -> s & lastTokenMarker .~ Just m
                               & allMarkers      %~ (m ^. Lexer.element :)

getLastTokenMarker :: State.Monad MarkerState m => m (Maybe MarkerToken)
getLastTokenMarker = view lastTokenMarker <$> State.get @MarkerState

useLastTokenMarker :: State.Monad MarkerState m => m (Maybe MarkerToken)
useLastTokenMarker = getLastTokenMarker <* cleanLastTokenMarker


-- === Instances === --

instance Default MarkerState where def = MarkerState def def
