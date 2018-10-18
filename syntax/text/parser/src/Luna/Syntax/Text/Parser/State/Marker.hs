{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Parser.State.Marker where

import Prologue hiding (Span, String, Type, span)

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified Luna.IR                     as IR
import qualified Luna.Syntax.Text.Lexer      as Lexer

import Data.Map.Strict (Map)



-----------------------------
-- === Marker Term Map === --
-----------------------------

-- === Definition === --

type ID = Word64

newtype TermMap = TermMap (Map ID IR.SomeTerm)
    deriving (Show, Mempty, Default)

newtype TermOrphanList = TermOrphanList [IR.SomeTerm]
    deriving (Show, Mempty, Default)

makeLenses ''TermMap
makeLenses ''TermOrphanList


-- === Utils === --

register :: State.Monad TermMap m => ID -> IR.SomeTerm -> m ()
register gid expr = State.modify_ @TermMap $ wrapped . at gid .~ Just expr
{-# INLINE register #-}

registerOrphan :: State.Monad TermOrphanList m => IR.SomeTerm -> m ()
registerOrphan expr = State.modify_ @TermOrphanList $ wrapped %~ (expr :)
{-# INLINE registerOrphan #-}



-- -- -------------------------
-- -- -- === TokenMarker === --
-- -- -------------------------

-- -- -- === Definition === --

-- data Token = Token
--     { __info    :: Lexer.TokenInfo
--     , _markerID :: ID
--     } deriving (Show)
-- makeLenses ''Token

-- data State = State
--     { _lastTokenMarker :: !(Maybe Token)
--     , _allMarkers      :: ![ID]
--     } deriving (Show)
-- makeLenses ''State


-- -- -- === Utils === --

-- clearLast :: State.Monad State m => m ()
-- clearLast = State.modify_ @State $ lastTokenMarker .~ Nothing
-- {-# INLINE clearLast #-}

-- setLast :: State.Monad State m => Token -> m ()
-- setLast m = State.modify_ @State
--           $ \s -> s & lastTokenMarker .~ Just m
--                     & allMarkers      %~ (m ^. markerID :)
-- {-# INLINE setLast #-}

-- getLast :: State.Monad State m => m (Maybe Token)
-- getLast = view lastTokenMarker <$> State.get @State
-- {-# INLINE getLast #-}

-- getAndClearLast :: State.Monad State m => m (Maybe Token)
-- getAndClearLast = getLast <* clearLast
-- {-# INLINE getAndClearLast #-}


-- -- === Instances === --

-- instance Lexer.IsToken Token where
--     info = token_info
--     {-# INLINE info #-}

-- instance Default State where
--     def = State def def
--     {-# INLINE def #-}
