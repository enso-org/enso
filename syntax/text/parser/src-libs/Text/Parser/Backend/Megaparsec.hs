{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Parser.Backend.Megaparsec where

import Prologue

import Control.Lens.Utils.Wrapped  (unwrap', wrap')
import Control.Monad.State.Layered (StateT)
import Data.Text.Position          (Delta, HasPosition, Offset,
                                    Position (Position), column, incOffset,
                                    line, position, succOffset)
import Text.Megaparsec             (MonadParsec (..), ParsecT, Stream,
                                    chunkLength, getPosition, setPosition)
import Text.Megaparsec.Pos         (Pos, SourcePos, mkPos, sourceColumn,
                                    sourceLine, unPos)
-- import Text.Megaparsec.Prim        (getPosition)
-- import Text.Megaparsec.Prim        as MP

import qualified Control.Monad.State.Layered    as State
import qualified Language.Symbol.Operator.Assoc as Assoc
import qualified Language.Symbol.Operator.Prec  as Prec



-- === Instances === --

-- Delta
instance Convertible Pos Delta where convert = convert . unPos
instance Convertible Delta Pos where convert = mkPos . convert

-- Position
instance HasPosition SourcePos where
    position = lens (\sp   -> Position (convert $ sourceLine sp) (convert $ sourceColumn sp))
                    (\sp p -> sp {sourceLine = convert $ p ^. line, sourceColumn = convert $ p ^. column})

-- Position getter
instance (Stream s, Ord e) => State.Getter Position (ParsecT e s m) where
    get = view position <$> getPosition

instance (Stream s, Ord e) => State.Setter Position (ParsecT e s m) where
    put p = do
        pp <- getPosition
        setPosition $ pp & position .~ p

-- Other States

instance {-# OVERLAPPABLE #-} (State.Getter s m, Stream t) => State.Getter s (ParsecT e t m)
instance {-# OVERLAPPABLE #-} (State.Setter s m, Stream t) => State.Setter s (ParsecT e t m)


-- StateT
deriving instance MonadParsec e t m => MonadParsec e t (StateT s m)

-- Offset handling
instance {-# OVERLAPPING #-} MonadParsec e s m => MonadParsec e s (StateT Offset m) where
    failure           = wrap' .: failure
    label s           = wrapped %~ label s
    hidden            = wrapped %~ hidden
    try               = wrapped %~ try
    lookAhead         = wrapped %~ lookAhead
    notFollowedBy     = wrapped %~ notFollowedBy
    withRecovery f    = wrapped %~ withRecovery (unwrap' <$> f)
    observing         = wrapped %~ observing
    eof               = wrap' eof
    token             = ((<* succOffset) . wrap') .: token
    tokens f ts       = ((<* incOffset (convert $ chunkLength (Proxy :: Proxy s) ts)) . wrap') $ tokens f ts
    getParserState    = wrap' getParserState
    updateParserState = wrap' . updateParserState

-- Prec
instance (Prec.RelReader name m, Stream s) => Prec.RelReader name (ParsecT e s m)
instance (Prec.RelWriter name m, Stream s) => Prec.RelWriter name (ParsecT e s m)

-- Assoc
instance (Assoc.Reader name m, Stream s) => Assoc.Reader name (ParsecT e s m)
instance (Assoc.Writer name m, Stream s) => Assoc.Writer name (ParsecT e s m)

