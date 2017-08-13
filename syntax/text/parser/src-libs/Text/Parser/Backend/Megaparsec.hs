{-# LANGUAGE UndecidableInstances #-}

module Text.Parser.Backend.Megaparsec where

import Prologue_old

import Text.Megaparsec.Prim (getPosition)
import Text.Megaparsec.Prim as MP
import Text.Megaparsec.Pos  (SourcePos, sourceLine, sourceColumn, Pos, unPos, unsafePos)
import Text.Megaparsec (ParsecT, ErrorComponent)

import Control.Monad.State.Dependent
import Data.Text.Position
import qualified Language.Symbol.Operator.Assoc as Assoc
import qualified Language.Symbol.Operator.Prec  as Prec


-- === Instances === --

-- Delta
instance Convertible Pos Delta where convert = wrap . unsafeConvert . unPos
instance Convertible Delta Pos where convert = unsafePos . convert . unwrap

-- Position
instance HasPosition SourcePos where
    position = lens (\sp   -> Position (convert $ sourceLine sp) (convert $ sourceColumn sp))
                    (\sp p -> sp {sourceLine = convert $ p ^. line, sourceColumn = convert $ p ^. column})

-- Position getter
instance (ErrorComponent e, Stream s) => MonadGetter Position (ParsecT e s m) where
    get'   = view position <$> getPosition

instance (ErrorComponent e, Stream s) => MonadSetter Position (ParsecT e s m) where
    put' p = do
        pp <- getPosition
        setPosition $ pp & position .~ p

-- Other States

instance {-# OVERLAPPABLE #-} (MonadGetter s m, ErrorComponent e, Stream t) => MonadGetter s (ParsecT e t m)
instance {-# OVERLAPPABLE #-} (MonadSetter s m, ErrorComponent e, Stream t) => MonadSetter s (ParsecT e t m)


-- StateT
deriving instance MonadParsec e t m => MonadParsec e t (StateT s m)

-- Offset handling
instance {-# OVERLAPPING #-} MonadParsec e s m => MonadParsec e s (StateT Offset m) where
    failure           = wrap' .:. failure
    label s           = wrapped %~ label s
    hidden            = wrapped %~ hidden
    try               = wrapped %~ try
    lookAhead         = wrapped %~ lookAhead
    notFollowedBy     = wrapped %~ notFollowedBy
    withRecovery f    = wrapped %~ withRecovery (unwrap' <$> f)
    observing         = wrapped %~ observing
    eof               = wrap' eof
    token             = ((<* succOffset) . wrap') .: token
    tokens f ts       = ((<* incOffset (convert $ length ts)) . wrap') $ tokens f ts
    getParserState    = wrap' getParserState
    updateParserState = wrap' . updateParserState

-- Prec
instance (Prec.RelReader name m, ErrorComponent e, Stream s) => Prec.RelReader name (ParsecT e s m)
instance (Prec.RelWriter name m, ErrorComponent e, Stream s) => Prec.RelWriter name (ParsecT e s m)

-- Assoc
instance (Assoc.Reader name m, ErrorComponent e, Stream s) => Assoc.Reader name (ParsecT e s m)
instance (Assoc.Writer name m, ErrorComponent e, Stream s) => Assoc.Writer name (ParsecT e s m)
