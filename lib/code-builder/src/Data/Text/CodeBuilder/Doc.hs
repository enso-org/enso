{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoStrict                  #-}
{-# LANGUAGE NoStrictData              #-}
{-# LANGUAGE TemplateHaskell           #-}


module Data.Text.CodeBuilder.Doc where

import Prologue hiding (Text)

import           Control.Monad.State
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy         as Text
import qualified Data.Text.Lazy.Builder as Text
import           GHC.Int                (Int64)


------------------------------------------------------------------------
-- Indentation handling
------------------------------------------------------------------------

data IndentState = IndentState
    { _indent :: !Int64
    , _col    :: !Int64
    } deriving (Show)
makeLenses ''IndentState


-- === Utils ===

indented :: MonadState IndentState m => (Int64 -> Int64) -> m a -> m a
indented f p = do
    s <- get
    put $ s & indent %~ f
    ret <- p
    put s
    pure ret

getIndent :: MonadState IndentState m => m Int64
getIndent = view indent <$> get

getIndentTxt :: (IsString a, MonadState IndentState m) => m a
getIndentTxt =
    (\i -> fromString $ replicate ((4 :: Integer) * fromIntegral i) ' ')
    <$> getIndent

modCol :: MonadState IndentState m => (Int64 -> Int64) -> m ()
modCol f = do
    s <- get
    put $ s & col %~ f

-- === Instances ===

instance Default IndentState where
    def = IndentState 0 0

------------------------------------------------------------------------
-- Doc
------------------------------------------------------------------------

data Doc = Empty
         | Text !Int64 Text
         | Line
         | Nest (Int64 -> Int64) Doc
         | Cat Doc Doc
         deriving (Generic)

-- === Utils ===

between :: Semigroup a => a -> a -> a -> a
between l r a = l <> a <> r

parensed :: (Semigroup a, IsString a) => a -> a
parensed = between "(" ")"

line :: Doc
line = Line

(</>) :: Doc -> Doc -> Doc
a </> b = a <> Line <> b

nested :: Doc -> Doc
nested = Nest (+1)

render :: Doc -> Text.Builder
render = flip evalState (def :: IndentState) . go where
    go = \case
        Empty    -> pure $ ""
        Text l t -> Text.fromLazyText t <$ modCol (+l)
        Line     -> do
            ind <- view indent <$> get
            modCol (const ind)
            pure $ fromString $ "\n" <> concat
                (replicate ((fromInteger . toInteger $ ind) :: Integer) "    ")
        Cat  a b -> (<>) <$> go a <*> go b
        Nest f d -> indented f $ go d

-- === Instances ===

instance Show Doc where
    show = \case
        Text _ t -> "Text " <> show t
        Line     -> "Line"
        Nest _ d -> "Nest " <> show d
        Cat  a b -> "Cat (" <> show a <> ") (" <> show b <> ")"
        Empty    -> ""

instance Mempty Doc where
    mempty = Empty

instance Semigroup Doc where
    a <> b = case a of
        Empty -> b
        _     -> case b of
            Empty -> a
            _     -> Cat a b


instance Convertible Text Doc where
    convert t = Text (Text.length t) t

instance IsString Doc where
    fromString = fromLazyText . fromString

