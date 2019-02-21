{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Data.Text.CodeBuilder.Builder where

import Prologue

import           Data.Text.CodeBuilder.Doc      (Doc)
import qualified Data.Text.CodeBuilder.Doc      as Doc
import           Data.Text.CodeBuilder.Tok      (Prec, Tok (Tok), doc,
                                                 precParens)
import qualified Data.Text.CodeBuilder.Tok      as Tok
import qualified Data.Text.Lazy                 as Text
import qualified Data.Text.Lazy.Builder         as Text
import           Language.Symbol.Operator.Assoc (Assoc)
import qualified Language.Symbol.Operator.Assoc as Assoc

import Control.Monad.State hiding (join, sequence)

----------------------------------------------------------------------
-- Code Builder
----------------------------------------------------------------------

newtype Builder s a = Builder { unBuilder :: State s a }
                    deriving (Monad, Applicative, Functor)

class (Monad m, Applicative m) => MonadTokBuilder s m | m -> s where
    getStyle :: m s
    putStyle :: s -> m ()

runBuilder :: Builder s a -> s -> a
runBuilder = evalState . unBuilder

data SimpleStyle = SimpleStyle deriving (Show)

instance {-# OVERLAPPABLE #-} (s ~ SimpleStyle) => Show (Builder s Tok) where
    show = Text.unpack . toLazyText . renderCode SimpleStyle


----------------------------------------------------------------------
-- Render styles
----------------------------------------------------------------------

renderStyled :: (MonadTokBuilder s m, Render s a) => a -> m Tok
renderStyled p = do
    s <- getStyle
    pure $ render s p


class Render style a where
    render :: style -> a -> Tok


renderCode :: s -> Builder s Tok -> Text.Builder
renderCode style f = Doc.render . view doc $ runBuilder f style

renderStr :: s -> Builder s Tok -> String
renderStr s = Text.unpack . toLazyText . renderCode s

----------------------------------------------------------------------
-- Combinators
----------------------------------------------------------------------

tok :: Prec -> Doc -> Builder s Tok
tok p d = pure $ Tok p d

parensed, bracked, braced, sbox, weak :: Builder s Tok -> Builder s Tok
parensed = fmap Tok.parensed
bracked  = fmap Tok.bracked
braced   = fmap Tok.braced
sbox     = fmap Tok.sbox
weak     = fmap Tok.weak

appWith :: Doc -> Assoc -> Prec -> Builder s Tok -> Builder s Tok
    -> Builder s Tok
appWith sep assoc aprec mbase marg = do
    base <- mbase
    arg  <- marg
    pure $ case assoc of
        Assoc.Left  -> Tok aprec $ base^.doc <> sep <> precParens aprec arg
        Assoc.Right -> Tok aprec $ precParens aprec base <> sep <> arg^.doc
        Assoc.None  -> error "Should not happen"

app :: Builder s Tok -> Builder s Tok -> Builder s Tok
app = appWith " " Assoc.Left 10

apps :: Builder s Tok -> [Builder s Tok] -> Builder s Tok
apps = foldl app

ifx :: Builder s Tok -> Builder s Tok -> Builder s Tok -> Builder s Tok
ifx ma ml mr = do
    Tok prec d <- ma
    l <- ml
    r <- mr
    pure . Tok prec $ precParens prec l <> " " <> d <> " " <> precParens prec r

tuple, list :: [Builder s Tok] -> Builder s Tok
tuple items = parensed $ fmap (intercalate ", ") $ sequence items
list  items = bracked  $ fmap (intercalate ", ") $ sequence items

(<+>) :: Builder s Tok -> Builder s Tok -> Builder s Tok
(<+>) = app

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance MonadTokBuilder s (Builder s) where
    getStyle = Builder $ get
    putStyle = Builder . put

instance IsString a => IsString (Builder s a) where
    fromString = pure . fromString

instance Convertible t a => Convertible t (Builder s a) where
    convert = pure . convert

instance Mempty    a => Mempty    (Builder s a) where mempty = pure mempty
instance Semigroup a => Semigroup (Builder s a) where a <> b = (<>) <$> a <*> b
