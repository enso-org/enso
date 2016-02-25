{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.CodeBuilder.Builder
-- Copyright   :  (C) 2014 Flowbox
-- License     :  AllRightsReserved
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.Text.CodeBuilder.Builder where

import Prelude ()
import Prologue

import qualified Data.Text.CodeBuilder.Tok as Tok
import           Data.Text.CodeBuilder.Tok (Tok(Tok), Prec, doc, precParens)
import qualified Data.Text.CodeBuilder.Doc as Doc
import           Data.Text.CodeBuilder.Doc (Doc)
import qualified Data.Text.Lazy            as Text
import qualified Data.Text.Lazy.Builder    as Text
import           Data.Text.Lazy.Builder    (toLazyText)

import Control.Monad.State hiding (join)

----------------------------------------------------------------------
-- Code Builder
----------------------------------------------------------------------

data Assoc = L | R deriving (Show, Generic)

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
    return $ render s p


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

appWith :: Doc -> Assoc -> Prec -> Builder s Tok -> Builder s Tok -> Builder s Tok
appWith sep assoc aprec mbase marg = do
    base <- mbase
    arg  <- marg
    return $ case assoc of
        L -> Tok aprec $ base^.doc <> sep <> precParens aprec arg
        R -> Tok aprec $ precParens aprec base <> sep <> arg^.doc

app :: Builder s Tok -> Builder s Tok -> Builder s Tok
app = appWith " " L 10

apps :: Builder s Tok -> [Builder s Tok] -> Builder s Tok
apps = foldl app

ifx :: Builder s Tok -> Builder s Tok -> Builder s Tok -> Builder s Tok
ifx ma ml mr = do
    a@(Tok prec d) <- ma
    l <- ml
    r <- mr
    return . Tok prec $ precParens prec l <> " " <> d <> " " <> precParens prec r

tuple, list :: [Builder s Tok] -> Builder s Tok
tuple items = parensed $ fmap (intercalate ", ") $ sequence items
list  items = bracked  $ fmap (intercalate ", ") $ sequence items

(<+>) = app

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance MonadTokBuilder s (Builder s) where
    getStyle = Builder $ get
    putStyle = Builder . put

instance IsString a => IsString (Builder s a) where
    fromString = pure . fromString

instance FromText a => FromText (Builder s a) where
    fromText = pure . fromText

instance Monoid a => Monoid (Builder s a) where
    mempty        = return mempty
    a `mappend` b = do
        va <- a
        vb <- b
        return $ va <> vb