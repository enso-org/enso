{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.CodeBuilder.Doc
-- Copyright   :  (C) 2014 Flowbox
-- License     :  AllRightsReserved
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.Text.CodeBuilder.Doc where

import Prologue hiding (Empty)

import           GHC.Int (Int64)
import           Control.Monad.State
import qualified Data.Text.Lazy         as Text
import qualified Data.Text.Lazy.Builder as Text
import           Data.Text.Lazy.Builder   (toLazyText, fromLazyText)

------------------------------------------------------------------------
-- Indentation handling
------------------------------------------------------------------------

data IndentState = IndentState { _indent :: !Int64
                               , _col    :: !Int64
                               } deriving (Show)

makeLenses ''IndentState


-- === Utils ===

indented f p = do
    s <- get
    put $ s & indent %~ f
    ret <- p
    put s
    return ret

getIndent = view indent <$> get

getIndentTxt = (\i -> fromString $ replicate (4 * fromIntegral i) ' ') <$> getIndent

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

between l r a = l <> a <> r
parensed = between "(" ")"
line = Line
a </> b = a <> Line <> b
nested = Nest (+1)

render :: Doc -> Text.Builder
render = flip evalState (def :: IndentState) . go where
    go = \case
        Empty    -> pure $ ""
        Text l t -> fromLazyText t <$ modCol (+l)
        Line     -> do
            ind <- view indent <$> get
            modCol (const ind)
            pure $ fromString $ "\n" ++ concat (replicate (fromInteger . toInteger $ ind) "    ")
        Cat  a b -> (<>) <$> go a <*> go b
        Nest f d -> indented f $ go d

-- === Instances ===

instance Show Doc where
    show = \case
        Text _ t -> "Text " ++ show t
        Line     -> "Line"
        Nest _ d -> "Nest " ++ show d
        Cat  a b -> "Cat (" ++ show a ++ ") (" ++ show b ++ ")"

instance Monoid Doc where
    mempty = Empty
    a `mappend` b = case a of
        Empty -> b
        _     -> case b of
            Empty -> a
            _ -> Cat a b

instance FromText Doc where
    fromText t = Text (Text.length t) t

instance IsString Doc where
    fromString = fromText . fromString

