module Data.Sequence.Class where


import Prelude hiding (dropWhile)

import Data.Item

import           Data.Char      (isSpace)
import qualified Data.List      as List
import           Data.Text      (Text)
import qualified Data.Text      as Text
import qualified Data.Text.Lazy as LazyText


type LazyText = LazyText.Text


----------------------
-- === Sequence === --
----------------------

-- === Definition === --

class Sequence a where
    dropWhile :: (Item a -> Bool) -> a -> a

class Sequence a => FiniteSequence a where
    dropWhileEnd :: (Item a -> Bool) -> a -> a
    dropAround   :: (Item a -> Bool) -> a -> a

    dropAround f = dropWhileEnd f . dropWhile f


-- === Utils === --

strip, stripStart, stripEnd :: (FiniteSequence a, Item a ~ Char) => a -> a
strip      = dropAround   isSpace
stripStart = dropWhile    isSpace
stripEnd   = dropWhileEnd isSpace


-- === Default instances === --

instance Sequence       [a] where dropWhile    = List.dropWhile
instance FiniteSequence [a] where dropWhileEnd = List.dropWhileEnd

instance Sequence       Text where dropWhile    = Text.dropWhile
instance FiniteSequence Text where dropWhileEnd = Text.dropWhileEnd
                                   dropAround   = Text.dropAround

instance Sequence       LazyText where dropWhile    = LazyText.dropWhile
instance FiniteSequence LazyText where dropWhileEnd = LazyText.dropWhileEnd
                                       dropAround   = LazyText.dropAround