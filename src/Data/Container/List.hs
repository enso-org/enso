module Data.Container.List where

import Data.Container.Class

import Prelude

import           Control.Lens 
import qualified GHC.Exts       as GHC
import qualified Data.Vector    as V
import qualified Data.Text      as Text
import           Data.Text      (Text)
import qualified Data.Text.Lazy as LazyText

type LazyText = LazyText.Text


class FromList       l where fromList         :: [Item l] -> l
                             default fromList :: (GHC.IsList l, GHC.Item l ~ Item l)
                                              => [Item l] -> l
                             fromList = GHC.fromList

class ToList         l where toList         :: l -> [Item l]
                             default toList :: (GHC.IsList l, GHC.Item l ~ Item l)
                                            => l -> [Item l]
                             toList = GHC.toList

type IsList l = (FromList l, ToList l)

asList :: IsList a => Iso' a [Item a]
asList = iso toList fromList 


-- === Instances ===

type instance Item Text = Char
instance ToList    Text where toList   = Text.unpack
instance FromList  Text where fromList = Text.pack  


type instance Item LazyText = Char
instance ToList    LazyText where toList   = LazyText.unpack
instance FromList  LazyText where fromList = LazyText.pack  