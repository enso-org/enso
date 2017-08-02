module Control.Lens.Aeson where

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON

import qualified Data.List as List
import GHC.Generics (Generic, Rep)


------------------------------------------
-- === JSON / Yaml conversion utils === --
------------------------------------------

options, optionsDropUnary :: JSON.Options
options          = JSON.defaultOptions { JSON.fieldLabelModifier = List.dropWhile (== '_')}
optionsDropUnary = options { JSON.unwrapUnaryRecords = True }

parse      :: (Generic a, JSON.GFromJSON   JSON.Zero (Rep a)) => JSON.Value -> JSON.Parser a
toEncoding :: (Generic a, JSON.GToEncoding JSON.Zero (Rep a)) => a -> JSON.Encoding
toJSON     :: (Generic a, JSON.GToJSON     JSON.Zero (Rep a)) => a -> JSON.Value
parse      = JSON.genericParseJSON  options
toEncoding = JSON.genericToEncoding options
toJSON     = JSON.genericToJSON     options

parseDropUnary      :: (Generic a, JSON.GFromJSON   JSON.Zero (Rep a)) => JSON.Value -> JSON.Parser a
toEncodingDropUnary :: (Generic a, JSON.GToEncoding JSON.Zero (Rep a)) => a -> JSON.Encoding
toJSONDropUnary     :: (Generic a, JSON.GToJSON     JSON.Zero (Rep a)) => a -> JSON.Value
parseDropUnary      = JSON.genericParseJSON  optionsDropUnary
toEncodingDropUnary = JSON.genericToEncoding optionsDropUnary
toJSONDropUnary     = JSON.genericToJSON     optionsDropUnary
