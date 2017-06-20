module Control.Lens.Aeson where

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Aeson.Encoding as JSON

import qualified Data.List as List
import GHC.Generics (Generic, Rep)


------------------------------------------
-- === JSON / Yaml conversion utils === --
------------------------------------------

lensJSONOptions :: JSON.Options
lensJSONOptions = JSON.defaultOptions { JSON.fieldLabelModifier = List.dropWhile (== '_')
                                      , JSON.unwrapUnaryRecords = True
                                      }

lensJSONParse      :: (Generic a, JSON.GFromJSON   JSON.Zero (Rep a)) => JSON.Value -> JSON.Parser a
lensJSONToEncoding :: (Generic a, JSON.GToEncoding JSON.Zero (Rep a)) => a -> JSON.Encoding
lensJSONToJSON     :: (Generic a, JSON.GToJSON     JSON.Zero (Rep a)) => a -> JSON.Value
lensJSONParse      = JSON.genericParseJSON  lensJSONOptions
lensJSONToEncoding = JSON.genericToEncoding lensJSONOptions
lensJSONToJSON     = JSON.genericToJSON     lensJSONOptions
