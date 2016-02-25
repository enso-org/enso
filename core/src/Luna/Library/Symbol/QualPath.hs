module Luna.Library.Symbol.QualPath where

import qualified Data.String.Utils as String
import           FastString
import qualified FastString
import           Prologue


type QualPath = [FastString]


mk :: ToString s => s -> QualPath
mk = map FastString.mkFastString . String.split "." . toString


instance IsString QualPath where
    fromString = mk
