module Luna.Build.Dependency.ParserUtils where

import Prologue

import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Text as P

spaces :: P.Parser ()
spaces = P.space >> pure ()

