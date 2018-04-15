module Luna.Build.Dependency.Constraint where

import Prologue hiding (Constraint, Constraints, EQ, GT, LT, and)

import qualified Data.Map.Strict               as Map
import qualified Luna.Build.Dependency.Version as Version

import Data.Map.Strict                   (Map)
import Luna.Build.Dependency.ParserUtils (Parser, and, spaces)
import Luna.Build.Dependency.Version     (Version)
import Text.Megaparsec                   (choice, sepBy)
import Text.Megaparsec.Char              (string)

------------------------
-- === Constraint === --
------------------------

-- === Definition === --

type Constraints = Map Text [Constraint]
type Versions    = Map Text [Version]
type PackageSet  = Map Text Version

data ConstraintType = EQ | GT | LT | LE | GE deriving (Eq, Generic, Ord, Show)

data Constraint = Constraint
    { __conType :: !ConstraintType
    , __version :: !Version
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Constraint


-- === API === --

unpack :: (Eq a, Ord a) => Map a [b] -> [(a, b)]
unpack inputMap = concat . Map.elems $ Map.mapWithKey flatten inputMap
    where flatten key list = (key, ) <$> list

isEQPrerelease :: Constraint -> Bool
isEQPrerelease = \case (Constraint EQ ver) -> Version.isPrerelease ver
                       _                   -> False


-- === Instances === --

instance PrettyShow ConstraintType where
    prettyShow = \case EQ -> "=="
                       GT -> ">"
                       LT -> "<"
                       LE -> "<="
                       GE -> ">="

instance PrettyShow Constraint where
    prettyShow (Constraint ty ver) = prettyShow ty <> " " <> prettyShow ver



-------------------------------
-- === Parsing Functions === --
-------------------------------

-- === API === --

constraints :: Parser [Constraint]
constraints = constraint `sepBy` and

constraint :: Parser Constraint
constraint = Constraint <$> operator <* spaces <*> Version.version <* spaces

operator :: Parser ConstraintType
operator = choice [ EQ <$ string "=="
                  , LE <$ string "<="
                  , GE <$ string ">="
                  , GT <$ string ">"
                  , LT <$ string "<" ]

