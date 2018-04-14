module Luna.Build.Dependency.Constraint where

import Prologue hiding (Constraint, Constraints, and, EQ, GT, LT)

import qualified Data.Map.Strict               as Map
import qualified Luna.Build.Dependency.Version as Version

import Data.Map.Strict                   (Map)
import Luna.Build.Dependency.ParserUtils (spaces, and)
import Luna.Build.Dependency.Version     (Version)
import Text.Megaparsec                   (string, sepBy, choice)
import Text.Megaparsec.Text              (Parser)

------------------------
-- === Constraint === --
------------------------

-- === Definition === --

type Constraints = Map Text [Constraint]
type Versions    = Map Text [Version]
type PackageSet  = Map Text Version

data ConstraintType
    = EQ
    | GT
    | LT
    | LE
    | GE
    deriving (Eq, Generic, Ord)

data Constraint = Constraint
    { __conType :: !ConstraintType
    , __version :: !Version
    } deriving (Eq, Generic, Ord)
makeLenses ''Constraint

-- === API === --

unpack :: (Eq a, Ord a) => Map a [b] -> [(a, b)]
unpack map = concat $ Map.elems $ Map.mapWithKey convert map
    where convert key list = (key, ) <$> list

isEQPrerelease :: Constraint -> Bool
isEQPrerelease = \case (Constraint EQ ver) -> Version.isPrerelease ver
                       _                             -> False

-- === Instances === --

instance Show ConstraintType where
    show EQ = "=="
    show GT = ">"
    show LT = ">"
    show LE = "<="
    show GE = ">="

instance Show Constraint where
    show (Constraint ty ver) = show ty <> " " <> show ver

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

