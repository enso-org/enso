module Luna.Build.Dependency.Constraint
    ( ConstraintMap(..)
    , ConstraintType(..)
    , Constraint(..)
    , operator
    , constraint
    , constraints
    , V.versionToSolverVersion
    , V.solverVersionToVersion
    , V.solverVersionAsList
    ) where

import Prologue hiding (Constraint, and)

import qualified Luna.Build.Dependency.Version as V

import qualified Data.Map.Strict as M

import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Text as P
import Luna.Build.Dependency.ParserUtils

type ConstraintMap = M.Map Text [Constraint]

data ConstraintType
    = ConstraintEQ
    | ConstraintGT
    | ConstraintLT
    | ConstraintLE
    | ConstraintGE
    deriving (Eq, Generic, Ord, Show)

data Constraint = Constraint
    { __conType :: !ConstraintType
    , __version :: !V.Version
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Constraint

-----------------------
-- Parsing Functions --
-----------------------

constraints :: P.Parser [Constraint]
constraints = constraint `P.sepBy` and

constraint :: P.Parser Constraint
constraint = Constraint <$> operator <* spaces
          <*> V.version <* spaces

operator :: P.Parser ConstraintType
operator = P.choice
    [ ConstraintEQ <$ P.string "=="
    , ConstraintLE <$ P.string "<="
    , ConstraintGE <$ P.string ">="
    , ConstraintGT <$ P.string ">"
    , ConstraintLT <$ P.string "<" ]

and :: P.Parser ()
and = spaces *> P.string "&&" *> spaces

