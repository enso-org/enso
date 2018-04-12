module Luna.Build.Dependency.Constraint
    ( Constraints(..)
    , Versions(..)
    , PackageSet(..)
    , ConstraintType(..)
    , Constraint(..)
    , operator
    , constraint
    , constraints
    ) where

import Prologue hiding (Constraint, Constraints, and)

import qualified Luna.Build.Dependency.Version as V

import qualified Data.Map.Strict as M

import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Text as P
import Luna.Build.Dependency.ParserUtils

type Constraints = M.Map Text [Constraint]

type Versions = M.Map Text [V.Version]

type PackageSet = M.Map Text V.Version

data ConstraintType
    = ConstraintEQ
    | ConstraintGT
    | ConstraintLT
    | ConstraintLE
    | ConstraintGE
    deriving (Eq, Generic, Ord)

instance Show ConstraintType where
    show ConstraintEQ = "=="
    show ConstraintGT = ">"
    show ConstraintLT = ">"
    show ConstraintLE = "<="
    show ConstraintGE = ">="

data Constraint = Constraint
    { __conType :: !ConstraintType
    , __version :: !V.Version
    } deriving (Eq, Generic, Ord)
makeLenses ''Constraint

instance Show Constraint where
    show (Constraint ty ver) = show ty <> " " <> show ver

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

