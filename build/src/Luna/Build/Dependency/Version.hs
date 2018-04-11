module Luna.Build.Dependency.Version
    ( PrereleaseType(..)
    , Prerelease(..)
    , Version(..)
    , SolverVersion(..)
    , parseVersion
    , versionToSolverVersion
    , solverVersionToVersion
    , solverVersionAsList
    , version
    , prerelease
    , prereleaseType
    ) where

import Prologue

import qualified Text.Read as R

import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Text as P

import qualified Data.Char as C

-- Versioning in Luna follows the following convention:
--      major.minor.patch-prerelease.version
--
-- The prerelease is optional.

data PrereleaseType
    = Alpha
    | Beta
    | RC
    deriving (Eq, Generic, Ord)

instance Show PrereleaseType where
    show Alpha = "alpha"
    show Beta  = "beta"
    show RC    = "rc"

data Prerelease = Prerelease
    { __prType  :: !PrereleaseType
    , __version :: !Int
    } deriving (Eq, Generic, Ord)
makeLenses ''Prerelease

instance Show Prerelease where
    show (Prerelease ty ver) = (show ty) <> "." <> (show ver)

data Version = Version
    { __major      :: !Int
    , __minor      :: !Int
    , __patch      :: !Int
    , __prerelease :: !(Maybe Prerelease)
    } deriving (Eq, Generic)
makeLenses ''Version

instance Ord Version where
    v1 <= v2 = (versionToSolverVersion v1) <= (versionToSolverVersion v2)

instance Show Version where
    show (Version maj min patch pr) = nums <> (showPre pr)
        where nums    = (show maj) <> "." <> (show min) <> "." <> (show patch)
              showPre = \case
                  Nothing -> ""
                  Just pre -> "-" <> show pre

versionToSolverVersion :: Version -> SolverVersion
versionToSolverVersion (Version maj min pat pre) = case pre of
    Nothing -> SolverVersion maj min pat 3 0 -- 3 indicates no prerelease
    Just (Prerelease ty ver) -> SolverVersion maj min pat (toNum ty) ver
    where toNum Alpha = 0
          toNum Beta  = 1
          toNum RC    = 2

data SolverVersion = SolverVersion
    { __major      :: !Int
    , __minor      :: !Int
    , __patch      :: !Int
    , __prerelease :: !Int
    , __preVersion :: !Int
    } deriving (Eq, Generic, Ord)
makeLenses ''SolverVersion

instance Show SolverVersion where
    show sv = show $ solverVersionToVersion sv

solverVersionToVersion :: SolverVersion -> Version
solverVersionToVersion (SolverVersion maj min pat pre preVer) =
    (Version maj min pat preResult)
    where preResult = case pre of
            0 -> Just (Prerelease Alpha preVer)
            1 -> Just (Prerelease Beta preVer)
            2 -> Just (Prerelease RC preVer)
            _ -> Nothing

solverVersionAsList :: SolverVersion -> [Int]
solverVersionAsList (SolverVersion maj min pat pre preV) =
    [maj, min, pat, pre, preV]

parseVersion :: Text -> Maybe Version
parseVersion tx = case P.runParser version "" tx of
                      Left err -> Nothing
                      Right res -> Just res

-----------------------
-- Parsing Functions --
-----------------------

version :: P.Parser Version
version = do
    major <- natural
    minor <- P.option 0 (dot *> natural)
    patch <- P.option 0 (dot *> natural)
    prerelease <- P.optional (P.char '-' *> prerelease)
    if major + minor + patch == 0 then fail msg else
        pure $ Version major minor patch prerelease
    where msg = "Not all components of the version can be zero."

prerelease :: P.Parser Prerelease
prerelease = Prerelease <$> prereleaseType <* dot <*> natural

prereleaseType :: P.Parser PrereleaseType
prereleaseType = fromString <$> p
    where fromString "alpha" = Alpha
          fromString "beta"  = Beta
          fromString "rc"    = RC
          -- `_` case not needed as it is only applied on a successful parse
          p = P.string "alpha" <|> P.string "beta" <|> P.string "rc"

dot :: P.Parser Char
dot = P.char '.'

natural :: P.Parser Int
natural = R.read <$> some P.digitChar

