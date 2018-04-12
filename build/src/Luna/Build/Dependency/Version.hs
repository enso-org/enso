module Luna.Build.Dependency.Version
    ( PrereleaseType(..)
    , Prerelease(..)
    , Version(..)
    , isPrerelease
    , version
    , prerelease
    , prereleaseType
    ) where

import Prologue

import qualified Text.Read as R

import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Text as P

import qualified Data.Char as C
import Data.Word (Word64)

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
    , __version :: !Word64
    } deriving (Eq, Generic, Ord)
makeLenses ''Prerelease

instance Show Prerelease where
    show (Prerelease ty ver) = (show ty) <> "." <> (show ver)

data Version = Version
    { __major      :: !Word64
    , __minor      :: !Word64
    , __patch      :: !Word64
    , __prerelease :: !(Maybe Prerelease)
    } deriving (Eq, Generic)
makeLenses ''Version

instance Ord Version where
    (Version maj1 min1 pat1 pre1) < (Version maj2 min2 pat2 pre2) =
        if maj1 < maj2 then True
        else if maj1 > maj2 then False
        else if min1 < min2 then True
        else if min1 > min2 then False
        else if pat1 < pat2 then True
        else if pat1 > pat2 then False else pre1 `preLT` pre2
        where preLT :: Maybe Prerelease -> Maybe Prerelease -> Bool
              preLT l r = case l of
                Just (Prerelease ty1 ver1) ->
                    case r of
                        Just (Prerelease ty2 ver2) ->
                            if ty1 < ty2 then True
                            else if ty1 > ty2 then False
                            else if ver1 < ver2 then True else False
                        Nothing -> True
                Nothing -> False

    v1 <= v2 = (v1 < v2) || (v1 == v2)

instance Show Version where
    show (Version maj min patch pr) = nums <> (showPre pr)
        where nums    = (show maj) <> "." <> (show min) <> "." <> (show patch)
              showPre = \case
                  Nothing -> ""
                  Just pre -> "-" <> show pre

isPrerelease :: Version -> Bool
isPrerelease (Version _ _ _ (Just _)) = True
isPrerelease _                        = False

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

natural :: P.Parser Word64
natural = R.read <$> some P.digitChar

