module Luna.Package.Version where

import Prologue hiding (min, fromString)

import Data.Word                (Word64)
import Luna.Package.ParserUtils (Parser, dot, natural)
import Text.Megaparsec          (option, optional)
import Text.Megaparsec.Char     (string, char)

-- | Versioning in Luna follows the following convention:
-- |      major.minor.patch-prerelease.version
-- |
-- | The prerelease is optional.

---------------------
-- === Version === --
---------------------

-- === Definition === --

noPrereleaseNum :: Word64
noPrereleaseNum = 3

data PrereleaseType
    = Alpha
    | Beta
    | RC
    deriving (Eq, Generic, Ord, Show)

data Prerelease = Prerelease
    { __prType  :: !PrereleaseType
    , __version :: !Word64
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Prerelease

data Version = Version
    { __major      :: !Word64
    , __minor      :: !Word64
    , __patch      :: !Word64
    , __prerelease :: !(Maybe Prerelease)
    } deriving (Eq, Generic, Show)
makeLenses ''Version


-- === API === --

isPrerelease :: Version -> Bool
isPrerelease = \case (Version _ _ _ (Just _)) -> True
                     _                        -> False

prereleaseTyToNum :: PrereleaseType -> Word64
prereleaseTyToNum Alpha = 0
prereleaseTyToNum Beta  = 1
prereleaseTyToNum RC    = 2

numToPrereleaseTy :: Word64 -> PrereleaseType
numToPrereleaseTy 0 = Alpha
numToPrereleaseTy 1 = Beta
numToPrereleaseTy 2 = RC
numToPrereleaseTy _ = RC


-- === Instances === --

instance PrettyShow PrereleaseType where
    prettyShow Alpha = "alpha"
    prettyShow Beta  = "beta"
    prettyShow RC    = "rc"

instance PrettyShow Prerelease where
    prettyShow (Prerelease ty ver) = prettyShow ty <> "." <> convert (show ver)

instance Ord Version where
    (Version maj1 min1 pat1 pre1) < (Version maj2 min2 pat2 pre2) = if
        | maj1 < maj2 -> True
        | maj1 > maj2 -> False
        | min1 < min2 -> True
        | min1 > min2 -> False
        | pat1 < pat2 -> True
        | pat1 > pat2 -> False
        | otherwise   -> pre1 `preLT` pre2
        where preLT :: Maybe Prerelease -> Maybe Prerelease -> Bool
              preLT l r = case l of
                Just (Prerelease ty1 ver1) ->
                    case r of
                        Just (Prerelease ty2 ver2) -> if
                            | ty1 < ty2 -> True
                            | ty1 > ty2 -> False
                            | otherwise -> ver1 < ver2
                        Nothing -> True
                Nothing -> False

    v1 <= v2 = (v1 < v2) || (v1 == v2)

instance PrettyShow Version where
    prettyShow (Version maj min patch pr) = nums <> showPre pr
        where nums    = cShow maj <> "." <> cShow min <> "."
                      <> cShow patch
              showPre = \case
                  Nothing -> ""
                  Just pre -> "-" <> prettyShow pre
              cShow = convert . show

-------------------------------
-- === Parsing Functions === --
-------------------------------

-- === API === --

version :: Parser Version
version = do
    major <- natural
    minor <- option 0 (dot *> natural)
    patch <- option 0 (dot *> natural)
    prerel <- optional (char '-' *> prerelease)
    if major + minor + patch == 0 then fail msg else
        pure $ Version major minor patch prerel
    where msg = "Not all components of the version can be zero."

prerelease :: Parser Prerelease
prerelease = Prerelease <$> prereleaseType <* dot <*> natural

prereleaseType :: Parser PrereleaseType
prereleaseType = fromString <$> p
    where fromString "alpha" = Alpha
          fromString "beta"  = Beta
          fromString "rc"    = RC
          fromString _       = impossible
          p = string "alpha" <|> string "beta" <|> string "rc"

