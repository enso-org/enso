module Luna.Package.Version where

import Prologue hiding (fromString, min)

import qualified Control.Lens.Aeson as Lens
import qualified Data.Yaml          as Yaml

import Data.Word            (Word64)
import Luna.ParserUtils     (Parser, dot, natural)
import Text.Megaparsec      (option, optional)
import Text.Megaparsec.Char (char, string)

-- | Versioning in Luna follows the following convention:
-- |      major.minor.patch-prerelease.version
-- |
-- | The prerelease is optional.

----------------------------
-- === PrereleaseType === --
----------------------------

-- === Definition === --

noPrereleaseNum :: Word64
noPrereleaseNum = 3

data PrereleaseType
    = Alpha
    | Beta
    | RC
    deriving (Enum, Eq, Generic, Ord, Show)


-- === API === --

prereleaseTyToNum :: PrereleaseType -> Word64
prereleaseTyToNum = fromIntegral . fromEnum

numToPrereleaseTy :: Word64 -> PrereleaseType
numToPrereleaseTy = toEnum . fromIntegral


-- === Instances === --

instance StyledShow PrettyShowStyle PrereleaseType where
    styledShow _ Alpha = "alpha"
    styledShow _ Beta  = "beta"
    styledShow _ RC    = "rc"

instance Yaml.FromJSON PrereleaseType where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON PrereleaseType where
    toJSON     = Lens.toJSON
    toEncoding = Lens.toEncoding



------------------------
-- === Prerelease === --
------------------------

-- === Definition === --

data Prerelease = Prerelease
    { _preType    :: !PrereleaseType
    , _preVersion :: !Word64
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Prerelease


-- === Instances === --

instance StyledShow PrettyShowStyle Prerelease where
    styledShow tx (Prerelease ty ver) = styledShow tx ty <> "."
        <> convert (show ver)

instance Yaml.FromJSON Prerelease where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON Prerelease where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle



---------------------
-- === Version === --
---------------------

-- === Definition === --

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


-- === Instances === --

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

instance StyledShow PrettyShowStyle Version where
    styledShow tx (Version maj min patch pr) = nums <> showPre pr
        where nums    = cShow maj <> "." <> cShow min <> "."
                      <> cShow patch
              showPre = \case
                  Nothing -> ""
                  Just pre -> "-" <> styledShow tx pre
              cShow = convert . show

instance Default Version where
    def = Version 0 0 1 Nothing

instance Yaml.FromJSON Version where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON Version where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle



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

