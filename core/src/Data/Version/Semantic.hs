module Data.Version.Semantic where

import Prologue

-- === Definitions === --


data Release = PreRelease Tag [Tag]
             | Final
             deriving (Show)

data Tag = Tag Ident (Maybe Int) deriving (Show)

data Ident = Alpha
           | Beta
           | RC
           deriving (Show)

data Meta = Meta String deriving (Show)

data Version = Version { _major   :: Int
                       , _minor   :: Int
                       , _patch   :: Int
                       , _release :: Release
                       , _metas   :: [Meta]
                       } deriving (Show)

makeClassy ''Version


-- === Utils === --

showVersion :: Version -> String
showVersion (Version major minor patch release metas) = base <> rel <> ms where
    base  = show major <> "." <> show minor <> "." <> show patch
    rel   = case release of
        Final -> ""
        r     -> "-" <> showRelease r
    ms = if null metas then "" else intercalate "+" ("" : fmap showMeta metas)

showRelease :: Release -> String
showRelease = \case
    Final           -> ""
    PreRelease t ts -> showTag t <> if null ts then "" else "." <> intercalate "." (showTag <$> ts)

showTag :: Tag -> String
showTag (Tag ident n) = showIdent ident <> ntag where
    ntag = case n of
        Just  n -> "." <> show n
        Nothing -> ""

showIdent :: Ident -> String
showIdent = \case
    Alpha -> "alpha"
    Beta  -> "beta"
    RC    -> "rc"

showMeta :: Meta -> String
showMeta (Meta s) = s


-- === Instances === --

instance IsString Meta where fromString = Meta
