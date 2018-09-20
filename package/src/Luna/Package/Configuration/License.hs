module Luna.Package.Configuration.License where

import Prologue

import qualified Control.Lens.Aeson as Lens
import qualified Data.Yaml          as Yaml

import Luna.ParserUtils     (Parser)
import Text.Megaparsec      (choice)
import Text.Megaparsec.Char (anyChar, string)



---------------------
-- === License === --
---------------------

-- === Definition === --

data License
    = AFL_3_0
    | AGPL_3_0
    | Apache_2_0
    | Artistic_2_0
    | BSD_2_Clause
    | BSD_3_Clause
    | BSD_3_Clause_Clear
    | BSL_1_0
    | CCBySA_4_0
    | CCBy_4_0
    | CC_0_1_0
    | EUPL_1_0
    | GPL_2_0
    | GPL_3_0
    | ISC
    | LGPL_2_1
    | LGPL_3_0
    | MIT
    | MPL_2_0
    | MS_PL
    | None
    | Unknown Text
    | Unlicense
    deriving (Eq, Generic, Ord, Show)


-- === Instances === --

instance StyledShow PrettyShowStyle License where
    styledShow _ = \case
        AFL_3_0            -> "afl-3.0"
        AGPL_3_0           -> "agpl-3.0"
        Apache_2_0         -> "apache-2.0"
        Artistic_2_0       -> "artistic-2.0"
        BSD_2_Clause       -> "bsd-2-clause"
        BSD_3_Clause       -> "bsd-3-clause"
        BSD_3_Clause_Clear -> "bsd-3-clause-clear"
        BSL_1_0            -> "bsl-1.0"
        CCBySA_4_0         -> "cc-by-sa-4.0"
        CCBy_4_0           -> "cc-by-4.0"
        CC_0_1_0           -> "cc0-1.0"
        EUPL_1_0           -> "eupl-1.0"
        GPL_2_0            -> "gpl-2.0"
        GPL_3_0            -> "gpl-3.0"
        ISC                -> "isc"
        LGPL_2_1           -> "lgpl-2.1"
        LGPL_3_0           -> "lgpl-3.0"
        MIT                -> "mit"
        MPL_2_0            -> "mpl-2.0"
        MS_PL              -> "ms-pl"
        None               -> "None"
        Unknown tx         -> tx
        Unlicense          -> "unlicense"

instance Default License where
    def = None

instance Yaml.FromJSON License where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON License where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle



-----------------------------
-- === License Parsers === --
-----------------------------

-- === API === --

license :: Parser License
license = choice options <|> unknown where
    options =
        [ string "afl-3.0"            *> pure AFL_3_0
        , string "agpl-3.0"           *> pure AGPL_3_0
        , string "apache-2.0"         *> pure Apache_2_0
        , string "artistic-2.0"       *> pure Artistic_2_0
        , string "bsd-2-clause"       *> pure BSD_2_Clause
        , string "bsd-3-clause"       *> pure BSD_3_Clause
        , string "bsd-3-clause-clear" *> pure BSD_3_Clause_Clear
        , string "bsl-1.0"            *> pure BSL_1_0
        , string "cc-by-sa-4.0"       *> pure CCBySA_4_0
        , string "cc-by-4.0"          *> pure CCBy_4_0
        , string "cc0-1.0"            *> pure CC_0_1_0
        , string "eupl-1.0"           *> pure EUPL_1_0
        , string "gpl-2.0"            *> pure GPL_2_0
        , string "gpl-3.0"            *> pure GPL_3_0
        , string "isc"                *> pure ISC
        , string "lgpl-2.1"           *> pure LGPL_2_1
        , string "lgpl-3.0"           *> pure LGPL_3_0
        , string "mit"                *> pure MIT
        , string "mpl-2.0"            *> pure MPL_2_0
        , string "ms-pl"              *> pure MS_PL
        , string "none"               *> pure None
        , string "unlicense"          *> pure Unlicense ]
    unknown = do
        str <- some anyChar
        pure . Unknown $ convertTo @Text str

