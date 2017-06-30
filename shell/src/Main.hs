{-# LANGUAGE OverloadedStrings #-}

module Main where

import Luna.Prelude hiding (Level, switch, argument)
import qualified Luna.Shell
import Options.Applicative hiding (helper, info)
import qualified Options.Applicative as Opts
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map  as Map
import qualified Data.Text as Text
import qualified System.Environment as System
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

import qualified System.Console.Options as O

class Pretty a where
    showPretty :: a -> Text
    readPretty :: Text -> Either String a





class CmdParser a where
    parseCmd :: Parser a




phantom :: Mod FlagFields () -> Parser ()
phantom = flag () ()

phantom' :: Parser ()
phantom' = phantom mempty




-- === Verbosity === --

-- data Verbosity = Verbosity { _scope :: Text
--                            , _level :: Level
--                            } deriving (Show)
-- data Level = Debug
--            | Info
--            | Error
--            | Panic
--            deriving (Show)
--
-- makeLenses ''Verbosity


-- ------------------------
-- -- === PassStatus === --
-- ------------------------
--
-- data PassStatus = Enable | Disable deriving (Show, Read)
--
-- passMapParser :: Parser (Map Text PassStatus)
-- passMapParser = foldr (uncurry Map.insert) mempty <$> many passPassStatusParser
--
-- passPassStatusParser :: Parser (Text, PassStatus)
-- passPassStatusParser = flip (,)
--                <$> option (eitherReader $ readPretty . convert) (hidden <> long "pass" <> metavar "switch name" <> help "Switch passes execution.")
--                <*> strArgument (internal <> metavar "name")
--
-- instance Pretty PassStatus where
--     showPretty   = Text.toLower . convert . show
--     readPretty s = mapLeft (const . convert $ "Unexpected pass command '" <> s <> "'. Expecting 'enable' or 'disable'.") . tryReads $ Text.toTitle s


------------------------
-- === ConfigTree === --
------------------------

type ConfigTree = Map Text Text

configTreeParser :: Parser ConfigTree
configTreeParser = foldr (uncurry Map.insert) mempty <$> many configTreeValParser

configTreeValParser :: Parser (Text, Text)
configTreeValParser = (,)
    <$> strOption   (hidden <> long "set" <> metavar "component value" <> helpDoc (Just $ "Set configuration value." Doc.<$> "See \"configuration\" help topic for detailed description."))
    <*> strArgument (internal <> metavar "value")


-- luna build --set pass.analysis.simpleaa.enabled   true
-- luna build +pass.analysis.simpleaa

-- luna build --set pass.analysis.simpleaa.verbosity debug
-- luna build --verbosity pass.analysis.simpleaa debug
-- luna build --verbosity pass.analysis.* debug

-- luna build --set pass.analysis.simpleaa.verbosity enabled
-- luna build +pass.analysis.simpleaa.verbosity
-- luna build +*.verbosity
-- luna build +verbosity   # global option for the most important things

-- luna build --set report.unused.variable warning


-- stats   = +pass.**.stats
-- verbose = +pass.**.verbose

 

-- === Build === --

data ReportLevel = Silent
                 | Warning
                 | Error
                 deriving (Show)

data Optimization = None
                  | Normal
                  | Full
                  | Selected [Text]
                  deriving (Show)

-- data PassCfg = PassCfg { _verbosity :: Verbosity
--                        , _timeStats :: Bool
--                        , _memStats  :: Bool
--                     --    ,
--                        }

data BuildCfg = BuildCfg
    { --_optimization :: Optimization
    -- , _pragmas      :: Map Text Pragma
    -- , _pretend      :: Bool
     _pass         :: ConfigTree
    -- , _report       :: ConfigTree
    } deriving (Show)

-- stats


instance CmdParser BuildCfg where
    parseCmd = BuildCfg <$> configTreeParser




-- === Clean === --

data CleanOpts = CleanOpts deriving (Show)

instance CmdParser CleanOpts where
    parseCmd = pure CleanOpts


-- === Doc === --

data DocOpts = DocOpts deriving (Show)

instance CmdParser DocOpts where
    parseCmd = pure DocOpts


-- === Env === --

data EnvOpts = EnvOpts deriving (Show)

instance CmdParser EnvOpts where
    parseCmd = pure EnvOpts


-- === Install === --

data InstallOpts = InstallOpts deriving (Show)

instance CmdParser InstallOpts where
    parseCmd = pure InstallOpts


-- === New === --

data NewOpts = NewOpts deriving (Show)

instance CmdParser NewOpts where
    parseCmd = pure NewOpts


-- === Package === --

data PackageOpts = PackageOpts deriving (Show)

instance CmdParser PackageOpts where
    parseCmd = pure PackageOpts


-- === Help === --

data HelpOpts = HelpAboutConfig deriving (Show)

instance CmdParser HelpOpts where
    parseCmd = subparser (mconcat [ commandGroup "Help topics:", metavar "topic"
              , command "configuration" . info (pure HelpAboutConfig)   $ progDesc "Luna toolkit configuration management."
              ])


-- === Run === --

data RunOpts = RunOpts deriving (Show)

instance CmdParser RunOpts where
    parseCmd = pure RunOpts


-- === Version === --

data VersionOpts = VersionOpts deriving (Show)

instance CmdParser VersionOpts where
    parseCmd = pure VersionOpts



-- === Root === --

data RootCmd = Build   BuildCfg
             | Clean   CleanOpts
             | Doc     DocOpts
             | Env     EnvOpts
             | Install InstallOpts
             | New     NewOpts
             | Package PackageOpts
             | Help    HelpOpts
             | Run     RunOpts
             | Version VersionOpts
             deriving (Show)


rootCmd :: Parser RootCmd
rootCmd = subparser (mconcat [ commandGroup "Compilation:", metavar "command"
          , command "build"    . cmdInfo Build   $ progDesc "Compile packages and dependencies."
          , command "clean"    . cmdInfo Clean   $ progDesc "Remove compilation cache."
          , command "install"  . cmdInfo Install $ progDesc "Compile and install packages and dependencies."
          , command "run"      . cmdInfo Run     $ progDesc "Compile and run Luna programs."
          ])
      <|> subparser (mconcat [ commandGroup "Package management:", hidden
          , command "new"      . cmdInfo New     $ progDesc "Create new package."
          , command "package"  . cmdInfo Package $ progDesc "Package management tools."
          ])
      <|> subparser (mconcat [ commandGroup "Information:", hidden
          , command "help"     . cmdInfo Help    $ progDesc "Additional help topics."
          , command "info"     . cmdInfo Version $ progDesc "Access environment information."
          ])



-------------------
-- === Shell === --
-------------------

main :: IO ()
main = do
    O.main
    -- args <- System.getArgs
    -- opts <- handleParseResult $ execParserPure prefs pinfo (preprocessArgs args)
    -- print opts
    -- return ()
    -- where prefs = defaultPrefs {prefShowHelpOnEmpty = True}
    --       pinfo = info rootCmd
    --             $ fullDesc <> header "Luna compiler and ecosystem toolkit."
    --                        <> footer "Use `luna [topic] help` for more information about that topic."


preprocessArgs :: [String] -> [String]
preprocessArgs = concat . go 0 where
    go i   = \case []     -> []
                   (a:as) -> (procArg i a) : go (succ i) as
    procArg i a = if
        | a == "help" && i /= 0 -> ["--help"] -- FIXME: https://github.com/pcapriotti/optparse-applicative/issues/272
        | countMinus a == 1     -> ["--set", drop 1 a <> ".enabled", "false"]
        | countPlus  a == 1     -> ["--set", drop 1 a <> ".enabled", "true"]
        | otherwise             -> [a]


countMinus, countPlus :: String -> Int
countMinus = countPrefix '-'
countPlus  = countPrefix '+'

countPrefix :: Char -> String -> Int
countPrefix c = length . List.takeWhile (== c)


info p = Opts.info (p <**> helper)

cmdInfo p = info (p <$> parseCmd)

helper :: Parser (a -> a)
helper = abortOption ShowHelpText (internal <> long "help" <> help "Show this help text.")


-- print =<< customExecParser () opts
--   where
--     opts = info (buildOpts <**> helper)
--       ( fullDesc
--      <> progDesc "Print a greeting for TARGET"
--      <> header "hello - a test for optparse-applicative" )

-- greet :: BuildCfg -> IO ()
-- greet (BuildCfg h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
-- greet _ = return ()


--
-- luna build +pass simpleaa
--
-- luna build +debug.verbose import qualified Data.Map as Map


-- luna build --verbose debug
-- luna build +verbose
-- luna build -verbose --pass enable
