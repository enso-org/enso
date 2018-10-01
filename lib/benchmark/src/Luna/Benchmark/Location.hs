{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Benchmark.Location
    ( module Luna.Benchmark.Location
    , Stack.SrcLoc(SrcLoc)
    , Stack.HasCallStack
    ) where

import Prologue

import qualified Control.Lens.Aeson as Lens
import qualified Data.Yaml          as Yaml
import qualified GHC.Stack          as Stack



--------------------
-- === Location === --
--------------------

-- === API === --

get :: Stack.HasCallStack => Stack.SrcLoc
get = if length stack < 2 then
        (def @Stack.SrcLoc) { Stack.srcLocPackage = "Unknown"
                            , Stack.srcLocModule = "Unknown" }
    -- Safe as guaranteed to have at least 2 elements in this branch
    else unsafeHead $ drop 1 stack
    where stack = snd <$> Stack.getCallStack Stack.callStack


-- === Instances === --

deriving instance Generic Stack.SrcLoc
deriving instance Ord Stack.SrcLoc

instance Default Stack.SrcLoc where
    def = Stack.SrcLoc def def def def def def def

instance Yaml.FromJSON Stack.SrcLoc where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON Stack.SrcLoc where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance StyledShow PrettyShowStyle Stack.SrcLoc where
    styledShow _ (Stack.SrcLoc pkg mod file lineS colS _ _) = convert pkg
        <> ":" <> convert mod <> " " <> convert file <> ":"
        <> convert (show lineS) <> ":" <> convert (show colS)

