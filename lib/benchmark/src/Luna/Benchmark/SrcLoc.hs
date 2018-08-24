{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Benchmark.SrcLoc
    ( module Luna.Benchmark.SrcLoc
    , Stack.SrcLoc(SrcLoc) ) where

import Prologue

import qualified Control.Lens.Aeson as Lens
import qualified Data.Yaml          as Yaml
import qualified GHC.Stack          as Stack



--------------------
-- === SrcLoc === --
--------------------

-- === API === --

-- FIXME [AA] This doesn't report the location properly. To fix later.
get :: Stack.HasCallStack => [Stack.SrcLoc]
get = snd <$> Stack.getCallStack Stack.callStack


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

