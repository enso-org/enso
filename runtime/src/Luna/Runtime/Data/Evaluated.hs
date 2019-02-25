{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Runtime.Data.Evaluated where

import Prologue

import qualified Control.Concurrent.Future as Future
import qualified Luna.IR                   as IR
import qualified Luna.Pass.Attr            as Attr
import qualified Luna.Runtime.Data         as Luna
import qualified Luna.Runtime.Eff          as Luna

import Data.Map (Map)

newtype Units = Units (Map IR.Qualified Unit)
type instance Attr.Type Units = Attr.Atomic
instance Default Units where
    def = Units def

data Unit = Unit
    { _defs :: Luna.DefMap
    , _classes :: Map IR.Name Class }
data Class = Class { _meths :: Luna.DefMap }

makeLenses ''Units
makeLenses ''Unit
makeLenses ''Class

getObjectMethodMap :: Units -> IR.Qualified -> IR.Name -> Luna.DefMap
getObjectMethodMap units modName className = map where
    mayMap = units ^? wrapped . ix modName . classes . ix className . meths
    map = fromJust err mayMap
    err = error $ "Can't find method map for: " <> convert modName
                                                <> "."
                                                <> convert className

lookupSymbol :: MonadIO m => Units -> IR.Qualified -> IR.Name -> m Luna.Value
lookupSymbol units mod name = do
    let sym = units ^? wrapped . ix mod . defs . ix name
    case sym of
        Nothing -> pure $ Luna.throw $ "Symbol not found: "
                                       <> convertVia @IR.Name mod
                                       <> "."
                                       <> convert name
        Just fut -> Future.get fut

