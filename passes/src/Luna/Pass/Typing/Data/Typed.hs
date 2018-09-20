{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Typing.Data.Typed where

import Prologue

import qualified Control.Concurrent.Future as Future
import qualified Data.Map                  as Map
import qualified Luna.IR                   as IR
import qualified Luna.Pass.Attr            as Attr
import qualified Luna.Pass.Data.Error      as Error

import Data.Graph.Store (Rooted)
import Data.Map (Map)

newtype DefHeader = DefHeader (Either Error.CompileError
                                      (Rooted (IR.Term IR.DefHeader)))
makeLenses ''DefHeader

type instance Attr.Type DefHeader = Attr.Atomic
instance Default DefHeader where
    def = wrap . Left $ Error.placeholderError

newtype Def = Def (Future.Future DefHeader)
instance Show Def where show _ = "<def>"
makeLenses ''Def

data Class = Class
    { _methods :: Map IR.Name Def
    , _conses  :: Map IR.Name (Rooted (IR.Term IR.ResolvedCons))
    } deriving Show
instance Show (Rooted a) where show _ = "<rooted>"
makeLenses ''Class

data Unit = Unit
    { _definitions :: Map IR.Name Def
    , _classes     :: Map IR.Name Class
    } deriving Show
makeLenses ''Unit

newtype Units = Units (Map IR.Qualified Unit) deriving Show
makeLenses ''Units
type instance Attr.Type Units = Attr.Atomic
instance Default Units where
    def = wrap def

getDef :: MonadIO m
       => IR.Qualified -> IR.Name -> Units -> m DefHeader
getDef mod n m = do
    let fut = m ^? wrapped . ix mod . definitions . ix n . wrapped
    case fut of
        Just fut -> Future.get fut
        Nothing  -> return . wrap . Left $ Error.unexpectedFunctionNotFound mod n

requestDef :: (MonadIO m, Attr.Getter Units m)
           => IR.Qualified -> IR.Name
           -> m DefHeader
requestDef mod n = Attr.get >>= getDef mod n

getCons :: IR.Qualified -> IR.Name -> IR.Name -> Units
        -> Maybe (Rooted (IR.Term IR.ResolvedCons))
getCons mod cls cons units =
    units ^? wrapped . ix mod . classes . ix cls . conses . ix cons

requestCons :: (MonadIO m, Attr.Getter Units m)
           => IR.Qualified -> IR.Name -> IR.Name
           -> m (Maybe (Rooted (IR.Term IR.ResolvedCons)))
requestCons mod cls cons = getCons mod cls cons <$> Attr.get

getMethod :: MonadIO m
          => IR.Qualified -> IR.Name -> IR.Name -> Units -> m DefHeader
getMethod mod cls n m = do
    let fut = m ^? wrapped . ix mod . classes . ix cls . methods . ix n . wrapped
    case fut of
        Just fut -> Future.get fut
        Nothing  -> return . wrap . Left $ Error.methodNotFound mod cls n

requestMethod :: (MonadIO m, Attr.Getter Units m)
              => IR.Qualified -> IR.Name -> IR.Name -> m DefHeader
requestMethod mod cls n = getMethod mod cls n =<< Attr.get

