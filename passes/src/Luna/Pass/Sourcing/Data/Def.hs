module Luna.Pass.Sourcing.Data.Def where

import Prologue

import qualified Data.Map       as Map
import qualified Luna.IR        as IR

data Documented a = Documented
    { _documentation :: Maybe Text
    , _documented    :: a
    } deriving (Show, Functor, Foldable, Traversable)

newtype DefsMap = DefsMap (Map.Map IR.Name (Documented (IR.Term IR.Function))) deriving (Show, Default)

makeLenses ''Documented
makeLenses ''DefsMap
