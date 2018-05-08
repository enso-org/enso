module Data.Graph.Component.Discovery where

import Prologue

import qualified Data.Graph.Component.Class    as Component
import qualified Data.Graph.Component.Dynamic  as Component
import qualified Data.Graph.Component.Provider as Component

import Data.Graph.Component.Class (Component)



discover :: Component tag layout -> [Component.Dynamic]
discover _ = []
