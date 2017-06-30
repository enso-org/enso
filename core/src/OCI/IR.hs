{-# LANGUAGE UndecidableInstances #-}

module OCI.IR (module OCI.IR, module X) where

import           Luna.Prelude hiding (String)
import qualified Luna.Prelude as Prelude

import OCI.IR.Class        as X
import OCI.IR.Layout.Class as X
import OCI.IR.Layout.Typed as X (ET, E, T)
import OCI.IR.Layer        as X hiding (Definition)
import OCI.IR.Layer.Class  as X
import OCI.Pass.Manager    as X
import Data.Property       as X
import OCI.IR.Name         as X

import Data.Event as X (Event, Emitter, Emitters, type (//), emit, Payload (Payload))
