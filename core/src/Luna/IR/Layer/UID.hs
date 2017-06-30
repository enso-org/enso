{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.IR.Layer.UID where

import Data.Data (Data)

import Luna.Prelude
import OCI.IR.Layer.Class
import Control.Arrow ((&&&))

import           Data.ManagedVectorMap (STRefM, modifySTRef', newSTRef)
import           OCI.Pass.Definition  (makePass)
import           Luna.IR.ToRefactor2 (Listener, tpElemPass, listener, addElemEventListener)
import           OCI.Pass
import           OCI.IR
import qualified OCI.IR.Class as Event

import Type.Any (AnyType) -- FIXME[WD]: required by TH, should be silently included

-----------------
-- === UID === --
-----------------

-- === Definition === --

type ID = Word64
data UID = UID deriving (Show)
type instance LayerData UID t = ID


-- === Construction === --

nextUID :: PrimMonad m => STRefM m ID -> m ID
nextUID ref = modifySTRef' ref $ id &&& succ

initUID :: Req m '[Writer // Layer // Abstract (Elem t) // UID] => STRefM m ID -> Listener (New // Elem t) m
initUID ref = listener $ \(t, _) -> putLayer @UID t =<< nextUID ref
makePass 'initUID

watchUIDImport :: Req m '[Writer // Layer // Abstract (Elem t) // UID]
               => STRefM m ID -> Listener (Event.Import // Elem t) m
watchUIDImport ref = listener $ \(t, _, _) -> putLayer @UID t =<< nextUID ref
makePass 'watchUIDImport

init :: MonadPassManager m => m ()
init = do
    ref <- newSTRef (def :: ID)
    addElemEventListener @UID $ initUIDPass        ref
    addElemEventListener @UID $ watchUIDImportPass ref
