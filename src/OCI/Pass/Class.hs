module OCI.Pass.Class where

import Prologue

import Control.Monad.Branch
import Control.Monad.State.Layered
import Foreign (Ptr)






-- data LayerInfo = LayerInfo
--     { _layerRep :: SomeTypeRep
--     , _layerByteOffset :: !Int
--     ,
--
-- }

--------------------------------
-- === Pass Configuration === --
--------------------------------

data IRData
    = Layer
    | Attr

-- | For example: In 'Layer MyPass TERM = '[Model]
type family In        (d :: IRData) pass component :: [*]
type family Out       (d :: IRData) pass component :: [*]
type family Preserves (d :: IRData) pass component :: [*]





---------------------------
-- === Layer Offsets === --
---------------------------

newtype LayerOffsets = LayerOffsets (Ptr())


------------------
-- === Pass === --
------------------

-- === Definition === --

type       Pass pass m   = SubPass pass m ()
newtype SubPass pass m a = SubPass (StateT LayerOffsets m a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadPlus, MonadTrans, MonadThrow, MonadBranch)
makeLenses ''SubPass


runPass :: Functor m => SubPass pass m a -> m a
runPass = flip evalStateT undefined . coerce where
    -- termLayers = [someTypeRep @(TERM,Model)]

{-# INLINE runPass #-}
