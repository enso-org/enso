{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Syntax.Text.Source where

import Prologue

import qualified Luna.Pass.Attr as Attr

import Data.Text.Position (Delta)
import Data.Text32        (Text32)



--------------------
-- === Source === --
--------------------

-- === Definition === --

newtype Source = Source Text32 deriving (Show, Eq, Mempty)
makeLenses ''Source

type instance Attr.Type Source = Attr.Atomic


-- === Instances === --

instance Default            Source where def        = mempty             ; {-# INLINE def        #-}
instance IsString           Source where fromString = convert            ; {-# INLINE fromString #-}
instance Convertible String Source where convert    = convertVia @Text32 ; {-# INLINE convert    #-}
instance Convertible Text32 Source where convert    = coerce             ; {-# INLINE convert    #-}
instance Convertible Source Text32 where convert    = coerce             ; {-# INLINE convert    #-}
instance Convertible Source String where convert    = convertVia @Text32 ; {-# INLINE convert    #-}

