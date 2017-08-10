{-# LANGUAGE TypeApplications #-}

module Text.PrettyPrint.ANSI.Leijen.Convert where

import Text.PrettyPrint.ANSI.Leijen
import Data.Convert.Class
import Data.Default
import Data.Text
import Data.Convert.Instances.Text

type IsDoc t = Convertible t Doc

instance Default Doc where def = convert "" ; {-# INLINE def #-}

instance Convertible String Doc where convert s = text s             ; {-# INLINE convert #-}
instance Convertible Text   Doc where convert   = convertVia @String ; {-# INLINE convert #-}
