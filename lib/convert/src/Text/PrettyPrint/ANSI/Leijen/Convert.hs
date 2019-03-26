{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TypeApplications #-}

module Text.PrettyPrint.ANSI.Leijen.Convert where

import Prelude

import Data.Convert.Instances.Text ()

import Text.PrettyPrint.ANSI.Leijen
import Data.Convert.Class
import Data.Default
import Data.Text

type IsDoc t = Convertible t Doc

instance Default Doc where def = convert ("" :: String) ; {-# INLINE def #-}

instance Convertible String Doc where convert s = text s             ; {-# INLINE convert #-}
instance Convertible Text   Doc where convert   = convertVia @String ; {-# INLINE convert #-}

