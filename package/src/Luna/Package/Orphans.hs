{-# OPTIONS_GHC -Wno-orphans #-}

module Luna.Package.Orphans where

import Prologue


import OCI.Data.Name  (Name)
import Path           (Path, toFilePath)



-----------------------
-- === Instances === --
-----------------------

instance Convertible (Path a b) Name where
    convert = convert . Path.toFilePath ; {-# INLINE convert    #-}

instance Convertible (Path a b) Text where
    convert = convert . Path.toFilePath ; {-# INLINE convert    #-}
