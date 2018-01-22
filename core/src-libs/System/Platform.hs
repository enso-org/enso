{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}


module System.Platform where

import           Data.Default
import           Prologue_old        hiding (lookup, splitAt)
import           GHC.Int         (Int32, Int64)


#include "ghcplatform.h"


------------------------------------------------------------------------
-- Platform
------------------------------------------------------------------------

data Windows    = Windows deriving (Show, Eq, Generic)
data Linux      = Linux   deriving (Show, Eq, Generic)
data Darwin     = Darwin  deriving (Show, Eq, Generic)
data GHCJS      = GHCJS   deriving (Show, Eq, Generic)
data FreeBSD    = FreeBSD deriving (Show, Eq, Generic)

-- === utils ===

#ifdef darwin_HOST_OS
type Platform = Darwin
delimeter = '/'
#endif

#ifdef linux_HOST_OS
type Platform = Linux
delimeter = '/'
#endif

#ifdef mingw32_HOST_OS
type Platform = Windows
delimeter = '\\'
#endif

#ifdef ghcjs_HOST_OS
type Platform = GHCJS
delimeter = '/'
#endif

#ifdef freebsd_HOST_OS
type Platform = FreeBSD
delimeter = '/'
#endif

platform :: Platform
platform = def

#ifdef ghcjs_HOST_OS
type PosixInt = Int32
#else
type PosixInt = Int64
#endif

-- === instances ===

instance Default Windows where def = Windows
instance Default Darwin  where def = Darwin
instance Default Linux   where def = Linux
instance Default GHCJS   where def = GHCJS
instance Default FreeBSD where def = FreeBSD

------------------------------------------------------------------------
