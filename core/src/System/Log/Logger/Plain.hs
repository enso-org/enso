{-# LANGUAGE UndecidableInstances #-}

module System.Log.Logger.Plain where

import Prologue

import qualified Text.PrettyPrint.ANSI.Leijen as Doc

import System.Log.Logger.Class
import System.Log.Data


-------------------------
-- === PlainLogger === --
-------------------------

-- === Definition === --

data PLAIN
type PlainLogger = IdentityLogger PLAIN


-- === Utils === --

clearStyles :: DataStore Msg m => m ()
clearStyles = modifyData_ @Msg (wrapped %~ Doc.plain) ; {-# INLINE clearStyles #-}

plain :: Logger PlainLogger m a -> m a
plain = runIdentityLogger ; {-# INLINE plain #-}


-- === Instances === --

instance DataStore Msg m => IsLogger PlainLogger m where
    runLogger = clearStyles ; {-# INLINE runLogger #-}
