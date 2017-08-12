module Prologue.Control.Monad.IO (module Prologue.Control.Monad.IO, module X) where

import qualified Prelude as P

import Prelude                          (Char, String)
import Prelude                          as X (FilePath, IOError, userError)
import Prologue.Data.Show
import Prologue.Control.Monad.Primitive
import Data.Functor.Utils


putChar  :: PrimMonadIO m => Char   -> m ()
putStr   :: PrimMonadIO m => String -> m ()
putStrLn :: PrimMonadIO m => String -> m ()
putChar  = liftPrim . P.putChar  ; {-# INLINE putChar  #-}
putStr   = liftPrim . P.putStr   ; {-# INLINE putStr   #-}
putStrLn = liftPrim . P.putStrLn ; {-# INLINE putStrLn #-}

print, pprint :: (PrimMonadIO m, Show a) => a -> m ()
print  = liftPrim . P.print ; {-# INLINE print  #-}
pprint = putStrLn . ppShow  ; {-# INLINE pprint #-}

getChar     :: PrimMonadIO m => m Char
getLine     :: PrimMonadIO m => m String
getContents :: PrimMonadIO m => m String
getChar     = liftPrim P.getChar     ; {-# INLINE getChar     #-}
getLine     = liftPrim P.getLine     ; {-# INLINE getLine     #-}
getContents = liftPrim P.getContents ; {-# INLINE getContents #-}

interact :: PrimMonadIO m => (String -> String) -> m ()
interact = liftPrim . P.interact ; {-# INLINE interact #-}

readFile  :: PrimMonadIO m => FilePath -> m String
writeFile :: PrimMonadIO m => FilePath -> String -> m ()
readFile  = liftPrim .  P.readFile  ; {-# INLINE readFile  #-}
writeFile = liftPrim .: P.writeFile ; {-# INLINE writeFile #-}

appendFile :: FilePath -> String -> IO ()
appendFile = liftPrim .: P.appendFile ; {-# INLINE appendFile #-}

ioError :: PrimMonadIO m => IOError -> m a
ioError = liftPrim . P.ioError ; {-# INLINE ioError #-}
