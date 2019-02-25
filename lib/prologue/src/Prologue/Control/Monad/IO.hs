module Prologue.Control.Monad.IO (module Prologue.Control.Monad.IO, module X) where

import qualified Prelude as P

import Prelude                          (Char, String)
import Prelude                          as X (FilePath, IOError, userError)
import Prologue.Data.Show
import Prologue.Control.Monad.Primitive
import Data.Functor.Utils


putChar  :: MonadIO m => Char   -> m ()
putStr   :: MonadIO m => String -> m ()
putStrLn :: MonadIO m => String -> m ()
putChar  = liftIO . P.putChar  ; {-# INLINE putChar  #-}
putStr   = liftIO . P.putStr   ; {-# INLINE putStr   #-}
putStrLn = liftIO . P.putStrLn ; {-# INLINE putStrLn #-}

print, pprint :: (MonadIO m, Show a) => a -> m ()
print  = liftIO . P.print ; {-# INLINE print  #-}
pprint = putStrLn . ppShow  ; {-# INLINE pprint #-}

getChar     :: MonadIO m => m Char
getLine     :: MonadIO m => m String
getContents :: MonadIO m => m String
getChar     = liftIO P.getChar     ; {-# INLINE getChar     #-}
getLine     = liftIO P.getLine     ; {-# INLINE getLine     #-}
getContents = liftIO P.getContents ; {-# INLINE getContents #-}

interact :: MonadIO m => (String -> String) -> m ()
interact = liftIO . P.interact ; {-# INLINE interact #-}

readFile  :: MonadIO m => FilePath -> m String
writeFile :: MonadIO m => FilePath -> String -> m ()
readFile  = liftIO .  P.readFile  ; {-# INLINE readFile  #-}
writeFile = liftIO .: P.writeFile ; {-# INLINE writeFile #-}

appendFile :: FilePath -> String -> IO ()
appendFile = liftIO .: P.appendFile ; {-# INLINE appendFile #-}

ioError :: MonadIO m => IOError -> m a
ioError = liftIO . P.ioError ; {-# INLINE ioError #-}
