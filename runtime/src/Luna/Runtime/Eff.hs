module Luna.Runtime.Eff where

import Prologue hiding (Exception)

import qualified Control.Exception as Exception

newtype Exception = Exception Text deriving (Show, Eq)
instance Exception.Exception Exception
makeLenses ''Exception

data Eff a = Pure a
           | Monadic (IO a)
           deriving (Functor)

instance Applicative Eff where
    pure = Pure

    Pure f    <*> Pure a    = Pure $ f a
    Pure f    <*> Monadic a = Monadic $ f <$> a
    Monadic f <*> Pure a    = Monadic $ ($ a) <$> f
    Monadic f <*> Monadic a = Monadic $ f <*> a

instance Monad Eff where
    Pure    a >>= f = f a
    Monadic a >>= f = Monadic $ do
        a' <- a
        case f a' of
            Pure r    -> pure r
            Monadic r -> r

instance MonadFix Eff where
    mfix = unsafeLiftIO . mfix . fmap runIO

unsafeLiftIO :: IO a -> Eff a
unsafeLiftIO = Monadic

instance MonadIO Eff where
    liftIO a = do
        let handle :: SomeException -> IO (Either Text a)
            handle = pure . Left . convert . displayException
        res <- unsafeLiftIO $ Exception.catch (Right <$> a) handle
        case res of
            Left a  -> throw a
            Right r -> return r

runIO :: Eff a -> IO a
runIO (Pure a)    = pure a
runIO (Monadic a) = a

runError :: Eff a -> Eff (Either Exception a)
runError (Pure a)    = pure $ Right a
runError (Monadic a) = liftIO $ Exception.try a

throw :: Text -> Eff a
throw = unsafeLiftIO . Exception.throwIO . Exception

