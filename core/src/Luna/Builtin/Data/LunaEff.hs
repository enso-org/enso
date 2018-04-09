module Luna.Builtin.Data.LunaEff where

import           Luna.Prelude
import           Control.Monad            (ap)
import           GHC.Exts                 (Any)
import           Luna.Builtin.Data.Stream
import           Control.Monad.Except

newtype LunaEffCont a = LunaEffCont { runLunaEffCont :: ExceptT String IO a } deriving (Functor, Applicative, Monad, MonadIO)

performIO' :: IO a -> LunaEffCont a
performIO' = liftIO

runIO' :: LunaEffCont a -> IO a
runIO' a = (\(Right x) -> x) <$> (runExceptT $ runLunaEffCont a)

throw' :: String -> LunaEffCont a
throw' e = LunaEffCont $ throwError e

runError' :: LunaEffCont a -> LunaEffCont (Either String a)
runError' a = LunaEffCont $ lift $ runExceptT $ runLunaEffCont a

data LunaEff a = LunaPure a
               | LunaMonadic (LunaEffCont a) deriving (Functor)

instance Applicative LunaEff where
    pure = LunaPure

    LunaPure    f <*> LunaPure    a = LunaPure    $ f a
    LunaPure    f <*> LunaMonadic a = LunaMonadic $ f <$> a
    LunaMonadic f <*> LunaPure    a = LunaMonadic $ ($ a) <$> f
    LunaMonadic f <*> LunaMonadic a = LunaMonadic $ f <*> a

instance Monad LunaEff where
    LunaPure    a >>= f = f a
    LunaMonadic a >>= f = LunaMonadic $ do
        a' <- a
        case f a' of
            LunaPure    r -> return r
            LunaMonadic r -> r

instance MonadIO LunaEff where
    liftIO = LunaMonadic . liftIO

{-newtype LunaEffCont a = LunaEffCont { runLunaEff :: forall w. (a -> PE w) -> PE w }-}

{-instance Functor LunaEffCont where-}
    {-fmap f (LunaEffCont a) = LunaEffCont $ a . (. f)-}

{-instance Applicative LunaEffCont where-}
    {-pure  = return-}
    {-(<*>) = ap-}

{-instance Monad LunaEffCont where-}
    {-return a = LunaEffCont ($ a)-}
    {-m >>= f  = LunaEffCont $ \k -> runLunaEff m $ \v -> runLunaEff (f v) k-}

{-data PE a = P a | E (Effect (PE a))-}

{-newtype EffPayload a = EffPayload Any-}
{-type    EffTag       = String-}

{-data Effect a = Effect { _payload :: EffPayload a-}
                       {-, _tag     :: EffTag-}
                       {-}-}

{-makeLenses ''Effect-}

{-data ReaderE a = Ask (Int -> a)-}
{-data ErrorE  a = Throw String-}
{-data StateE  a = Get (Int -> a) | Put Int a-}
{-data LiftIOE a = LiftIOE (IO Any) (Any -> a)-}

{-fmap_reader :: (a -> b) -> EffPayload a -> EffPayload b-}
{-fmap_reader f (EffPayload a) = EffPayload $ unsafeCoerce $ case unsafeCoerce a of-}
    {-Ask g -> Ask $ f . g-}

{-fmap_error :: (a -> b) -> EffPayload a -> EffPayload b-}
{-fmap_error f = unsafeCoerce-}

{-fmap_liftIO :: (a -> b) -> EffPayload a -> EffPayload b-}
{-fmap_liftIO f (EffPayload a) = EffPayload $ unsafeCoerce $ case unsafeCoerce a of-}
    {-LiftIOE a v -> LiftIOE a $ f . v-}

{--- TODO: instead of case use dynamic handlers map-}
{-instance Functor Effect where-}
    {-fmap f (Effect d tag) = flip Effect tag $ case tag of-}
        {-"Reader" -> fmap_reader f d-}
        {-"Error"  -> fmap_error  f d-}
        {-"LiftIO" -> fmap_liftIO f d-}

{-admin :: LunaEffCont a -> PE a-}
{-admin m = runLunaEff m P-}

{-send :: (forall w. (a -> PE w) -> PE w) -> LunaEffCont a-}
{-send = LunaEffCont-}

{-sendRequest :: (forall w. (a -> PE w) -> Effect (PE w)) -> LunaEffCont a-}
{-sendRequest f = send $ E . f-}

{-run :: LunaEffCont a -> a-}
{-run m = case admin m of-}
    {-P a -> a-}
    {-E a -> error "Expected pure computation, got action!"-}

{-handlerFor :: EffTag -> (a -> LunaEffCont b) -> (c -> LunaEffCont b) -> LunaEffCont a -> LunaEffCont b-}
{-handlerFor t whenP whenE m = loop $ admin m where-}
    {-loop (P a) = whenP a-}
    {-loop (E e) = if e ^. tag == t-}
        {-then whenE $ unsafeCoerce $ e ^. payload-}
        {-else send (\k -> E $ fmap k e) >>= loop-}

{-performIO'' :: IO a -> (forall w. (a -> w) -> Effect w)-}
{-performIO'' m f = Effect (EffPayload $ unsafeCoerce $ LiftIOE (unsafeCoerce <$> m) (f . unsafeCoerce)) "LiftIO"-}

{-performIO' :: IO a -> LunaEffCont a-}
{-performIO' m = sendRequest $ performIO'' m-}

performIO :: IO a -> LunaEff a
performIO m = LunaMonadic $ performIO' m

{-runIO' :: forall a. LunaEffCont a -> IO a-}
{-runIO' m = loop $ admin m where-}
    {-loop m = case m of-}
        {-P a -> return a-}
        {-E (Effect (EffPayload p) "LiftIO") -> do-}
            {-let LiftIOE m pe :: LiftIOE (PE a) = unsafeCoerce p-}
            {-(pe <$> m) >>= loop-}
        {-_ -> error "Unexpected effect type when running IO"-}

runIO :: LunaEff a -> IO a
runIO (LunaPure m)    = return m
runIO (LunaMonadic m) = runIO' m

{-throw'' :: String -> Effect a-}
{-throw'' s = Effect (EffPayload $ unsafeCoerce $ Throw s) "Error"-}

{-throw' :: String -> LunaEffCont a-}
{-throw' s = sendRequest $ const $ throw'' s-}

throw :: String -> LunaEff a
throw s = LunaMonadic $ throw' s

{-runError' :: LunaEffCont a -> LunaEffCont (Either String a)-}
{-runError' = handlerFor "Error" (return . Right) $ \(Throw t) -> return $ Left t-}

runError :: LunaEff a -> LunaEff (Either String a)
runError (LunaPure    m) = LunaPure (Right m)
runError (LunaMonadic m) = LunaMonadic $ runError' m

{-ask' :: (Int -> w) -> Effect w-}
{-ask' f = Effect (EffPayload $ unsafeCoerce $ Ask f) "Reader"-}

{-ask :: LunaEffCont Int-}
{-ask = sendRequest ask'-}

{-runReader :: LunaEffCont a -> Int -> LunaEffCont a-}
{-runReader m a = loop $ admin m where-}
    {-loop :: PE a -> LunaEffCont a-}
    {-loop (P a)     = return a-}
    {-loop (E e) = case (e ^. tag) of-}
        {-"Reader" -> loop $ case unsafeCoerce (e ^. payload) of-}
            {-Ask f -> f a-}
        {-_     -> send (\k -> E $ fmap k e) >>= loop-}

