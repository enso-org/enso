module Luna.Builtin.Data.LunaEff where

import           Luna.Prelude
import           Control.Monad            (ap)
import           GHC.Exts                 (Any)
import           Luna.Builtin.Data.Stream

newtype LunaEff a = LunaEff { runLunaEff :: forall w. (a -> PE w) -> PE w }

instance Functor LunaEff where
    fmap f (LunaEff a) = LunaEff $ a . (. f)

instance Applicative LunaEff where
    pure  = return
    (<*>) = ap

instance Monad LunaEff where
    return a = LunaEff ($ a)
    m >>= f  = LunaEff $ \k -> runLunaEff m $ \v -> runLunaEff (f v) k

data PE a = P a | E (Effect (PE a))

newtype EffPayload a = EffPayload Any
type    EffTag       = String

data Effect a = Effect { _payload :: EffPayload a
                       , _tag     :: EffTag
                       }

makeLenses ''Effect

data ReaderE a = Ask (Int -> a)
data ErrorE  a = Throw String
data StateE  a = Get (Int -> a) | Put Int a
data LiftIOE a = LiftIOE (IO Any) (Any -> a)

fmap_reader :: (a -> b) -> EffPayload a -> EffPayload b
fmap_reader f (EffPayload a) = EffPayload $ unsafeCoerce $ case unsafeCoerce a of
    Ask g -> Ask $ f . g

fmap_error :: (a -> b) -> EffPayload a -> EffPayload b
fmap_error f = unsafeCoerce

fmap_liftIO :: (a -> b) -> EffPayload a -> EffPayload b
fmap_liftIO f (EffPayload a) = EffPayload $ unsafeCoerce $ case unsafeCoerce a of
    LiftIOE a v -> LiftIOE a $ f . v

-- TODO: instead of case use dynamic handlers map
instance Functor Effect where
    fmap f (Effect d tag) = flip Effect tag $ case tag of
        "Reader" -> fmap_reader f d
        "Error"  -> fmap_error  f d
        "LiftIO" -> fmap_liftIO f d

admin :: LunaEff a -> PE a
admin m = runLunaEff m P

send :: (forall w. (a -> PE w) -> PE w) -> LunaEff a
send = LunaEff

sendRequest :: (forall w. (a -> PE w) -> Effect (PE w)) -> LunaEff a
sendRequest f = send $ E . f

run :: LunaEff a -> a
run m = case admin m of
    P a -> a
    E a -> error "Expected pure computation, got action!"

handlerFor :: EffTag -> (a -> LunaEff b) -> (c -> LunaEff b) -> LunaEff a -> LunaEff b
handlerFor t whenP whenE m = loop $ admin m where
    loop (P a) = whenP a
    loop (E e) = if e ^. tag == t
        then whenE $ unsafeCoerce $ e ^. payload
        else send (\k -> E $ fmap k e) >>= loop

performIO' :: IO a -> (forall w. (a -> w) -> Effect w)
performIO' m f = Effect (EffPayload $ unsafeCoerce $ LiftIOE (unsafeCoerce <$> m) (f . unsafeCoerce)) "LiftIO"

performIO :: IO a -> LunaEff a
performIO m = sendRequest $ performIO' m

runIO :: forall a. LunaEff a -> IO a
runIO m = loop $ admin m where
    loop m = case m of
        P a -> return a
        E (Effect (EffPayload p) "LiftIO") -> do
            let LiftIOE m pe :: LiftIOE (PE a) = unsafeCoerce p
            (pe <$> m) >>= loop
        _ -> error "Unexpected effect type when running IO"

throw' :: String -> Effect a
throw' s = Effect (EffPayload $ unsafeCoerce $ Throw s) "Error"

throw :: String -> LunaEff a
throw s = sendRequest $ const $ throw' s

runError :: LunaEff a -> LunaEff (Either String a)
runError = handlerFor "Error" (return . Right) $ \(Throw t) -> return $ Left t

ask' :: (Int -> w) -> Effect w
ask' f = Effect (EffPayload $ unsafeCoerce $ Ask f) "Reader"

ask :: LunaEff Int
ask = sendRequest ask'

runReader :: LunaEff a -> Int -> LunaEff a
runReader m a = loop $ admin m where
    loop :: PE a -> LunaEff a
    loop (P a)     = return a
    loop (E e) = case (e ^. tag) of
        "Reader" -> loop $ case unsafeCoerce (e ^. payload) of
            Ask f -> f a
        _     -> send (\k -> E $ fmap k e) >>= loop

