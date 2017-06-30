module Luna.Test.Evaluation.ValueSpec where


import           Luna.Builtin.Data.LunaEff
import           Luna.Prelude        hiding (String)
import qualified Luna.Prelude        as P
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldSatisfy, Expectation)
import Data.IORef

spec :: Spec
spec = do
    describe "Monadic values" $ do
        it "works for a single effect" $ do
            let res = run $ flip runReader 20 $ do
                    x <- ask
                    let w = x + 10
                    y <- ask
                    return $ w + y
            res `shouldBe` 50
        it "allows to combine Reader with Error" $ do
            let res = run $ flip runReader 15 $ runError $ do
                    x <- ask
                    y <- ask
                    throw $ show $ x + y
                    return 50
            res `shouldBe` Left "30"
        it "runs IO" $ do
            ioref <- newIORef 0
            res   <- runIO $ do
                performIO $ writeIORef ioref 12
                y <- performIO $ readIORef  ioref
                performIO $ writeIORef ioref $ y + 1
                performIO $ writeIORef ioref 42
                return y
            res `shouldBe` 12
            refVal <- readIORef ioref
            refVal `shouldBe` 42
        it "combines IO with Error" $ do
            ioref <- newIORef 0
            res   <- runIO $ runError $ do
                performIO $ writeIORef ioref 12
                throw "BANG!"
                performIO $ writeIORef ioref 42
                return 28
            res `shouldBe` Left "BANG!"
            refVal <- readIORef ioref
            refVal `shouldBe` 12

