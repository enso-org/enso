module Luna.Pass.PassManagerSpec where


import           Luna.Prelude
import           Luna.IR
import           Luna.Pass        (SubPass, Inputs, Outputs, Preserves, Events)
import qualified Luna.Pass        as Pass
import           System.Log
import System.Log.Logger.Format
import Control.Monad.Raise
import Test.Hspec (Spec, describe, it, shouldReturn)
import Data.TypeDesc

newtype TestAttr = TestAttr Int
makeWrapped ''TestAttr

data TestManagerPass
type instance Abstract TestManagerPass = TestManagerPass
type instance Inputs  Net   TestManagerPass = '[]
type instance Outputs Net   TestManagerPass = '[]
type instance Inputs  Layer TestManagerPass = '[]
type instance Outputs Layer TestManagerPass = '[]
type instance Inputs  Attr  TestManagerPass = '[TestAttr]
type instance Outputs Attr  TestManagerPass = '[TestAttr]
type instance Inputs  Event TestManagerPass = '[]
type instance Outputs Event TestManagerPass = '[]
type instance Preserves     TestManagerPass = '[]

readWriteAttr :: (MonadRef m, MonadIO m, MonadPassManager m) => Int -> SubPass TestManagerPass m Int
readWriteAttr i = do
    writeAttr @TestAttr $ TestAttr i
    unwrap <$> readAttr @TestAttr

writeTestAttr :: (MonadRef m, MonadIO m, MonadPassManager m) => Int -> SubPass TestManagerPass m ()
writeTestAttr = writeAttr @TestAttr . TestAttr

readTestAttr :: (MonadRef m, MonadIO m, MonadPassManager m) => SubPass TestManagerPass m Int
readTestAttr = unwrap' <$> readAttr @TestAttr

fromRight :: Either a b -> b
fromRight (Right b) = b

testAttrVisibilityBetweenPasses :: Int -> IO Int
testAttrVisibilityBetweenPasses i = fmap fromRight $ tryAll $ dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    setAttr (getTypeDesc @TestAttr) $ TestAttr -11
    Pass.eval' $ writeTestAttr i
    res <- Pass.eval' readTestAttr
    return res

testAttrVisibilityInsidePass :: Int -> IO Int
testAttrVisibilityInsidePass i = fmap fromRight $ tryAll $ dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    setAttr (getTypeDesc @TestAttr) $ TestAttr -11
    Pass.eval' $ readWriteAttr i

spec = do
    describe "Attr passing" $ do
        it "should pass changes in attributes between pass runs" $ do
            testAttrVisibilityBetweenPasses 42 `shouldReturn` 42
        it "should retain changes in attributes during a single pass run" $ do
            testAttrVisibilityInsidePass 42 `shouldReturn` 42
