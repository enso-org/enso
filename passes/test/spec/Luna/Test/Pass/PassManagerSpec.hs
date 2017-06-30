module Luna.Test.Pass.PassManagerSpec where


import           Luna.Prelude hiding (fromRight)
import           Luna.IR
import           OCI.Pass        (SubPass, Inputs, Outputs, Preserves, Events)
import qualified OCI.Pass        as Pass
import           System.Log
import System.Log.Logger.Format
import Control.Monad.Raise
import Test.Hspec (Spec, describe, it, shouldReturn)
import Data.TypeDesc
import Control.Monad.State.Dependent

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

data TestOutputPass
type instance Abstract TestOutputPass = TestOutputPass
type instance Inputs  Net   TestOutputPass = '[]
type instance Outputs Net   TestOutputPass = '[]
type instance Inputs  Layer TestOutputPass = '[]
type instance Outputs Layer TestOutputPass = '[]
type instance Inputs  Attr  TestOutputPass = '[]
type instance Outputs Attr  TestOutputPass = '[TestAttr]
type instance Inputs  Event TestOutputPass = '[]
type instance Outputs Event TestOutputPass = '[]
type instance Preserves     TestOutputPass = '[]

readWriteAttr :: (MonadRef m, MonadIO m, MonadPassManager m) => Int -> SubPass TestManagerPass m Int
readWriteAttr i = do
    putAttr @TestAttr $ TestAttr i
    unwrap <$> getAttr @TestAttr

writeTestAttr :: (MonadRef m, MonadIO m, Writers Attr '[TestAttr] m) => Int -> m ()
writeTestAttr = putAttr @TestAttr . TestAttr

readTestAttr :: (MonadRef m, MonadIO m, MonadPassManager m) => SubPass TestManagerPass m Int
readTestAttr = unwrap' <$> getAttr @TestAttr

fromRight :: Either a b -> b
fromRight (Right b) = b

testAttrVisibilityBetweenPasses :: Int -> IO Int
testAttrVisibilityBetweenPasses i = fmap fromRight $ tryAll $ dropLogs $ evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    setAttr (getTypeDesc @TestAttr) $ TestAttr -11
    Pass.eval' @TestManagerPass $ writeTestAttr i
    res <- Pass.eval' readTestAttr
    return res

testAttrVisibilityInsidePass :: Int -> IO Int
testAttrVisibilityInsidePass i = fmap fromRight $ tryAll $ dropLogs $ evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    setAttr (getTypeDesc @TestAttr) $ TestAttr -11
    Pass.eval' $ readWriteAttr i

testAttrVisibilityWhenAttrIsNotInput :: Int -> IO Int
testAttrVisibilityWhenAttrIsNotInput i = fmap fromRight $ tryAll $ dropLogs $ evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
    runRegs
    setAttr (getTypeDesc @TestAttr) $ TestAttr -11
    Pass.eval' @TestOutputPass $ writeTestAttr i
    res <- Pass.eval' readTestAttr
    return res

spec = do
    describe "Attr passing" $ do
        it "should pass changes in attributes between pass runs" $ do
            testAttrVisibilityBetweenPasses 42 `shouldReturn` 42
        it "should retain changes in attributes during a single pass run" $ do
            testAttrVisibilityInsidePass 42 `shouldReturn` 42
        it "should work with passes that only output a given Attr" $ do
            testAttrVisibilityWhenAttrIsNotInput 42 `shouldReturn` 42
