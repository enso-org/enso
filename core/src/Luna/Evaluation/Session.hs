module Luna.Evaluation.Session where

import           GHC.Prim                           (Any)
import qualified Language.Haskell.Session           as HS
import qualified Language.Haskell.Session.Hint.Eval as HEval
import           Prologue
import           Unsafe.Coerce



type Name = String
type Type = String



findSymbol :: HS.SessionMonad m => Name -> Type -> m Any
findSymbol n t = unsafeCoerce <$> HEval.interpretTyped n t

appArg :: Any -> Any -> Any
appArg = unsafeCoerce

unsafeCast :: Any -> a
unsafeCast = unsafeCoerce

toAny :: a -> Any
toAny = unsafeCoerce


run a = liftIO $ HS.run $ HS.setImports defaultImports >> a


defaultImports :: [HS.Import]
defaultImports = ["Prelude"]



test :: IO ()
test = HS.run $ do
    HS.setImports defaultImports
    length <- findSymbol "length" "String -> Int"
    let r = appArg length (unsafeCoerce "bla")
    print (unsafeCast r :: Int)
    return ()
