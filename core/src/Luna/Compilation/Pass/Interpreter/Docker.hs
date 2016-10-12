module Luna.Compilation.Pass.Interpreter.Docker where

import           Prelude.Luna
import           Luna.Compilation.Pass.Interpreter.Value
import           Control.Monad.Except
import           System.Process                          (callProcess, readProcessWithExitCode)
import           System.Exit
import qualified Data.Map                                as Map
import           Unsafe.Coerce

data DockerConf = DockerConf { _container :: String
                             , _mounts    :: [(String, String)]
                             , _workdir   :: Maybe String
                             } deriving (Show, Eq)

makeLenses ''DockerConf

mkDockerConf :: String -> DockerConf
mkDockerConf n = DockerConf n [] Nothing

dockerConfClass :: ClassDescription
dockerConfClass = ClassDescription $ Map.fromList
    [ ("setContainer", toMethodBoxed $ \d c   -> d & container .~ c)
    , ("setPWD",       toMethodBoxed $ \d dir -> d & workdir ?~ dir)
    , ("mount",        toMethodBoxed $ \d s t -> d & mounts    %~ ((s, t) :))
    , ("run",          toMethodBoxed runDocker)
    , ("readFile",     toMethodBoxed readDockerFile)
    ]

runDocker :: DockerConf -> String -> LunaM String
runDocker conf cmd = do
    let ms  = case conf ^. mounts of
          [] -> []
          ms -> "-v" : ((\(a, b) -> a ++ ":" ++ b) <$> conf ^. mounts)
    let pwd = case conf ^. workdir of
          Just w -> ["-w", w]
          _      -> []
    let command = ["run", "--rm"] ++ pwd ++ ms ++ [conf ^. container] ++ ["/bin/bash", "-c", cmd]
    (code, out, err) <- liftIO $ readProcessWithExitCode "docker" command ""
    case code of
        ExitSuccess   -> return out
        ExitFailure c -> throwError $ unlines [show c, err]

readDockerFile :: DockerConf -> String -> LunaM String
readDockerFile conf path = runDocker conf $ "cat " ++ path

instance ToData   DockerConf where unsafeToData = Boxed . Object dockerConfClass . unsafeCoerce
instance FromData DockerConf where unsafeFromData (Boxed (Object _ d)) = unsafeCoerce d
