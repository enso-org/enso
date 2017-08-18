{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module Luna.Builtin.Std where

import           Control.Concurrent
import           Control.Monad.Except
import           Control.Monad.Trans.State                    (evalStateT, get)

import qualified Data.Aeson                                   as Aeson
import           Data.ByteString.Char8                        (pack)
import qualified Data.ByteString                              as StrictByteString
import           Data.ByteString.Lazy                         (ByteString)
import qualified Data.ByteString.Lazy                         as ByteString
import qualified Data.CaseInsensitive                         as CI
import           Data.Foldable                                (toList)
import qualified Data.HashMap.Lazy                            as HM
import           Data.IORef
import           Data.Int                                     (Int64)
import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map
import qualified Data.Map.Base                                as IMap
import qualified Data.MessagePack                             as MsgPack
import           Data.Scientific                              (toRealFloat)
import           Data.Set                                     (Set)
import qualified Data.Set                                     as Set
import           Data.Time                                    (DiffTime)
import qualified Data.Time                                    as Time
import           Data.Text.Lazy                               (Text)
import qualified Data.Text                                    as AnyText
import qualified Data.Text.Lazy                               as Text
import qualified Data.Text.Lazy.Encoding                      as Text
import qualified Data.Text.Encoding                           as T
import           Data.TypeDesc
import           Data.UUID                                    (UUID)
import qualified Data.UUID.V4                                 as UUID

import           GHC.IO.Handle                                (Handle)
import qualified GHC.IO.Handle                                as Handle

import           Luna.Builtin.Data.Class                      (Class (..))
import           Luna.Builtin.Data.Function                   as Function
import           Luna.Builtin.Data.LunaEff                    (LunaEff, performIO, runError, runIO, throw)
import           Luna.Builtin.Data.LunaValue                  (constructor, Constructor (..), fields, force', LunaData (..), LunaValue, Object (..), tag)
import           Luna.Builtin.Data.Module                     as Module
import           Luna.Builtin.Prim
import           Luna.IR                                      hiding (Function)
import           Luna.Pass.Data.ExprRoots
import           Luna.Pass.Data.UniqueNameGen
import           Luna.Pass.Evaluation.Interpreter
import           Luna.Pass.Inference.Data.MergeQueue          (MergeQueue(..))
import           Luna.Pass.Inference.Data.SimplifierQueue     (SimplifierQueue(..))
import           Luna.Pass.Inference.Data.Unifications        (Unifications(..))
import           Luna.Pass.Resolution.Data.CurrentTarget
import           Luna.Pass.Resolution.Data.UnresolvedAccs     (getAccs, UnresolvedAccs)
import qualified Luna.Pass.Transform.Desugaring.RemoveGrouped as RemoveGrouped
import           Luna.Pass.Typechecking.Typecheck
import qualified Luna.Pass.UnitCompilation.ClassProcessing    as ClassProcessing
import           Luna.Prelude                                 as P hiding (cons, Constructor, nothing, Text, toList)
import           Luna.Test.IR.Runner
import           Luna.Test.Utils

import qualified Network.HTTP.Client                          as HTTP
import qualified Network.HTTP.Simple                          as HTTP
import qualified Network.HTTP.Types.Header                    as HTTP

import           OCI.IR.Combinators
import           OCI.IR.Name.Qualified
import           OCI.Pass                                     (SubPass)
import qualified OCI.Pass                                     as Pass

import           System.Exit                                  (ExitCode (ExitFailure, ExitSuccess))
import           System.Process                               (CreateProcess, ProcessHandle, StdStream (CreatePipe, Inherit, NoStream, UseHandle))
import qualified System.Process                               as Process


stdlibImports :: [QualName]
stdlibImports = [ ["Std", "Base"] , ["Std", "HTTP"], ["Std", "System"], ["Std", "Time"]]

data LTp = LVar Name | LCons Name [LTp]

varNamesFromType :: LTp -> Set Name
varNamesFromType (LVar n)    = Set.singleton n
varNamesFromType (LCons n s) = varNamesFromTypes s

varNamesFromTypes :: [LTp] -> Set Name
varNamesFromTypes = Set.unions . fmap varNamesFromType

int :: Integer -> Int
int = fromIntegral

integer :: Int -> Integer
integer = fromIntegral

mkType :: (MonadRef m, MonadPassManager m) => Map Name (Expr Draft) -> LTp -> SubPass TestPass m (Expr Draft, Expr Draft)
mkType vars (LVar n)         = do
    let v = fromJust $ Map.lookup n vars
    mv  <- var "a"
    mon <- monadic v mv
    return (generalize mon, generalize mv)
mkType vars (LCons n fs) = do
    fields <- fmap fst <$> mapM (mkType vars) fs
    mv     <- var "a"
    cs     <- cons n fields
    mon    <- monadic cs mv
    return (generalize mon, generalize mv)

typeRepForIO :: LunaValue -> [LTp] -> LTp -> IO Function
typeRepForIO val args out = do
    res <- runPM False $ do
        runRegs
        Pass.eval' @TestPass $ do
            let varNames = Set.toList $ varNamesFromTypes $ out : args
            vars <- fmap generalize <$> mapM var varNames
            let varMap = Map.fromList $ zip varNames vars
            fieldTypes <- mapM (mkType varMap) args
            (outType, outM) <- mkType varMap out
            pure       <- cons_ @Draft "Pure"
            io         <- cons_ @Draft "IO"
            let lamInPure o i = do
                    l  <- lam i o
                    lm <- monadic l pure
                    return $ generalize lm
            funType <- foldM lamInPure outType $ fst <$> reverse fieldTypes
            monads  <- scanM (fmap generalize .: unify) (unsafeGeneralize io) (snd <$> fieldTypes)
            let lastMonad = head $ reverse monads
            replace lastMonad outM
            res <- compile $ generalize funType
            return $ Function res val $ Assumptions def (tail monads) def def
    case res of
        Right r -> return r
        Left e  -> error $ show e

compileFunction :: Imports -> SubPass TestPass (PMStack IO) SomeExpr -> IO Function
compileFunction imps pass = do
    Right res <- runPM False $ do
        runRegs
        root   <- Pass.eval' pass
        trans  <- typecheck TgtNone imps [unsafeGeneralize root]
        let newRoot = fromJust $ Map.lookup (unsafeGeneralize root) trans
        val              <- Pass.eval' $ interpret imps newRoot
        Just (unifies :: Unifications)    <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @Unifications)
        Just (merges  :: MergeQueue)      <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @MergeQueue)
        Just (apps    :: SimplifierQueue) <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @SimplifierQueue)
        Just (accs    :: UnresolvedAccs)  <- unsafeCoerce <$> unsafeGetAttr (getTypeDesc @UnresolvedAccs)
        rooted <- Pass.eval' @TestPass $ do
            tp <- getLayer @Type root >>= source
            let whiteList = Set.unions [Set.singleton (generalize tp), Set.fromList (generalize <$> unwrap unifies), Set.fromList (generalize <$> unwrap merges), Set.fromList (generalize <$> unwrap apps), Set.fromList (generalize <$> getAccs accs)]
            deepDeleteWithWhitelist root whiteList
            compile tp
        return $ Function rooted (evalStateT val def) $ Assumptions (unwrap unifies) (unwrap merges) (unwrap apps) (getAccs accs)
    return res

mkMonadProofFun' :: (MonadRef m, MonadPassManager m) => Maybe (Expr Draft) -> Expr Draft -> SubPass TestPass m ([Expr Unify], Expr Draft)
mkMonadProofFun' mon root = matchExpr root $ \case
    Lam i o -> do
        i'     <- source i
        newVar <- var "__mon"
        newArg <- monadic i' newVar
        (newMon, res) <- case mon of
            Just x  -> do
                m <- unify x newVar
                return (generalize m, [generalize m])
            Nothing -> return (generalize newVar, [])
        (r', newOut) <- mkMonadProofFun' (Just newMon) =<< source o
        newLam <- lam newArg newOut
        resultExpr <- fmap generalize  $ monadic newLam =<< cons_ @Draft "Pure"
        return (res ++ r', resultExpr)
    _ -> fmap ([],) $ fmap generalize $ monadic root $ fromJust mon

mkMonadProofFun :: (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass TestPass m (Assumptions, SomeExpr)
mkMonadProofFun tp = do
    (merges, transTp) <- mkMonadProofFun' Nothing tp
    deleteSubtree tp
    return (Assumptions def merges def def, generalize transTp)

withExceptions :: IO a -> LunaEff a
withExceptions a = do
    res <- performIO $ catchAll (Right <$> a) (return . Left . show)
    case res of
        Left a  -> throw a
        Right r -> return r

oneArgFun tArg1 tRes = runGraph $ do
    t1  <- cons_ @Draft tArg1
    t2  <- cons_ @Draft tRes
    l   <- lam t1 t2
    (assu, r) <- mkMonadProofFun $ generalize l
    cmp <- compile r
    return (assu, cmp)

twoArgFun tArg1 tArg2 tRes = runGraph $ do
    t1   <- cons_ @Draft tArg1
    t2   <- cons_ @Draft tArg2
    t3   <- cons_ @Draft tRes
    l1   <- lam t2 t3
    l2   <- lam t1 l1
    (assu, r) <- mkMonadProofFun $ generalize l2
    cmp <- compile r
    return (assu, cmp)


doubleClass :: Imports -> IO Class
doubleClass imps = do
    Right (boxed3DoublesAssumptions, boxed3Doubles) <- twoArgFun "Real" "Real" "Real"
    Right (boxed2DoublesAssumptions, boxed2Doubles) <- oneArgFun "Real" "Real"
    Right (double2BoolAssumptions, double2Bool)     <- twoArgFun "Real" "Real" "Bool"
    Right (double2TextAssumptions, double2text)     <- oneArgFun "Real" "Text"
    Right (double2JSONAssumptions, double2JSON)     <- oneArgFun "Real" "JSON"
    Right (double2DoubleAssumptions, double2Double) <- oneArgFun "Real" "Real"

    let plusVal     = toLunaValue tmpImps ((+)  :: Double -> Double -> Double)
        timeVal     = toLunaValue tmpImps ((*)  :: Double -> Double -> Double)
        minusVal    = toLunaValue tmpImps ((-)  :: Double -> Double -> Double)
        divVal      = toLunaValue tmpImps ((/)  :: Double -> Double -> Double)
        eqVal       = toLunaValue tmpImps ((==) :: Double -> Double -> Bool)
        ltVal       = toLunaValue tmpImps ((<)  :: Double -> Double -> Bool)
        gtVal       = toLunaValue tmpImps ((>)  :: Double -> Double -> Bool)
        showVal     = toLunaValue tmpImps (convert . show :: Double -> Text)
        toJSVal     = toLunaValue tmpImps (Aeson.toJSON :: Double -> Aeson.Value)
        toRealVal   = toLunaValue tmpImps (id   :: Double -> Double)
        tmpImps     = imps & importedClasses . at "Real" ?~ klass
        klass       = Class Map.empty $ Map.fromList [ ("+",        Function boxed3Doubles plusVal   boxed3DoublesAssumptions)
                                                     , ("*",        Function boxed3Doubles timeVal   boxed3DoublesAssumptions)
                                                     , ("-",        Function boxed3Doubles minusVal  boxed3DoublesAssumptions)
                                                     , ("/",        Function boxed3Doubles divVal    boxed3DoublesAssumptions)
                                                     , ("equals",   Function double2Bool   eqVal     double2BoolAssumptions)
                                                     , ("shortRep", Function double2text   showVal   double2TextAssumptions)
                                                     , ("toText",   Function double2text   showVal   double2TextAssumptions)
                                                     , ("toJSON",   Function double2JSON   toJSVal   double2JSONAssumptions)
                                                     , ("toReal",   Function double2Double toRealVal double2DoubleAssumptions)
                                                     ]
    return klass

intClass :: Imports -> IO Class
intClass imps = do
    Right (ints2BoolAssumptions, ints2Bool)   <- twoArgFun "Int" "Int" "Bool"
    Right (boxed3IntsAssumptions, boxed3Ints) <- twoArgFun "Int" "Int" "Int"
    Right (boxed2IntsAssumptions, boxed2Ints) <- oneArgFun "Int" "Int"
    Right (int2TextAssumptions, int2text)     <- oneArgFun "Int" "Text"
    Right (int2RealAssumptions, int2Real)     <- oneArgFun "Int" "Real"
    Right (int2JSONAssumptions, int2JSON)     <- oneArgFun "Int" "JSON"
    Right (int2TIAssumptions, int2TI)         <- oneArgFun "Int" "TimeInterval"

    let plusVal        = toLunaValue tmpImps ((+)  :: Integer -> Integer -> Integer)
        timeVal        = toLunaValue tmpImps ((*)  :: Integer -> Integer -> Integer)
        minusVal       = toLunaValue tmpImps ((-)  :: Integer -> Integer -> Integer)
        divVal         = toLunaValue tmpImps (div  :: Integer -> Integer -> Integer)
        eqVal          = toLunaValue tmpImps ((==) :: Integer -> Integer -> Bool)
        gtVal          = toLunaValue tmpImps ((>)  :: Integer -> Integer -> Bool)
        ltVal          = toLunaValue tmpImps ((<)  :: Integer -> Integer -> Bool)
        modVal         = toLunaValue tmpImps (mod  :: Integer -> Integer -> Integer)
        predVal        = toLunaValue tmpImps (pred :: Integer -> Integer)
        succVal        = toLunaValue tmpImps (succ :: Integer -> Integer)
        showVal        = toLunaValue tmpImps (convert . show :: Integer -> Text)
        toJSVal        = toLunaValue tmpImps (Aeson.toJSON   :: Integer -> Aeson.Value)
        toRealVal      = toLunaValue tmpImps (fromIntegral   :: Integer -> Double)
        secondsVal     = toLunaValue tmpImps (realToFrac            ::  Integer -> Time.DiffTime)
        minutesVal     = toLunaValue tmpImps ((* 60)   . realToFrac :: Integer -> Time.DiffTime)
        milisecondsVal = toLunaValue tmpImps ((/ 1000) . realToFrac :: Integer -> Time.DiffTime)
        tmpImps        = imps & importedClasses . at "Int" ?~ klass
        klass          = Class Map.empty $ Map.fromList [ ("+",           Function boxed3Ints plusVal        boxed3IntsAssumptions)
                                                        , ("*",           Function boxed3Ints timeVal        boxed3IntsAssumptions)
                                                        , ("-",           Function boxed3Ints minusVal       boxed3IntsAssumptions)
                                                        , ("div",         Function boxed3Ints divVal         boxed3IntsAssumptions)
                                                        , ("%",           Function boxed3Ints modVal         boxed3IntsAssumptions)
                                                        , ("pred",        Function boxed2Ints predVal        boxed2IntsAssumptions)
                                                        , ("succ",        Function boxed2Ints succVal        boxed2IntsAssumptions)
                                                        , ("seconds",     Function int2TI     secondsVal     int2TIAssumptions)
                                                        , ("minutes",     Function int2TI     minutesVal     int2TIAssumptions)
                                                        , ("miliseconds", Function int2TI     milisecondsVal int2TIAssumptions)
                                                        , ("shortRep",    Function int2text   showVal        int2TextAssumptions)
                                                        , ("toText",      Function int2text   showVal        int2TextAssumptions)
                                                        , ("toJSON",      Function int2JSON   toJSVal        int2JSONAssumptions)
                                                        , ("toReal",      Function int2Real   toRealVal      int2RealAssumptions)
                                                        , ("equals",      Function ints2Bool  eqVal          ints2BoolAssumptions)
                                                        , (">",           Function ints2Bool  gtVal          ints2BoolAssumptions)
                                                        , ("<",           Function ints2Bool  ltVal          ints2BoolAssumptions)
                                                        ]
    return klass

binaryClass :: Imports -> IO Class
binaryClass imps = do
    Right (toTextAssu, toTextIr) <- oneArgFun "Binary" "Text"
    Right (idAssu, idIr)         <- oneArgFun "Binary" "Binary"
    Right (eqAssu, eqIr)         <- twoArgFun "Binary" "Binary" "Bool"
    Right (plusAssu, plusIr)     <- twoArgFun "Binary" "Binary" "Binary"
    let toTextVal  = toLunaValue tmpImps Text.decodeUtf8
        idVal      = toLunaValue tmpImps (id   :: ByteString -> ByteString)
        eqVal      = toLunaValue tmpImps ((==) :: ByteString -> ByteString -> Bool)
        plusVal    = toLunaValue tmpImps ((<>) :: ByteString -> ByteString -> ByteString)
        bStrLen    = Text.pack . show . ByteString.length :: ByteString -> Text
        toShortRep = toLunaValue tmpImps $ \bs -> "Binary<" <> bStrLen bs <> ">"
        tmpImps    = imps & importedClasses . at "Binary" ?~ klass
        klass      = Class Map.empty $ Map.fromList [ ("toText",   Function toTextIr toTextVal  toTextAssu)
                                                    , ("toBinary", Function idIr     idVal      idAssu)
                                                    , ("equals",   Function eqIr     eqVal      eqAssu)
                                                    , ("+",        Function plusIr   plusVal    plusAssu)
                                                    , ("shortRep", Function toTextIr toShortRep toTextAssu)
                                                    ]
    return klass

stringClass :: Imports -> IO Class
stringClass imps = do
    Right (plusAssu, plusIr)           <- twoArgFun "Text" "Text" "Text"
    Right (eqAssu, eqIr)               <- twoArgFun "Text" "Text" "Bool"
    Right (textAssu, textIr)           <- oneArgFun "Text" "Text"
    Right (toJSONAssu, toJSONIr)       <- oneArgFun "Text" "JSON"
    Right (toBinaryAssu, toBinaryIr)   <- oneArgFun "Text" "Binary"
    Right (text2TimeAssu, text2TimeIr) <- oneArgFun "Text" "Time"
    Right (wordsAssu, wordsIr)         <- runGraph $ do
        tText <- cons_ @Draft "Text"
        tList   <- cons "List" [tText]
        l       <- lam tText tList
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)
    let plusVal       = toLunaValue tmpImps ((<>) :: Text -> Text -> Text)
        shortRepVal   = toLunaValue tmpImps (Text.take 100 :: Text -> Text)
        idVal         = toLunaValue tmpImps (id   :: Text -> Text)
        eqVal         = toLunaValue tmpImps ((==) :: Text -> Text -> Bool)
        gtVal         = toLunaValue tmpImps ((>)  :: Text -> Text -> Bool)
        ltVal         = toLunaValue tmpImps ((<)  :: Text -> Text -> Bool)
        isPrefixOfVal = toLunaValue tmpImps Text.isPrefixOf
        hasPrefixVal  = toLunaValue tmpImps (flip Text.isPrefixOf)
        wordsVal      = toLunaValue tmpImps Text.words
        linesVal      = toLunaValue tmpImps Text.lines
        lowercaseVal  = toLunaValue tmpImps Text.toLower
        uppercaseVal  = toLunaValue tmpImps Text.toUpper
        reverseVal    = toLunaValue tmpImps Text.reverse
        charsVal      = toLunaValue tmpImps (Text.chunksOf 1)
        toJSONVal     = toLunaValue tmpImps (Aeson.toJSON :: Text -> Aeson.Value)
        toBinaryVal   = toLunaValue tmpImps (Text.encodeUtf8 :: Text -> ByteString)
        escapeJSONVal = toLunaValue tmpImps (Text.decodeUtf8 . Aeson.encode :: Text -> Text)
        tmpImps       = imps & importedClasses . at "Text" ?~ klass
        klass         = Class Map.empty $ Map.fromList [ ("+",          Function plusIr     plusVal       plusAssu)
                                                       , ("equals",     Function eqIr       eqVal         eqAssu)
                                                       , (">",          Function eqIr       gtVal         eqAssu)
                                                       , ("<",          Function eqIr       ltVal         eqAssu)
                                                       , ("isPrefixOf", Function eqIr       isPrefixOfVal eqAssu)
                                                       , ("hasPrefix",  Function eqIr       hasPrefixVal  eqAssu)
                                                       , ("words",      Function wordsIr    wordsVal      wordsAssu)
                                                       , ("lines",      Function wordsIr    linesVal      wordsAssu)
                                                       , ("characters", Function wordsIr    charsVal      wordsAssu)
                                                       , ("lowercase",  Function textIr     lowercaseVal  textAssu)
                                                       , ("uppercase",  Function textIr     uppercaseVal  textAssu)
                                                       , ("reverse",    Function textIr     reverseVal    textAssu)
                                                       , ("shortRep",   Function textIr     shortRepVal   textAssu)
                                                       , ("toText",     Function textIr     idVal         textAssu)
                                                       , ("escapeJSON", Function textIr     escapeJSONVal textAssu)
                                                       , ("toJSON",     Function toJSONIr   toJSONVal     toJSONAssu)
                                                       , ("toBinary",   Function toBinaryIr toBinaryVal   toBinaryAssu)
                                                       ]
    return klass

preludeArithOp op = compileFunction def $ do
    a     <- var "a"
    b     <- var "b"
    acpl  <- acc a op
    apb   <- app acpl b
    l1    <- lam b apb
    l2    <- lam a l1
    tpA   <- var "a"
    monA  <- var "monA"
    monB  <- var "monB"
    monC  <- var "monC"
    montA <- monadic tpA monA
    montB <- monadic tpA monB
    montC <- monadic tpA monC
    tl1   <- lam montA montB
    pure  <- cons_ @Draft "Pure"
    tl1M  <- monadic tl1 pure
    tl2   <- lam montC tl1M
    tl2M  <- monadic tl2 pure
    reconnectLayer' @UserType (Just tl2M) l2
    return $ generalize l2

preludeCmpOp importBoxes op = compileFunction importBoxes $ do
    a    <- var "a"
    b    <- var "b"
    acpl <- acc a op
    apb  <- app acpl b
    l1   <- lam b apb
    l2   <- lam a l1
    tpA  <- var "a"
    bool' <- cons_ @Draft "Bool"
    monA <- var "monA"
    monB <- var "monB"
    monC <- var "monC"
    montA <- monadic tpA monA
    montB <- monadic bool' monB
    montC <- monadic tpA monC
    tl1   <- lam montA montB
    pure  <- cons_ @Draft "Pure"
    tl1M  <- monadic tl1 pure
    tl2   <- lam montC tl1M
    tl2M  <- monadic tl2 pure
    reconnectLayer' @UserType (Just tl2M) l2
    return $ generalize l2
    
prelude :: Imports -> IO Imports
prelude imps = mdo
    minus <- preludeArithOp "-"
    times <- preludeArithOp "*"
    mod   <- preludeArithOp "%"
    plus  <- preludeArithOp "+"
    gt    <- preludeCmpOp importBoxes ">"
    lt    <- preludeCmpOp importBoxes "<"
    eq    <- preludeCmpOp importBoxes "equals"
    let funMap = Map.fromList [("+", plus), ("-", minus), ("*", times), (">", gt), ("<", lt), ("==", eq), ("%", mod)]
    string <- stringClass importBoxes
    int    <- intClass    importBoxes
    double <- doubleClass importBoxes
    binary <- binaryClass importBoxes
    let importBoxes = unionImports imps $ Imports (Map.fromList [("Int", int), ("Text", string), ("Real", double), ("Binary", binary)]) funMap
    return importBoxes


stdlib :: Imports -> IO Imports
stdlib = prelude

newtype GCState = GCState (Map UUID (IO ()))
makeWrapped ''GCState

instance Default GCState where
    def = GCState def

cleanupGC :: GCState -> IO GCState
cleanupGC st = sequence (unwrap st) >> return def

systemStd :: Imports -> IO (IO (), Imports)
systemStd imps = do
    std     <- stdlib imps

    gcState <- newMVar $ GCState Map.empty

    let registerGC :: IO () -> IO UUID
        registerGC act = do
            uuid <- UUID.nextRandom
            modifyMVar_ gcState $ return . (wrapped . at uuid ?~ act)
            return uuid

        deregisterGC :: UUID -> IO ()
        deregisterGC uuid = modifyMVar_ gcState $ return . (wrapped . at uuid .~ Nothing)

        cleanup :: IO ()
        cleanup = modifyMVar_ gcState cleanupGC

    let putStr :: Text -> LunaEff ()
        putStr = performIO . putStrLn . convert
    Right printLn <- runGraph $ do
        mA     <- var "a"
        io     <- cons_ @Draft "IO"
        comm   <- unify mA io
        tpInt  <- cons_ @Draft "Text"
        tpIntM <- monadic tpInt mA
        tpNo   <- cons_ @Draft "None"
        tpNoM  <- monadic tpNo comm
        intl   <- lam tpIntM tpNoM
        pure   <- cons_ @Draft "Pure"
        lamP   <- monadic intl pure
        res    <- compile $ generalize lamP
        return $ Function res (toLunaValue std putStr) $ Assumptions def [generalize comm] def def

    let errVal = toLunaValue std $ \(a :: Text) -> (throw $ "Luna error: " ++ convert a :: LunaValue)
    Right err <- runGraph $ do
        tpInt <- cons_ @Draft "Text"
        a     <- var "a"
        tl    <- lam tpInt a
        (assu, r) <- mkMonadProofFun $ generalize tl
        cmp <- compile r
        return $ Function cmp errVal assu

    let readFileVal :: Text -> LunaEff Text
        readFileVal = withExceptions . fmap convert . readFile . convert
    readFileF <- typeRepForIO (toLunaValue std readFileVal) [LCons "Text" []] $ LCons "Text" []

    let readBinaryVal :: Text -> LunaEff ByteString
        readBinaryVal = withExceptions . ByteString.readFile . convert
    readBinaryF <- typeRepForIO (toLunaValue std readBinaryVal) [LCons "Text"[]] $ LCons "Binary" []

    let parseJSONVal :: Text -> LunaEff Aeson.Value
        parseJSONVal t = case Aeson.eitherDecode $ Text.encodeUtf8 t of
            Left e  -> throw e
            Right a -> return a
    Right parseJSON <- runGraph $ do
        mA     <- var "a"
        pure   <- cons_ @Draft "Pure"
        comm   <- unify mA pure
        tpInt  <- cons_ @Draft "Text"
        tpIntM <- monadic tpInt mA
        tpNo   <- cons_ @Draft "JSON"
        tpNoM  <- monadic tpNo comm
        intl   <- lam tpIntM tpNoM
        lamP   <- monadic intl pure
        res    <- compile $ generalize lamP
        return $ Function res (toLunaValue std parseJSONVal) $ Assumptions def [generalize comm] def def

    Right (mpack2BinaryAssu, mpack2BinaryIr) <- oneArgFun "MsgPack" "Binary"
    let encodeMsgPackVal = toLunaValue std (MsgPack.pack :: MsgPack.Object -> ByteString)
        encodeMsgPack    = Function mpack2BinaryIr encodeMsgPackVal mpack2BinaryAssu

    let parseMsgPackVal :: ByteString -> LunaEff MsgPack.Object
        parseMsgPackVal = withExceptions . MsgPack.unpack
    parseMsgPack <- typeRepForIO (toLunaValue std parseMsgPackVal) [LCons "Binary" []] $ LCons "MsgPack" []

    let primPerformHttpVal :: Text -> Text -> [(Text, Text)] -> Maybe (Text, Text) -> [(Text, Maybe Text)] -> ByteString -> LunaEff (HTTP.Response HTTP.BodyReader)
        primPerformHttpVal uri method headers auth params body = performIO $ do
            let packHeader (k, v) = (CI.mk $ convert k, convert v)
                packParam  (k, v) = (convert k, convert <$> v)
            baseReq <- HTTP.parseRequest (convert uri)
            manager <- HTTP.newManager HTTP.defaultManagerSettings
            let newHeaders = map packHeader headers
                oldHeaders = HTTP.requestHeaders baseReq
                req        = baseReq
                    & HTTP.setRequestBodyLBS body
                    & HTTP.setRequestMethod  (convert method)
                    & HTTP.setRequestHeaders (oldHeaders <> newHeaders)
                    & HTTP.addRequestHeader  HTTP.hAccept (pack "*/*")
                    & HTTP.setRequestQueryString (map packParam params)
                    & case auth of
                        Just (u, p) -> HTTP.setRequestBasicAuth (convert u) (convert p)
                        Nothing -> id
            HTTP.responseOpen req manager

    let textT           = LCons "Text"   []
        tupleT          = LCons "Tuple2" [textT, textT]
        maybeTupleT     = LCons "Maybe"  [tupleT]
        tupleListT      = LCons "List"   [tupleT]
        tupleMaybeListT = LCons "List"   [LCons "Tuple2" [textT, LCons "Maybe" [textT]]]
    primPerformHttp <- typeRepForIO (toLunaValue std primPerformHttpVal)
                                    [textT, textT, tupleListT, maybeTupleT, tupleMaybeListT, LCons "Binary" []]
                                    (LCons "HttpResponse" [])

    let primGetCurrentTimeVal :: LunaEff (Time.UTCTime)
        primGetCurrentTimeVal = performIO Time.getCurrentTime
    primGetCurrentTime <- typeRepForIO (toLunaValue std primGetCurrentTimeVal) [] $ LCons "Time" []

    Right (times2IntervalAssu, times2IntervalIr) <- twoArgFun "Time" "Time" "TimeInterval"
    let primDiffTimesVal = toLunaValue std Time.diffUTCTime
        primDiffTimes    = Function times2IntervalIr primDiffTimesVal times2IntervalAssu

    Right (time2TextAssu, time2TextIr) <- oneArgFun "Time" "Text"
    let primShowTimeVal = toLunaValue std (convert . show :: Time.UTCTime -> Text)
        primShowTime    = Function time2TextIr primShowTimeVal time2TextAssu

    Right (times2BoolAssu, times2BoolIr) <- twoArgFun "Time" "Time" "Bool"
    let primTimesEqVal = toLunaValue std ((==) :: Time.UTCTime -> Time.UTCTime -> Bool)
        primTimesEq    = Function times2BoolIr primTimesEqVal times2BoolAssu

    Right (text2TimeAssu, text2TimeIr) <- runGraph $ do
        tText  <- cons_ @Draft "Text"
        tTime  <- cons_ @Draft "Time"
        tMaybe <- cons "Maybe" [tTime]
        l1     <- lam tText tMaybe
        l2     <- lam tText l1
        (assu, r) <- mkMonadProofFun $ generalize l2
        cmp    <- compile r
        return (assu, cmp)
    let parseTime :: Text -> Text -> Maybe Time.UTCTime
        parseTime fmt str = Time.parseTime Time.defaultTimeLocale (convert fmt) (convert str)
        primParseTimeVal  = toLunaValue std parseTime
        primParseTime     = Function text2TimeIr primParseTimeVal text2TimeAssu

    let sleepVal = performIO . threadDelay . int
    sleep <- typeRepForIO (toLunaValue std sleepVal) [LCons "Int" []] $ LCons "None" []

    let forkVal :: LunaEff () -> LunaEff ()
        forkVal act = performIO $ mdo
            uid <- registerGC $ killThread tid
            tid <- forkFinally (void $ runIO $ runError act) $ const (deregisterGC uid)
            return ()
    fork <- typeRepForIO (toLunaValue std forkVal) [LCons "None" []] $ LCons "None" []

    let newEmptyMVarVal :: LunaEff (MVar LunaData)
        newEmptyMVarVal = performIO newEmptyMVar
    newEmptyMVar' <- typeRepForIO (toLunaValue std newEmptyMVarVal) [] (LCons "MVar" [LVar "a"])

    let putMVarVal :: MVar LunaData -> LunaData -> LunaEff ()
        putMVarVal = performIO .: putMVar
    putMVar' <- typeRepForIO (toLunaValue std putMVarVal) [LCons "MVar" [LVar "a"], LVar "a"] (LCons "None" [])

    let takeMVarVal :: MVar LunaData -> LunaValue
        takeMVarVal = performIO . takeMVar
        readMVarVal :: MVar LunaData -> LunaValue
        readMVarVal = performIO . readMVar
    takeMVar' <- typeRepForIO (toLunaValue std takeMVarVal) [LCons "MVar" [LVar "a"]] (LVar "a")

    let writeFileVal :: Text -> Text -> LunaEff ()
        writeFileVal p c = withExceptions $ writeFile (convert p) (convert c)
    writeFile' <- typeRepForIO (toLunaValue std writeFileVal) [LCons "Text" [], LCons "Text" []] (LCons "None" [])

    let runProcessVal :: CreateProcess -> LunaEff (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
        runProcessVal = withExceptions . Process.createProcess
    runProcess' <- typeRepForIO (toLunaValue std runProcessVal) [ LCons "ProcessDescription" [] ] ( LCons "ProcessResults" [ LCons "Maybe" [ LCons "FileHandle" [] ]
                                                                                                                           , LCons "Maybe" [ LCons "FileHandle" [] ]
                                                                                                                           , LCons "Maybe" [ LCons "FileHandle" [] ]
                                                                                                                           , LCons "ProcessHandle" [] ] )
    let hIsOpenVal :: Handle -> LunaEff Bool
        hIsOpenVal = withExceptions . Handle.hIsOpen
    hIsOpen' <- typeRepForIO (toLunaValue std hIsOpenVal) [LCons "FileHandle" []] (LCons "Bool" [])

    let hIsClosedVal :: Handle -> LunaEff Bool
        hIsClosedVal = withExceptions . Handle.hIsClosed
    hIsClosed' <- typeRepForIO (toLunaValue std hIsClosedVal) [LCons "FileHandle" []] (LCons "Bool" [])

    let hCloseVal :: Handle -> LunaEff ()
        hCloseVal = withExceptions . Handle.hClose
    hClose' <- typeRepForIO (toLunaValue std hCloseVal) [LCons "FileHandle" []] (LCons "None" [])

    let hGetContentsVal :: Handle -> LunaEff Text
        hGetContentsVal = fmap Text.pack . withExceptions . Handle.hGetContents
    hGetContents' <- typeRepForIO (toLunaValue std hGetContentsVal) [LCons "FileHandle" []] (LCons "Text" [])

    let hGetLineVal :: Handle -> LunaEff Text
        hGetLineVal = fmap Text.pack . withExceptions . Handle.hGetLine
    hGetLine' <- typeRepForIO (toLunaValue std hGetLineVal) [LCons "FileHandle" []] (LCons "Text" [])

    let hPutTextVal :: Handle -> Text -> LunaEff ()
        hPutTextVal h = withExceptions . Handle.hPutStr h . Text.unpack
    hPutText' <- typeRepForIO (toLunaValue std hPutTextVal) [LCons "FileHandle" [], LCons "Text" []] (LCons "None" [])

    let waitForProcessVal :: ProcessHandle -> LunaEff ExitCode
        waitForProcessVal = withExceptions . Process.waitForProcess
    waitForProcess' <- typeRepForIO (toLunaValue std waitForProcessVal) [LCons "ProcessHandle" []] (LCons "ExitCode" [])

    let systemModule = Map.fromList [ ("putStr", printLn)
                                    , ("errorStr", err)
                                    , ("readFile", readFileF)
                                    , ("readBinary", readBinaryF)
                                    , ("writeFile", writeFile')
                                    , ("parseJSON", parseJSON)
                                    , ("parseMsgPack", parseMsgPack)
                                    , ("encodeMsgPack", encodeMsgPack)
                                    , ("primPerformHttp", primPerformHttp)
                                    , ("primGetCurrentTime", primGetCurrentTime)
                                    , ("primDiffTimes", primDiffTimes)
                                    , ("primShowTime", primShowTime)
                                    , ("primTimesEq", primTimesEq)
                                    , ("primParseTime", primParseTime)
                                    , ("primFork", fork)
                                    , ("sleep", sleep)
                                    , ("primNewMVar", newEmptyMVar')
                                    , ("primPutMVar", putMVar')
                                    , ("primTakeMVar", takeMVar')
                                    , ("primReadMVar", takeMVar' & Function.value .~ toLunaValue std readMVarVal)
                                    , ("primRunProcess", runProcess')
                                    , ("primHIsOpen", hIsOpen')
                                    , ("primHIsClosed", hIsClosed')
                                    , ("primHClose", hClose')
                                    , ("primHGetContents", hGetContents')
                                    , ("primHGetLine", hGetLine')
                                    , ("primHPutText", hPutText')
                                    , ("primWaitForProcess", waitForProcess')
                                    ]

    return $ (cleanup, std & importedFunctions %~ Map.union systemModule)

unexpectedConstructorFor name = throw $ "Expected a " <> name <> " luna object, got unexpected constructor"

instance ToLunaValue a => ToLunaValue (IO a) where
    toLunaValue imps = toLunaValue imps <=< withExceptions

instance ToLunaData StrictByteString.ByteString where
    toLunaData imps = toLunaData imps . ByteString.fromStrict

instance (ToLunaValue b) => ToLunaData (HTTP.Response b) where
    toLunaData imps v = LunaObject $ Object (
            Constructor "HttpResponse"
                [ toLunaData imps . integer   $ HTTP.getResponseStatusCode v
                , LunaThunk . toLunaValue imps $ HTTP.responseBody v
                ]
            ) (getObjectMethodMap "HttpResponse" imps)

instance (ToLunaData a, ToLunaData b) => ToLunaData (IMap.Map a b) where
    toLunaData imps v = LunaObject $ Object (constructorOf v) $ getObjectMethodMap "Map" imps where
        constructorOf IMap.Tip             = Constructor "Tip" []
        constructorOf (IMap.Bin s k v l r) = Constructor "Bin" [toLunaData imps $ integer s, toLunaData imps k, toLunaData imps v, toLunaData imps l, toLunaData imps r]

instance ToLunaData Aeson.Value where
    toLunaData imps v = LunaObject $ Object (constructorOf v) $ getObjectMethodMap "JSON" imps where
        constructorOf (Aeson.Array  a) = Constructor "JSONArray"  [toLunaData imps . toList $ a]
        constructorOf (Aeson.String a) = Constructor "JSONString" [toLunaData imps (convert a :: Text)]
        constructorOf (Aeson.Number a) = Constructor "JSONNumber" [toLunaData imps (toRealFloat a :: Double)]
        constructorOf (Aeson.Bool   a) = Constructor "JSONBool"   [toLunaData imps a]
        constructorOf  Aeson.Null      = Constructor "JSONNull"   []
        constructorOf (Aeson.Object a) = Constructor "JSONObject" [toLunaData imps $ (Map.mapKeys convert $ Map.fromList $ HM.toList a :: Map Text Aeson.Value)]

instance FromLunaData CreateProcess where
    fromLunaData v = let errorMsg = "Expected a ProcessDescription luna object, got unexpected constructor" in
        force' v >>= \case
            LunaObject obj -> case obj ^. constructor . fields of
                [processPath, args, mayStdIn, mayStdOut, mayStdErr] -> do
                    p      <- Process.proc <$> fmap Text.unpack (fromLunaData processPath) <*> fmap (map Text.unpack) (fromLunaData args)
                    stdIn  <- fromMaybe Inherit <$> fromLunaData mayStdIn
                    stdOut <- fromMaybe Inherit <$> fromLunaData mayStdOut
                    stdErr <- fromMaybe Inherit <$> fromLunaData mayStdErr
                    return $ p { Process.std_in = stdIn, Process.std_out = stdOut, Process.std_err = stdErr }
                _ -> throw errorMsg
            _ -> throw errorMsg

instance FromLunaData StdStream where
    fromLunaData v = let errorMsg = "Expected a PipeRequest luna object, got unexpected constructor" in
        force' v >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "Inherit"    -> return Inherit
                "UseHandle"  -> UseHandle <$> (fromLunaData . head $ obj ^. constructor . fields)
                "CreatePipe" -> return CreatePipe
                "NoStream"   -> return NoStream
                _            -> throw errorMsg
            _ -> throw errorMsg

instance FromLunaData ExitCode where
    fromLunaData v = force' v >>= \case
        LunaObject obj -> case obj ^. constructor . tag of
            "ExitSuccess" -> return ExitSuccess
            "ExitFailure" -> fmap (ExitFailure . int) . fromLunaData . head $ obj ^. constructor . fields
        _ -> throw "Expected a ExitCode luna object, got unexpected constructor"

instance ToLunaData ExitCode where
    toLunaData imps ec =
        let makeConstructor ExitSuccess     = Constructor "ExitSuccess" []
            makeConstructor (ExitFailure c) = Constructor "ExitFailure" [toLunaData imps $ integer c] in
        LunaObject $ Object (makeConstructor ec) $ getObjectMethodMap "ExitCode" imps

instance IsBoxed "FileHandle"    Handle
instance IsBoxed "ProcessHandle" ProcessHandle
instance IsBoxed "StrictText"    AnyText.Text

instance ToLunaData (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) where
    toLunaData imps (hin, hout, herr, ph) = LunaObject $ Object (Constructor "ProcessResults" [toLunaData imps hin, toLunaData imps hout, toLunaData imps herr, toLunaData imps ph]) $ getObjectMethodMap "ProcessResults" imps

instance FromLunaData MsgPack.Object where
    fromLunaData v = force' v >>= \case
        LunaObject obj -> case obj ^. constructor . tag of
            "MPNull"   -> return MsgPack.ObjectNil
            "MPBool"   -> MsgPack.ObjectBool <$> (fromLunaData . head $ obj ^. constructor . fields)
            "MPInt"    -> (MsgPack.ObjectInt . (fromIntegral :: Integer -> Int64)) <$> (fromLunaData . head $ obj ^. constructor . fields)
            "MPReal"   -> MsgPack.ObjectDouble <$> (fromLunaData . head $ obj ^. constructor . fields)
            "MPString" -> (MsgPack.ObjectStr . Text.toStrict)       <$> (fromLunaData . head $ obj ^. constructor . fields)
            "MPBinary" -> (MsgPack.ObjectBin . ByteString.toStrict) <$> (fromLunaData . head $ obj ^. constructor . fields)
            "MPArray"  -> MsgPack.ObjectArray <$> (fromLunaData . head $ obj ^. constructor . fields)
            "MPMap"    -> MsgPack.ObjectMap   <$> (fromLunaData . head $ obj ^. constructor . fields)
            _          -> unexpectedConstructorFor "MsgPack"
        _ -> unexpectedConstructorFor "MsgPack"

instance ToLunaData MsgPack.Object where
    toLunaData imps  MsgPack.ObjectNil       = LunaObject $ Object (Constructor "MPNull"   [])                                              (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectBool   b) = LunaObject $ Object (Constructor "MPBool"   [toLunaData imps b])                             (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectInt    i) = LunaObject $ Object (Constructor "MPInt"    [toLunaData imps $ (fromIntegral i :: Integer)]) (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectWord   w) = LunaObject $ Object (Constructor "MPInt"    [toLunaData imps $ (fromIntegral w :: Integer)]) (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectFloat  f) = LunaObject $ Object (Constructor "MPReal"   [toLunaData imps $ (realToFrac   f :: Double)])  (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectDouble d) = LunaObject $ Object (Constructor "MPReal"   [toLunaData imps d])                             (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectStr    s) = LunaObject $ Object (Constructor "MPString" [toLunaData imps $ Text.fromStrict s])           (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectBin    b) = LunaObject $ Object (Constructor "MPBinary" [toLunaData imps $ ByteString.fromStrict b])     (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectArray  l) = LunaObject $ Object (Constructor "MPArray"  [toLunaData imps l])                             (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectMap    m) = LunaObject $ Object (Constructor "MPMap"    [toLunaData imps m])                             (getObjectMethodMap "MsgPack" imps)
    toLunaData imps (MsgPack.ObjectExt  _ _) = LunaError "MessagePack ObjectExt is not supported."

instance FromLunaData Time.DiffTime where
    fromLunaData dt = let errorMsg = "Expected a TimeInterval luna object, got unexpected constructor" in
        force' dt >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "TimeInterval" -> fmap Time.picosecondsToDiffTime . fromLunaData . head $ obj ^. constructor . fields
                _              -> throw errorMsg
            _  -> throw errorMsg

instance FromLunaData Time.NominalDiffTime where
    fromLunaData dt = realToFrac <$> (fromLunaData dt :: LunaEff Time.DiffTime)

instance FromLunaData Time.UTCTime where
    fromLunaData t = let errorMsg = "Expected a Time luna object, got unexpected constructor" in
        force' t >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "TimeVal" -> Time.UTCTime <$> (Time.ModifiedJulianDay <$> fromLunaData days) <*> fromLunaData diff where [days, diff] = obj ^. constructor . fields
                _         -> throw errorMsg
            _ -> throw errorMsg

instance ToLunaData Time.DiffTime where
    toLunaData imps diffTime = LunaObject $ Object (Constructor "TimeInterval" [toLunaData imps $ Time.diffTimeToPicoseconds diffTime]) (getObjectMethodMap "TimeInterval" imps) 

instance ToLunaData Time.NominalDiffTime where
    toLunaData imps nDiffTime = toLunaData imps (realToFrac nDiffTime :: Time.DiffTime)

instance ToLunaData Time.UTCTime where
    toLunaData imps (Time.UTCTime days diff) = LunaObject $ Object (Constructor "TimeVal" [toLunaData imps $ Time.toModifiedJulianDay days, toLunaData imps diff]) (getObjectMethodMap "Time" imps)

