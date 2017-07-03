{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module Luna.Builtin.Std where

import Luna.Prelude  as P hiding (cons, Constructor, nothing, Text, toList)
import Data.Foldable (toList)
import Luna.IR       hiding (Function)
import Luna.Test.Utils
import Luna.Test.IR.Runner
import Luna.Builtin.Data.Class (Class (..))
import Luna.Builtin.Prim
import Luna.Builtin.Data.Module   as Module
import Luna.Builtin.Data.Function as Function
import qualified Data.Map as Map
import           Data.Map (Map)
import OCI.Pass (SubPass)
import OCI.IR.Combinators
import qualified OCI.Pass        as Pass
import Luna.Builtin.Data.LunaValue  (LunaValue, LunaData (..), Constructor (..), Object (..), force')
import Luna.Builtin.Data.LunaEff    (runError, throw, runIO, performIO, LunaEff)
import Luna.Pass.Evaluation.Interpreter
import Luna.Pass.Typechecking.Typecheck
import Control.Monad.Trans.State             (evalStateT, get)
import Control.Monad.Except
import Luna.Pass.Inference.Data.Unifications    (Unifications)
import Luna.Pass.Inference.Data.SimplifierQueue (SimplifierQueue)
import Luna.Pass.Inference.Data.MergeQueue      (MergeQueue)
import Luna.Pass.Resolution.Data.UnresolvedAccs (UnresolvedAccs, getAccs)
import Luna.Pass.Resolution.Data.CurrentTarget
import Luna.Pass.Data.UniqueNameGen
import Luna.Pass.Data.ExprRoots
import qualified Luna.Pass.Transform.Desugaring.RemoveGrouped  as RemoveGrouped
import qualified Luna.Pass.UnitCompilation.ClassProcessing     as ClassProcessing
import Data.TypeDesc
import qualified Data.Set as Set
import           Data.Set (Set)
import OCI.IR.Name.Qualified
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Aeson as Aeson
import           Data.Scientific (toRealFloat)
import qualified Data.Map.Base     as IMap
import qualified Data.HashMap.Lazy as HM
import           Data.UUID         (UUID)
import qualified Data.UUID.V4      as UUID

import Data.IORef
import Control.Concurrent

-- http

import qualified Network.HTTP.Simple as HTTP

stdlibImports :: [QualName]
stdlibImports = [ ["Std", "Base"] , ["Std", "HTTP"] ]

data LTp = LVar Name | LCons Name [LTp]

varNamesFromType :: LTp -> Set Name
varNamesFromType (LVar n)    = Set.singleton n
varNamesFromType (LCons n s) = varNamesFromTypes s

varNamesFromTypes :: [LTp] -> Set Name
varNamesFromTypes = Set.unions . fmap varNamesFromType

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

doubleClass :: Imports -> IO Class
doubleClass imps = do
    Right (boxed3DoublesAssumptions, boxed3Doubles) <- runGraph $ do
        tDouble <- cons_ @Draft "Real"
        l1   <- lam tDouble tDouble
        l2   <- lam tDouble l1
        (assu, r) <- mkMonadProofFun $ generalize l2
        cmp <- compile r
        return (assu, cmp)

    Right (boxed2DoublesAssumptions, boxed2Doubles) <- runGraph $ do
        tDouble <- cons_ @Draft "Real"
        l    <- lam tDouble tDouble
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)

    Right (double2StringAssumptions, double2string) <- runGraph $ do
        tDouble <- cons_ @Draft "Real"
        tStr <- cons_ @Draft "Text"
        l    <- lam tDouble tStr
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)

    Right (double2JSONAssumptions, double2JSON) <- runGraph $ do
        tDouble <- cons_ @Draft "Real"
        tStr <- cons_ @Draft "JSON"
        l    <- lam tDouble tStr
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)

    let plusVal     = toLunaValue tmpImps ((+)  :: Double -> Double -> Double)
        timeVal     = toLunaValue tmpImps ((*)  :: Double -> Double -> Double)
        minusVal    = toLunaValue tmpImps ((-)  :: Double -> Double -> Double)
        divVal      = toLunaValue tmpImps ((/)  :: Double -> Double -> Double)
        showVal     = toLunaValue tmpImps (convert . show :: Double -> Text)
        toJSVal     = toLunaValue tmpImps (Aeson.toJSON :: Double -> Aeson.Value)
        tmpImps     = imps & importedClasses . at "Real" ?~ klass
        klass       = Class Map.empty $ Map.fromList [ ("+",        Function boxed3Doubles plusVal  boxed3DoublesAssumptions)
                                                     , ("*",        Function boxed3Doubles timeVal  boxed3DoublesAssumptions)
                                                     , ("-",        Function boxed3Doubles minusVal boxed3DoublesAssumptions)
                                                     , ("/",        Function boxed3Doubles divVal   boxed3DoublesAssumptions)
                                                     , ("shortRep", Function double2string showVal  double2StringAssumptions)
                                                     , ("toString", Function double2string showVal  double2StringAssumptions)
                                                     , ("toJSON",   Function double2JSON   toJSVal  double2JSONAssumptions)
                                                     ]
    return klass

intClass :: Imports -> IO Class
intClass imps = do
    Right (ints2BoolAssumptions, ints2Bool) <- runGraph $ do
        tInt  <- cons_ @Draft "Int"
        tBool <- cons_ @Draft "Bool"
        l1    <- lam tInt tBool
        l2    <- lam tInt l1
        (assu, r) <- mkMonadProofFun $ generalize l2
        cmp <- compile r
        return (assu, cmp)

    Right (boxed3IntsAssumptions, boxed3Ints) <- runGraph $ do
        tInt <- cons_ @Draft "Int"
        l1   <- lam tInt tInt
        l2   <- lam tInt l1
        (assu, r) <- mkMonadProofFun $ generalize l2
        cmp <- compile r
        return (assu, cmp)

    Right (boxed2IntsAssumptions, boxed2Ints) <- runGraph $ do
        tInt <- cons_ @Draft "Int"
        l    <- lam tInt tInt
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)

    Right (int2StringAssumptions, int2string) <- runGraph $ do
        tInt <- cons_ @Draft "Int"
        tStr <- cons_ @Draft "Text"
        l    <- lam tInt tStr
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)

    Right (int2RealAssumptions, int2Real) <- runGraph $ do
        tInt <- cons_ @Draft "Int"
        tStr <- cons_ @Draft "Real"
        l    <- lam tInt tStr
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)

    Right (int2JSONAssumptions, int2JSON) <- runGraph $ do
        tInt <- cons_ @Draft "Int"
        tStr <- cons_ @Draft "JSON"
        l    <- lam tInt tStr
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)

    let plusVal        = toLunaValue tmpImps ((+)  :: Int -> Int -> Int)
        timeVal        = toLunaValue tmpImps ((*)  :: Int -> Int -> Int)
        minusVal       = toLunaValue tmpImps ((-)  :: Int -> Int -> Int)
        divVal         = toLunaValue tmpImps (div  :: Int -> Int -> Int)
        eqVal          = toLunaValue tmpImps ((==) :: Int -> Int -> Bool)
        gtVal          = toLunaValue tmpImps ((>)  :: Int -> Int -> Bool)
        ltVal          = toLunaValue tmpImps ((<)  :: Int -> Int -> Bool)
        modVal         = toLunaValue tmpImps (mod  :: Int -> Int -> Int)
        predVal        = toLunaValue tmpImps (pred :: Int -> Int)
        succVal        = toLunaValue tmpImps (succ :: Int -> Int)
        showVal        = toLunaValue tmpImps (convert . show :: Int -> Text)
        toJSVal        = toLunaValue tmpImps (Aeson.toJSON :: Int -> Aeson.Value)
        toRealVal      = toLunaValue tmpImps (fromIntegral :: Int -> Double)
        secondsVal     = toLunaValue tmpImps ((* 1000000) :: Int -> Int)
        milisecondsVal = toLunaValue tmpImps ((* 1000)    :: Int -> Int)
        tmpImps        = imps & importedClasses . at "Int" ?~ klass
        klass          = Class Map.empty $ Map.fromList [ ("+",           Function boxed3Ints plusVal        boxed3IntsAssumptions)
                                                        , ("*",           Function boxed3Ints timeVal        boxed3IntsAssumptions)
                                                        , ("-",           Function boxed3Ints minusVal       boxed3IntsAssumptions)
                                                        , ("div",         Function boxed3Ints divVal         boxed3IntsAssumptions)
                                                        , ("%",           Function boxed3Ints modVal         boxed3IntsAssumptions)
                                                        , ("pred",        Function boxed2Ints predVal        boxed2IntsAssumptions)
                                                        , ("succ",        Function boxed2Ints succVal        boxed2IntsAssumptions)
                                                        , ("seconds",     Function boxed2Ints secondsVal     boxed2IntsAssumptions)
                                                        , ("miliseconds", Function boxed2Ints milisecondsVal boxed2IntsAssumptions)
                                                        , ("shortRep",    Function int2string showVal        int2StringAssumptions)
                                                        , ("toJSON",      Function int2JSON   toJSVal        int2JSONAssumptions)
                                                        , ("toReal",      Function int2Real   toRealVal      int2RealAssumptions)
                                                        , ("equals",      Function ints2Bool  eqVal          ints2BoolAssumptions)
                                                        , (">",           Function ints2Bool  gtVal          ints2BoolAssumptions)
                                                        , ("<",           Function ints2Bool  ltVal          ints2BoolAssumptions)
                                                        ]
    return klass

binaryClass :: Imports -> IO Class
binaryClass imps = do
    Right (toTextAssu, toTextIr) <- runGraph $ do
        tText   <- cons_ @Draft "Binary"
        tJSON   <- cons_ @Draft "Text"
        l       <- lam tText tJSON
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)
    let toTextVal = toLunaValue tmpImps Text.decodeUtf8
        tmpImps       = imps & importedClasses . at "Binary" ?~ klass
        klass         = Class Map.empty $ Map.fromList [ ("toText", Function toTextIr toTextVal toTextAssu)
                                                       ]
    return klass

stringClass :: Imports -> IO Class
stringClass imps = do
    Right (plusAssu, plusIr) <- runGraph $ do
        tText <- cons_ @Draft "Text"
        l1      <- lam tText tText
        l2      <- lam tText l1
        (assu, r) <- mkMonadProofFun $ generalize l2
        cmp <- compile r
        return (assu, cmp)
    Right (eqAssu, eqIr) <- runGraph $ do
        tText <- cons_ @Draft "Text"
        tBool   <- cons_ @Draft "Bool"
        l1      <- lam tText tBool
        l2      <- lam tText l1
        (assu, r) <- mkMonadProofFun $ generalize l2
        cmp <- compile r
        return (assu, cmp)
    Right (shortRepAssu, shortRepIr) <- runGraph $ do
        tText <- cons_ @Draft "Text"
        l       <- lam tText tText
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)
    Right (toJSONAssu, toJSONIr) <- runGraph $ do
        tText   <- cons_ @Draft "Text"
        tJSON   <- cons_ @Draft "JSON"
        l       <- lam tText tJSON
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)
    Right (toBinaryAssu, toBinaryIr) <- runGraph $ do
        tText   <- cons_ @Draft "Text"
        tBinary   <- cons_ @Draft "Binary"
        l       <- lam tText tBinary
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)
    Right (wordsAssu, wordsIr) <- runGraph $ do
        tText <- cons_ @Draft "Text"
        tList   <- cons "List" [tText]
        l       <- lam tText tList
        (assu, r) <- mkMonadProofFun $ generalize l
        cmp <- compile r
        return (assu, cmp)
    let plusVal       = toLunaValue tmpImps ((<>) :: Text -> Text -> Text)
        shortRepVal   = toLunaValue tmpImps (id   :: Text -> Text)
        eqVal         = toLunaValue tmpImps ((==) :: Text -> Text -> Bool)
        gtVal         = toLunaValue tmpImps ((>)  :: Text -> Text -> Bool)
        ltVal         = toLunaValue tmpImps ((<)  :: Text -> Text -> Bool)
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
                                                       , ("words",      Function wordsIr    wordsVal      wordsAssu)
                                                       , ("lines",      Function wordsIr    linesVal      wordsAssu)
                                                       , ("characters", Function wordsIr    charsVal      wordsAssu)
                                                       , ("lowercase",  Function shortRepIr lowercaseVal  shortRepAssu)
                                                       , ("uppercase",  Function shortRepIr uppercaseVal  shortRepAssu)
                                                       , ("reverse",    Function shortRepIr reverseVal    shortRepAssu)
                                                       , ("shortRep",   Function shortRepIr shortRepVal   shortRepAssu)
                                                       , ("escapeJSON", Function shortRepIr escapeJSONVal shortRepAssu)
                                                       , ("toJSON",     Function toJSONIr   toJSONVal     toJSONAssu)
                                                       , ("toBinary",   Function toBinaryIr toBinaryVal   toBinaryAssu)
                                                       ]
    return klass

prelude :: Imports -> IO Imports
prelude imps = mdo
    minus <- compileFunction def $ do
        a    <- var "a"
        b    <- var "b"
        acpl <- acc a "-"
        apb  <- app acpl b
        l1   <- lam b apb
        l2   <- lam a l1
        tpA  <- var "a"
        monA <- var "monA"
        monB <- var "monB"
        monC <- var "monC"
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
    times <- compileFunction def $ do
        a    <- var "a"
        b    <- var "b"
        acpl <- acc a "*"
        apb  <- app acpl b
        l1   <- lam b apb
        l2   <- lam a l1
        tpA  <- var "a"
        monA <- var "monA"
        monB <- var "monB"
        monC <- var "monC"
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
    mod <- compileFunction def $ do
        a    <- var "a"
        b    <- var "b"
        acpl <- acc a "%"
        apb  <- app acpl b
        l1   <- lam b apb
        l2   <- lam a l1
        tpA  <- var "a"
        monA <- var "monA"
        monB <- var "monB"
        monC <- var "monC"
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
    plus <- compileFunction def $ do
        a    <- var "a"
        b    <- var "b"
        acpl <- acc a "+"
        apb  <- app acpl b
        l1   <- lam b apb
        l2   <- lam a l1
        tpA  <- var "a"
        monA <- var "monA"
        monB <- var "monB"
        monC <- var "monC"
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
    gt <- compileFunction importBoxes $ do
        a    <- var "a"
        b    <- var "b"
        acpl <- acc a ">"
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
    lt <- compileFunction importBoxes $ do
        a    <- var "a"
        b    <- var "b"
        acpl <- acc a "<"
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
    let funMap = Map.fromList [("+", plus), ("-", minus), ("*", times), (">", gt), ("<", lt), ("%", mod)]
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

withExceptions :: IO a -> LunaEff a
withExceptions a = do
    res <- performIO $ catchAll (Right <$> a) (return . Left . show)
    case res of
        Left a  -> throw a
        Right r -> return r

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

    let primPerformHttpVal :: Text -> Text -> ByteString -> LunaEff (HTTP.Response ByteString)
        primPerformHttpVal uri method body = performIO $ do
            baseReq <- HTTP.parseRequest (convert uri)
            HTTP.httpLBS $ baseReq & HTTP.setRequestBodyLBS body
                                   & HTTP.setRequestMethod (convert method)
    primPerformHttp <- typeRepForIO (toLunaValue std primPerformHttpVal) [LCons "Text" [], LCons "Text" [], LCons "Binary" []] $ LCons "HttpResponse" []

    let sleepVal = performIO . threadDelay
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

    let systemModule = Map.fromList [ ("putStr", printLn)
                                    , ("errorStr", err)
                                    , ("readFile", readFileF)
                                    , ("writeFile", writeFile')
                                    , ("parseJSON", parseJSON)
                                    , ("primPerformHttp", primPerformHttp)
                                    , ("primFork", fork)
                                    , ("sleep", sleep)
                                    , ("primNewMVar", newEmptyMVar')
                                    , ("primPutMVar", putMVar')
                                    , ("primTakeMVar", takeMVar')
                                    , ("primReadMVar", takeMVar' & Function.value .~ toLunaValue std readMVarVal)
                                    ]

    return $ (cleanup, std & importedFunctions %~ Map.union systemModule)

instance ToLunaData (HTTP.Response ByteString) where
    toLunaData imps v = LunaObject $ Object (Constructor "HttpResponse" [toLunaData imps $ HTTP.getResponseStatusCode v, toLunaData imps $ HTTP.getResponseBody v]) $ getObjectMethodMap "HttpResponse" imps

instance (ToLunaData a, ToLunaData b) => ToLunaData (IMap.Map a b) where
    toLunaData imps v = LunaObject $ Object (constructorOf v) $ getObjectMethodMap "Map" imps where
        constructorOf IMap.Tip             = Constructor "Tip" []
        constructorOf (IMap.Bin s k v l r) = Constructor "Bin" [toLunaData imps s, toLunaData imps k, toLunaData imps v, toLunaData imps l, toLunaData imps r]

instance ToLunaData Aeson.Value where
    toLunaData imps v = LunaObject $ Object (constructorOf v) $ getObjectMethodMap "JSON" imps where
        constructorOf (Aeson.Array  a) = Constructor "JSONArray"  [toLunaData imps . toList $ a]
        constructorOf (Aeson.String a) = Constructor "JSONString" [toLunaData imps (convert a :: Text)]
        constructorOf (Aeson.Number a) = Constructor "JSONNumber" [toLunaData imps (toRealFloat a :: Double)]
        constructorOf (Aeson.Bool   a) = Constructor "JSONBool"   [toLunaData imps a]
        constructorOf  Aeson.Null      = Constructor "JSONNull"   []
        constructorOf (Aeson.Object a) = Constructor "JSONObject" [toLunaData imps $ (Map.mapKeys convert $ Map.fromList $ HM.toList a :: Map Text Aeson.Value)]
