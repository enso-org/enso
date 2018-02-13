{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.Base where

import           Luna.Prelude                hiding (Text, force, toList)
import           Luna.IR

import           Control.Concurrent          (MVar, newEmptyMVar, takeMVar, readMVar, putMVar, forkFinally, killThread, threadDelay)
import qualified Control.Exception           as Exception
import qualified Data.Aeson                  as Aeson
import qualified Data.Bifunctor              as Bifunc
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as ByteString
import           Data.Foldable               (toList)
import qualified Data.HashMap.Lazy           as HM
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import qualified Data.Map.Base               as IMap
import           Data.Scientific             (toRealFloat)
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as Text
import qualified Data.Text.Lazy.Encoding     as Text

import           Luna.Builtin.Prim           (toLunaValue, ToLunaData, toLunaData, ToLunaObject, toConstructor, RuntimeRepOf, RuntimeRep (..))
import           Luna.Builtin.Data.Function  (Function (Function), value)
import           Luna.Builtin.Data.Module    (Imports, getObjectMethodMap)
import           Luna.Builtin.Data.LunaValue (LunaValue, LunaData (..), Constructor (..), Object (..), force)
import           Luna.Builtin.Data.LunaEff   (LunaEff, runIO, runError, throw)

import           Luna.Std.Builder            (LTp (..), makeTypePure, makeFunctionPure, makeFunctionIO, maybeLT, listLT, eitherLT, preludeCmpOp, preludeArithOp, preludeUnaryOp, integer, int)
import           Luna.Std.Finalizers         (FinalizersCtx, registerFinalizer, cancelFinalizer)
import           Text.Read                   (readMaybe)

import           System.Random               (randomIO)
import           System.Directory            (canonicalizePath)
import           System.FilePath             (pathSeparator)

primReal :: Imports -> IO (Map Name Function)
primReal imps = do
    (doubleIntDoubleAssumptions, doubleIntDouble) <- makeTypePure ["Real", "Int"]  "Real"
    (boxed3DoublesAssumptions,   boxed3Doubles)   <- makeTypePure ["Real", "Real"] "Real"
    (double2BoolAssumptions,     double2Bool)     <- makeTypePure ["Real", "Real"] "Bool"
    (double2TextAssumptions,     double2text)     <- makeTypePure ["Real"]         "Text"
    (double2IntAssumptions,      double2int)      <- makeTypePure ["Real"]         "Int"
    (double2JSONAssumptions,     double2JSON)     <- makeTypePure ["Real"]         "JSON"
    (double2DoubleAssumptions,   double2Double)   <- makeTypePure ["Real"]         "Real"

    let plusVal     = toLunaValue imps ((+)          :: Double -> Double -> Double)
        timeVal     = toLunaValue imps ((*)          :: Double -> Double -> Double)
        minusVal    = toLunaValue imps ((-)          :: Double -> Double -> Double)
        divVal      = toLunaValue imps ((/)          :: Double -> Double -> Double)
        logBaseVal  = toLunaValue imps (flip logBase :: Double -> Double -> Double)
        powVal      = toLunaValue imps ((**)         :: Double -> Double -> Double)
        eqVal       = toLunaValue imps ((==)         :: Double -> Double -> Bool)
        ltVal       = toLunaValue imps ((<)          :: Double -> Double -> Bool)
        gtVal       = toLunaValue imps ((>)          :: Double -> Double -> Bool)
        roundVal    = toLunaValue imps ((\r prec -> (fromIntegral $ round (r * (10 ^ prec))) / (10 ^ prec :: Double)) :: Double -> Integer -> Double)
        showVal     = toLunaValue imps (convert . show :: Double -> Text)
        floorVal    = toLunaValue imps (floor   :: Double -> Integer)
        ceilingVal  = toLunaValue imps (ceiling :: Double -> Integer)

        sinVal      = toLunaValue imps (sin  :: Double -> Double)
        cosVal      = toLunaValue imps (cos  :: Double -> Double)
        tanVal      = toLunaValue imps (tan  :: Double -> Double)

        asinVal     = toLunaValue imps (asin :: Double -> Double)
        acosVal     = toLunaValue imps (acos :: Double -> Double)
        atanVal     = toLunaValue imps (atan :: Double -> Double)

        sinhVal     = toLunaValue imps (sinh  :: Double -> Double)
        coshVal     = toLunaValue imps (cosh  :: Double -> Double)
        tanhVal     = toLunaValue imps (tanh  :: Double -> Double)

        asinhVal    = toLunaValue imps (asinh :: Double -> Double)
        acoshVal    = toLunaValue imps (acosh :: Double -> Double)
        atanhVal    = toLunaValue imps (atanh :: Double -> Double)

        expVal      = toLunaValue imps (exp  :: Double -> Double)
        sqrtVal     = toLunaValue imps (sqrt :: Double -> Double)
        logVal      = toLunaValue imps (log  :: Double -> Double)
        uminusVal   = toLunaValue imps ((* (-1)) :: Double -> Double)
    return $ Map.fromList [ ("primRealAdd",      Function boxed3Doubles   plusVal    boxed3DoublesAssumptions  )
                          , ("primRealMultiply", Function boxed3Doubles   timeVal    boxed3DoublesAssumptions  )
                          , ("primRealSubtract", Function boxed3Doubles   minusVal   boxed3DoublesAssumptions  )
                          , ("primRealDivide",   Function boxed3Doubles   divVal     boxed3DoublesAssumptions  )
                          , ("primRealPow",      Function boxed3Doubles   powVal     boxed3DoublesAssumptions  )
                          , ("primRealLog",      Function boxed3Doubles   logBaseVal boxed3DoublesAssumptions  )

                          , ("primRealFloor",    Function double2int      floorVal   double2IntAssumptions     )
                          , ("primRealCeiling",  Function double2int      ceilingVal double2IntAssumptions     )

                          , ("primRealEquals",   Function double2Bool     eqVal      double2BoolAssumptions    )
                          , ("primRealLt",       Function double2Bool     ltVal      double2BoolAssumptions    )
                          , ("primRealGt",       Function double2Bool     gtVal      double2BoolAssumptions    )

                          , ("primRealRound",    Function doubleIntDouble roundVal   doubleIntDoubleAssumptions)
                          , ("primRealToText",   Function double2text     showVal    double2TextAssumptions    )

                          , ("primRealSin",      Function double2Double   sinVal     double2DoubleAssumptions  )
                          , ("primRealCos",      Function double2Double   cosVal     double2DoubleAssumptions  )
                          , ("primRealTan",      Function double2Double   tanVal     double2DoubleAssumptions  )

                          , ("primRealAsin",     Function double2Double   asinVal    double2DoubleAssumptions  )
                          , ("primRealAcos",     Function double2Double   acosVal    double2DoubleAssumptions  )
                          , ("primRealAtan",     Function double2Double   atanVal    double2DoubleAssumptions  )

                          , ("primRealSinh",     Function double2Double   sinhVal    double2DoubleAssumptions  )
                          , ("primRealCosh",     Function double2Double   coshVal    double2DoubleAssumptions  )
                          , ("primRealTanh",     Function double2Double   tanhVal    double2DoubleAssumptions  )

                          , ("primRealAsinh",    Function double2Double   asinhVal   double2DoubleAssumptions  )
                          , ("primRealAcosh",    Function double2Double   acoshVal   double2DoubleAssumptions  )
                          , ("primRealAtanh",    Function double2Double   atanhVal   double2DoubleAssumptions  )

                          , ("primRealExp",      Function double2Double   expVal     double2DoubleAssumptions  )
                          , ("primRealLn",       Function double2Double   logVal     double2DoubleAssumptions  )
                          , ("primRealSqrt",     Function double2Double   sqrtVal    double2DoubleAssumptions  )
                          , ("primRealNegate",   Function double2Double   uminusVal  double2DoubleAssumptions  )
                          ]

primInt :: Imports -> IO (Map Name Function)
primInt imps = do
    (ints2BoolAssumptions,  ints2Bool)  <- makeTypePure ["Int", "Int"] "Bool"
    (boxed3IntsAssumptions, boxed3Ints) <- makeTypePure ["Int", "Int"] "Int"
    (boxed2IntsAssumptions, boxed2Ints) <- makeTypePure ["Int"]        "Int"
    (int2TextAssumptions,   int2text)   <- makeTypePure ["Int"]        "Text"
    (int2RealAssumptions,   int2Real)   <- makeTypePure ["Int"]        "Real"
    (int2JSONAssumptions,   int2JSON)   <- makeTypePure ["Int"]        "JSON"
    (int2TIAssumptions,     int2TI)     <- makeTypePure ["Int"]        "TimeInterval"

    let plusVal        = toLunaValue imps ((+)      :: Integer -> Integer -> Integer)
        timeVal        = toLunaValue imps ((*)      :: Integer -> Integer -> Integer)
        minusVal       = toLunaValue imps ((-)      :: Integer -> Integer -> Integer)
        divVal         = toLunaValue imps (div      :: Integer -> Integer -> Integer)
        eqVal          = toLunaValue imps ((==)     :: Integer -> Integer -> Bool)
        gtVal          = toLunaValue imps ((>)      :: Integer -> Integer -> Bool)
        ltVal          = toLunaValue imps ((<)      :: Integer -> Integer -> Bool)
        modVal         = toLunaValue imps (mod      :: Integer -> Integer -> Integer)
        predVal        = toLunaValue imps (pred     :: Integer -> Integer)
        negateVal      = toLunaValue imps ((* (-1)) :: Integer -> Integer)
        succVal        = toLunaValue imps (succ     :: Integer -> Integer)
        showVal        = toLunaValue imps (convert . show :: Integer -> Text)
        toRealVal      = toLunaValue imps (fromIntegral   :: Integer -> Double)
    return $ Map.fromList [ ("primIntAdd",         Function boxed3Ints plusVal        boxed3IntsAssumptions)
                          , ("primIntMultiply",    Function boxed3Ints timeVal        boxed3IntsAssumptions)
                          , ("primIntSubtract",    Function boxed3Ints minusVal       boxed3IntsAssumptions)
                          , ("primIntDivide",      Function boxed3Ints divVal         boxed3IntsAssumptions)
                          , ("primIntModulo",      Function boxed3Ints modVal         boxed3IntsAssumptions)
                          , ("primIntPred",        Function boxed2Ints predVal        boxed2IntsAssumptions)
                          , ("primIntSucc",        Function boxed2Ints succVal        boxed2IntsAssumptions)
                          , ("primIntNegate",      Function boxed2Ints negateVal      boxed2IntsAssumptions)
                          , ("primIntToText",      Function int2text   showVal        int2TextAssumptions  )
                          , ("primIntToReal",      Function int2Real   toRealVal      int2RealAssumptions  )
                          , ("primIntEquals",      Function ints2Bool  eqVal          ints2BoolAssumptions )
                          , ("primIntGt",          Function ints2Bool  gtVal          ints2BoolAssumptions )
                          , ("primIntLt",          Function ints2Bool  ltVal          ints2BoolAssumptions )
                          ]

primBinary :: Imports -> IO (Map Name Function)
primBinary imps = do
    (eqAssu,     eqIr)     <- makeTypePure ["Binary", "Binary"] "Bool"
    (plusAssu,   plusIr)   <- makeTypePure ["Binary", "Binary"] "Binary"
    (toTextAssu, toTextIr) <- makeTypePure ["Binary"]           "Text"
    (lenAssu,    lenIr)    <- makeTypePure ["Binary"]           "Int"
    let toTextVal  = toLunaValue imps Text.decodeUtf8
        eqVal      = toLunaValue imps ((==) :: ByteString -> ByteString -> Bool)
        plusVal    = toLunaValue imps ((<>) :: ByteString -> ByteString -> ByteString)
        lenVal     = toLunaValue imps (fromIntegral . ByteString.length :: ByteString -> Integer)
    return $ Map.fromList [ ("primBinaryToText",   Function toTextIr toTextVal  toTextAssu)
                          , ("primBinaryEquals",   Function eqIr     eqVal      eqAssu    )
                          , ("primBinaryConcat",   Function plusIr   plusVal    plusAssu  )
                          , ("primBinaryLength",   Function lenIr    lenVal     lenAssu   )
                          ]

primText :: Imports -> IO (Map Name Function)
primText imps = do
    (plusAssu,        plusIr)        <- makeTypePure ["Text", "Text"] "Text"
    (eqAssu,          eqIr)          <- makeTypePure ["Text", "Text"] "Bool"
    (textAssu,        textIr)        <- makeTypePure ["Text"]         "Text"
    (isEmptyAssu,     isEmptyIr)     <- makeTypePure ["Text"]         "Bool"
    (lengthAssu,      lengthIr)      <- makeTypePure ["Text"]         "Int"
    (toJSONAssu,      toJSONIr)      <- makeTypePure ["Text"]         "JSON"
    (toBinaryAssu,    toBinaryIr)    <- makeTypePure ["Text"]         "Binary"
    (wordsAssu,       wordsIr)       <- makeTypePure ["Text"]         (listLT  "Text")
    (toMaybeIntAssu,  toMaybeIntIr)  <- makeTypePure ["Text"]         (maybeLT "Int")
    (toMaybeRealAssu, toMaybeRealIr) <- makeTypePure ["Text"]         (maybeLT "Real")

    let plusVal       = toLunaValue imps ((<>) :: Text -> Text -> Text)
        shortRepVal   = toLunaValue imps (Text.take 100 :: Text -> Text)
        idVal         = toLunaValue imps (id :: Text -> Text)
        eqVal         = toLunaValue imps ((==) :: Text -> Text -> Bool)
        gtVal         = toLunaValue imps ((>)  :: Text -> Text -> Bool)
        ltVal         = toLunaValue imps ((<)  :: Text -> Text -> Bool)
        isEmptyVal    = toLunaValue imps (Text.null :: Text -> Bool)
        lengthVal     = toLunaValue imps (fromIntegral . Text.length :: Text -> Integer)
        hasPrefixVal  = toLunaValue imps (flip Text.isPrefixOf)
        wordsVal      = toLunaValue imps Text.words
        linesVal      = toLunaValue imps Text.lines
        lowercaseVal  = toLunaValue imps Text.toLower
        uppercaseVal  = toLunaValue imps Text.toUpper
        reverseVal    = toLunaValue imps Text.reverse
        charsVal      = toLunaValue imps (Text.chunksOf 1)
        toBinaryVal   = toLunaValue imps (Text.encodeUtf8 :: Text -> ByteString)
        escapeJSONVal = toLunaValue imps (Text.decodeUtf8 . Aeson.encode :: Text -> Text)
        toIntVal      = toLunaValue imps (readMaybe . convert :: Text -> Maybe Integer)
        toRealVal     = toLunaValue imps (readMaybe . convert :: Text -> Maybe Double)
    return $ Map.fromList [ ("primTextConcat",     Function plusIr        plusVal       plusAssu)
                          , ("primTextEquals",     Function eqIr          eqVal         eqAssu)
                          , ("primTextIsEmpty",    Function isEmptyIr     isEmptyVal    isEmptyAssu)
                          , ("primTextLength",     Function lengthIr      lengthVal     lengthAssu)
                          , ("primTextGt",         Function eqIr          gtVal         eqAssu)
                          , ("primTextLt",         Function eqIr          ltVal         eqAssu)
                          , ("primTextHasPrefix",  Function eqIr          hasPrefixVal  eqAssu)
                          , ("primTextWords",      Function wordsIr       wordsVal      wordsAssu)
                          , ("primTextLines",      Function wordsIr       linesVal      wordsAssu)
                          , ("primTextCharacters", Function wordsIr       charsVal      wordsAssu)
                          , ("primTextLowercase",  Function textIr        lowercaseVal  textAssu)
                          , ("primTextUppercase",  Function textIr        uppercaseVal  textAssu)
                          , ("primTextReverse",    Function textIr        reverseVal    textAssu)
                          , ("primTextShortRep",   Function textIr        shortRepVal   textAssu)
                          , ("primTextEscapeJSON", Function textIr        escapeJSONVal textAssu)
                          , ("primTextToBinary",   Function toBinaryIr    toBinaryVal   toBinaryAssu)
                          , ("primTextToInt",      Function toMaybeIntIr  toIntVal      toMaybeIntAssu)
                          , ("primTextToReal",     Function toMaybeRealIr toRealVal     toMaybeRealAssu)
                          ]

operators :: Imports -> IO (Map Name Function)
operators imps = do
    minus    <- preludeArithOp "-"
    times    <- preludeArithOp "*"
    mod      <- preludeArithOp "%"
    plus     <- preludeArithOp "+"
    div      <- preludeArithOp "/"
    pow      <- preludeArithOp "^"
    uminus   <- preludeUnaryOp "negate"
    gt       <- preludeCmpOp imps ">"
    lt       <- preludeCmpOp imps "<"
    gte      <- preludeCmpOp imps ">="
    lte      <- preludeCmpOp imps "<="
    eq       <- preludeCmpOp imps "=="
    return $ Map.fromList [ ("+",        plus)
                          , ("-",        minus)
                          , ("*",        times)
                          , ("^",        pow)
                          , (">",        gt)
                          , ("<",        lt)
                          , (">=",       gte)
                          , ("<=",       lte)
                          , ("==",       eq)
                          , ("%",        mod)
                          , ("/",        div)
                          , ("#uminus#", uminus)
                          ]

io :: Imports -> FinalizersCtx -> IO (Map Name Function)
io std finalizersCtx = do

    let noneT   = "None"
        realT   = "Real"
        textT   = "Text"
        intT    = "Int"
        binaryT = "Binary"

    let putStr :: Text -> IO ()
        putStr = putStrLn . convert
    printLn <- makeFunctionIO (toLunaValue std putStr) ["Text"] "None"

    let runErrVal = toLunaValue std $ \(x :: LunaValue) -> ((_Left %~ convert) <$> runError (force x) :: LunaEff (Either Text LunaData))
    runErr <- makeFunctionPure runErrVal ["a"] (eitherLT "Text" "a")

    let errVal = toLunaValue std $ \(a :: Text) -> (throw $ convert a :: LunaValue)
    err <- makeFunctionPure errVal ["Text"] "a"

    let expandPathVal :: Text -> IO Text
        expandPathVal = fmap convert . canonicalizePath . convert
    expandPathF <- makeFunctionIO (toLunaValue std expandPathVal) [textT] textT

    let readFileVal :: Text -> IO Text
        readFileVal = fmap convert . readFile . convert
    readFileF <- makeFunctionIO (toLunaValue std readFileVal) [textT] textT

    let readBinaryVal :: Text -> IO ByteString
        readBinaryVal = ByteString.readFile . convert
    readBinaryF <- makeFunctionIO (toLunaValue std readBinaryVal) [textT] binaryT

    let parseJSONVal :: Text -> IO (Either Text Aeson.Value)
        parseJSONVal = return . Bifunc.first convert . Aeson.eitherDecode . Text.encodeUtf8
    parseJSON <- makeFunctionPure (toLunaValue std parseJSONVal) ["Text"] (eitherLT "Text" "JSON")

    let randomRealVal = randomIO :: IO Double
    randomReal <- makeFunctionIO (toLunaValue std randomRealVal) [] realT

    let sleepVal = threadDelay . int
    sleep <- makeFunctionIO (toLunaValue std sleepVal) [intT] noneT

    let forkVal :: LunaEff () -> IO ()
        forkVal act = mdo
            uid <- registerFinalizer finalizersCtx $ killThread tid
            tid <- forkFinally (void $ runIO $ runError act) $ const (cancelFinalizer finalizersCtx uid)
            return ()
    fork <- makeFunctionIO (toLunaValue std forkVal) [noneT] noneT

    let newEmptyMVarVal :: IO (MVar LunaData)
        newEmptyMVarVal = newEmptyMVar
    newEmptyMVar' <- makeFunctionIO (toLunaValue std newEmptyMVarVal) [] (LCons "MVar" [LVar "a"])

    let putMVarVal :: MVar LunaData -> LunaData -> IO ()
        putMVarVal = putMVar
    putMVar' <- makeFunctionIO (toLunaValue std putMVarVal) [LCons "MVar" [LVar "a"], LVar "a"] noneT

    let takeMVarVal :: MVar LunaData -> IO LunaData
        takeMVarVal = takeMVar
        readMVarVal :: MVar LunaData -> IO LunaData
        readMVarVal = readMVar
    takeMVar' <- makeFunctionIO (toLunaValue std takeMVarVal) [LCons "MVar" [LVar "a"]] (LVar "a")


    let writeFileVal :: Text -> Text -> IO ()
        writeFileVal p c = writeFile (convert p) (convert c)
    writeFile' <- makeFunctionIO (toLunaValue std writeFileVal) [textT, textT] noneT

    let evaluateVal :: Text -> IO Text
        evaluateVal t = do
            Exception.evaluate $ rnf t
            return t
    evaluate' <- makeFunctionIO (toLunaValue std evaluateVal) [textT] textT


    let pathSepVal :: Text
        pathSepVal = convert pathSeparator
    pathSep <- makeFunctionPure (toLunaValue std pathSepVal) [] textT

    return $ Map.fromList [ ("putStr", printLn)
                          , ("errorStr", err)
                          , ("runError", runErr)
                          , ("primReadFile", readFileF)
                          , ("expandPath", expandPathF)
                          , ("readBinary", readBinaryF)
                          , ("pathSeparator", pathSep)
                          , ("primWriteFile", writeFile')
                          , ("parseJSON", parseJSON)
                          , ("randomReal", randomReal)
                          , ("primFork", fork)
                          , ("sleep", sleep)
                          , ("primNewMVar", newEmptyMVar')
                          , ("primPutMVar", putMVar')
                          , ("primTakeMVar", takeMVar')
                          , ("primReadMVar", takeMVar' & value .~ toLunaValue std readMVarVal)
                          , ("primEvaluate", evaluate')
                          ]

exports :: Imports -> FinalizersCtx -> IO (Map Name Function)
exports imps finalizersCtx = do
    realFuns <- primReal     imps
    intFuns  <- primInt      imps
    textFuns <- primText     imps
    binFuns  <- primBinary   imps
    opFuns   <- operators    imps
    ioFuns   <- io           imps finalizersCtx
    return $ Map.unions [realFuns, intFuns, textFuns, binFuns, opFuns, ioFuns]

instance (ToLunaData a, ToLunaData b) => ToLunaData (IMap.Map a b) where
    toLunaData imps v = LunaObject $ Object (constructorOf v) $ getObjectMethodMap "Map" imps where
        constructorOf IMap.Tip             = Constructor "Tip" []
        constructorOf (IMap.Bin s k v l r) = Constructor "Bin" [toLunaData imps $ integer s, toLunaData imps k, toLunaData imps v, toLunaData imps l, toLunaData imps r]

type instance RuntimeRepOf Aeson.Value = AsClass "JSON" Aeson.Value
instance ToLunaObject Aeson.Value where
    toConstructor imps (Aeson.Array  a) = Constructor "JSONArray"  [toLunaData imps . toList $ a]
    toConstructor imps (Aeson.String a) = Constructor "JSONString" [toLunaData imps (convert a :: Text)]
    toConstructor imps (Aeson.Number a) = Constructor "JSONNumber" [toLunaData imps (toRealFloat a :: Double)]
    toConstructor imps (Aeson.Bool   a) = Constructor "JSONBool"   [toLunaData imps a]
    toConstructor imps  Aeson.Null      = Constructor "JSONNull"   []
    toConstructor imps (Aeson.Object a) = Constructor "JSONObject" [toLunaData imps $ (Map.mapKeys convert $ Map.fromList $ HM.toList a :: Map Text Aeson.Value)]

