{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.Base where

import           Prologue
import qualified Luna.IR as IR

import Control.Lens (_Left)
import           Control.Concurrent          (MVar, newEmptyMVar, takeMVar, readMVar, putMVar, forkFinally, killThread, threadDelay)
import qualified Control.Concurrent.Async    as Async
import qualified Control.Exception           as Exception (evaluate)
import qualified Control.Exception.Safe      as Exception
import qualified Data.Aeson                  as Aeson
import qualified Data.Bifunctor              as Bifunc
import qualified Data.Bits                   as Bits
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as ByteString
import           Data.Foldable               as Foldable (toList)
import qualified Data.HashMap.Lazy           as HM
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import qualified Data.Map.Internal           as IMap
import           Data.Scientific             (toRealFloat, Scientific, coefficient, base10Exponent, fromFloatDigits)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text

import qualified Luna.Runtime as Luna
import qualified Luna.Pass.Sourcing.Data.Def as Def

import           Luna.Std.Builder            (LTp (..), makeFunctionPure, makeFunctionIO, maybeLT, listLT, eitherLT, integer, int)
import           Luna.Std.Finalizers         (FinalizersCtx, registerFinalizer, cancelFinalizer)
import           Text.Read                   (readMaybe)

import           System.Random               (randomIO)
import           System.Directory            (canonicalizePath)
import           System.FilePath             (pathSeparator)

primReal :: IO (Map IR.Name Def.Def)
primReal = do
    let plusVal     = flip Luna.toValue ((+)          :: Double -> Double -> Double)
        timesVal    = flip Luna.toValue ((*)          :: Double -> Double -> Double)
        minusVal    = flip Luna.toValue ((-)          :: Double -> Double -> Double)
        divVal      = flip Luna.toValue ((/)          :: Double -> Double -> Double)
        logBaseVal  = flip Luna.toValue (flip logBase :: Double -> Double -> Double)
        powVal      = flip Luna.toValue ((**)         :: Double -> Double -> Double)
        eqVal       = flip Luna.toValue ((==)         :: Double -> Double -> Bool)
        ltVal       = flip Luna.toValue ((<)          :: Double -> Double -> Bool)
        gtVal       = flip Luna.toValue ((>)          :: Double -> Double -> Bool)
        roundVal    = flip Luna.toValue ((\r prec -> (fromIntegral $ round (r * (10 ^ prec))) / (10 ^ prec :: Double)) :: Double -> Integer -> Double)
        showVal     = flip Luna.toValue (convert . show :: Double -> Text)
        floorVal    = flip Luna.toValue (floor   :: Double -> Integer)
        ceilingVal  = flip Luna.toValue (ceiling :: Double -> Integer)

        sinVal      = flip Luna.toValue (sin  :: Double -> Double)
        cosVal      = flip Luna.toValue (cos  :: Double -> Double)
        tanVal      = flip Luna.toValue (tan  :: Double -> Double)

        asinVal     = flip Luna.toValue (asin :: Double -> Double)
        acosVal     = flip Luna.toValue (acos :: Double -> Double)
        atanVal     = flip Luna.toValue (atan :: Double -> Double)

        sinhVal     = flip Luna.toValue (sinh  :: Double -> Double)
        coshVal     = flip Luna.toValue (cosh  :: Double -> Double)
        tanhVal     = flip Luna.toValue (tanh  :: Double -> Double)

        asinhVal    = flip Luna.toValue (asinh :: Double -> Double)
        acoshVal    = flip Luna.toValue (acosh :: Double -> Double)
        atanhVal    = flip Luna.toValue (atanh :: Double -> Double)

        expVal      = flip Luna.toValue (exp  :: Double -> Double)
        sqrtVal     = flip Luna.toValue (sqrt :: Double -> Double)
        logVal      = flip Luna.toValue (log  :: Double -> Double)
        uminusVal   = flip Luna.toValue ((* (-1)) :: Double -> Double)
        toScientificVal = flip Luna.toValue (fromFloatDigits :: Double -> Scientific)

    plus  <- makeFunctionPure plusVal ["Real", "Real"] "Real"
    times <- makeFunctionPure timesVal ["Real", "Real"] "Real"
    minus <- makeFunctionPure minusVal ["Real", "Real"] "Real"
    div <- makeFunctionPure divVal ["Real", "Real"] "Real"
    logBase <- makeFunctionPure logBaseVal ["Real", "Real"] "Real"
    pow <- makeFunctionPure powVal ["Real", "Real"] "Real"

    eq <- makeFunctionPure eqVal ["Real", "Real"] "Bool"
    lt <- makeFunctionPure ltVal ["Real", "Real"] "Bool"
    gt <- makeFunctionPure gtVal ["Real", "Real"] "Bool"

    round <- makeFunctionPure roundVal ["Real", "Int"] "Real"
    show  <- makeFunctionPure showVal  ["Real"] "Text"

    floor <- makeFunctionPure floorVal ["Real"] "Int"
    ceiling <- makeFunctionPure ceilingVal ["Real"] "Int"

    sin <- makeFunctionPure sinVal ["Real"] "Real"
    cos <- makeFunctionPure cosVal ["Real"] "Real"
    tan <- makeFunctionPure tanVal ["Real"] "Real"

    asin <- makeFunctionPure asinVal ["Real"] "Real"
    acos <- makeFunctionPure acosVal ["Real"] "Real"
    atan <- makeFunctionPure atanVal ["Real"] "Real"

    sinh <- makeFunctionPure sinhVal ["Real"] "Real"
    cosh <- makeFunctionPure coshVal ["Real"] "Real"
    tanh <- makeFunctionPure tanhVal ["Real"] "Real"

    asinh <- makeFunctionPure asinhVal ["Real"] "Real"
    acosh <- makeFunctionPure acoshVal ["Real"] "Real"
    atanh <- makeFunctionPure atanhVal ["Real"] "Real"

    exp <- makeFunctionPure expVal ["Real"] "Real"
    sqrt <- makeFunctionPure sqrtVal ["Real"] "Real"
    log <- makeFunctionPure logVal ["Real"] "Real"
    uminus <- makeFunctionPure uminusVal ["Real"] "Real"

    toScientific <- makeFunctionPure toScientificVal ["Real"] "Scientific"

    return $ Map.fromList [ ("primRealAdd",      plus)
                          , ("primRealMultiply", times)
                          , ("primRealSubtract", minus)
                          , ("primRealDivide",   div)
                          , ("primRealPow",      pow)
                          , ("primRealLog",      logBase)

                          , ("primRealFloor",    floor)
                          , ("primRealCeiling",  ceiling)

                          , ("primRealEquals",   eq)
                          , ("primRealLt",       lt)
                          , ("primRealGt",       gt)

                          , ("primRealRound",    round)
                          , ("primRealToText",   show)

                          , ("primRealSin",      sin)
                          , ("primRealCos",      cos)
                          , ("primRealTan",      tan)

                          , ("primRealAsin",     asin)
                          , ("primRealAcos",     acos)
                          , ("primRealAtan",     atan)

                          , ("primRealSinh",     sinh)
                          , ("primRealCosh",     cosh)
                          , ("primRealTanh",     tanh)

                          , ("primRealAsinh",    asinh)
                          , ("primRealAcosh",    acosh)
                          , ("primRealAtanh",    atanh)

                          , ("primRealExp",      exp)
                          , ("primRealLn",       log)
                          , ("primRealSqrt",     sqrt)
                          , ("primRealNegate",   uminus)

                          , ("primRealToScientific", toScientific)
                          ]

primInt :: IO (Map IR.Name Def.Def)
primInt = do
    let plusVal        = flip Luna.toValue ((+)      :: Integer -> Integer -> Integer)
        timesVal        = flip Luna.toValue ((*)      :: Integer -> Integer -> Integer)
        minusVal       = flip Luna.toValue ((-)      :: Integer -> Integer -> Integer)
        divVal         = flip Luna.toValue (div      :: Integer -> Integer -> Integer)
        eqVal          = flip Luna.toValue ((==)     :: Integer -> Integer -> Bool)
        gtVal          = flip Luna.toValue ((>)      :: Integer -> Integer -> Bool)
        ltVal          = flip Luna.toValue ((<)      :: Integer -> Integer -> Bool)
        modVal         = flip Luna.toValue (mod      :: Integer -> Integer -> Integer)
        predVal        = flip Luna.toValue (pred     :: Integer -> Integer)
        negateVal      = flip Luna.toValue ((* (-1)) :: Integer -> Integer)
        succVal        = flip Luna.toValue (succ     :: Integer -> Integer)
        showVal        = flip Luna.toValue (convert . show :: Integer -> Text)
        toRealVal      = flip Luna.toValue (fromIntegral   :: Integer -> Double)
        shiftFun :: Integer -> Integer -> Integer
        shiftFun x i = Bits.shift (fromIntegral x) (fromIntegral i)
        shiftVal       = flip Luna.toValue shiftFun

    plus <- makeFunctionPure plusVal ["Int", "Int"] "Int"
    times <- makeFunctionPure timesVal ["Int", "Int"] "Int"
    minus <- makeFunctionPure minusVal ["Int", "Int"] "Int"
    div <- makeFunctionPure divVal ["Int", "Int"] "Int"
    mod <- makeFunctionPure modVal ["Int", "Int"] "Int"

    eq <- makeFunctionPure eqVal ["Int", "Int"] "Bool"
    lt <- makeFunctionPure ltVal ["Int", "Int"] "Bool"
    gt <- makeFunctionPure gtVal ["Int", "Int"] "Bool"

    pred <- makeFunctionPure predVal ["Int"] "Int"
    succ <- makeFunctionPure succVal ["Int"] "Int"
    negate <- makeFunctionPure negateVal ["Int"] "Int"

    show <- makeFunctionPure showVal ["Int"] "Text"
    toReal <- makeFunctionPure toRealVal ["Int"] "Real"

    shift <- makeFunctionPure shiftVal ["Int", "Int"] "Int"


    return $ Map.fromList [ ("primIntAdd",         plus)
                          , ("primIntMultiply",    times)
                          , ("primIntSubtract",    minus)
                          , ("primIntDivide",      div)
                          , ("primIntModulo",      mod)
                          , ("primIntPred",        pred)
                          , ("primIntSucc",        succ)
                          , ("primIntNegate",      negate)
                          , ("primIntToText",      show)
                          , ("primIntToReal",      toReal)
                          , ("primIntEquals",      eq)
                          , ("primIntGt",          gt)
                          , ("primIntLt",          lt)
                          , ("primIntShift",       shift)
                          ]

primBinary :: IO (Map IR.Name Def.Def)
primBinary = do
    let toTextVal  = flip Luna.toValue Text.decodeUtf8
        eqVal      = flip Luna.toValue ((==) :: ByteString -> ByteString -> Bool)
        plusVal    = flip Luna.toValue ((<>) :: ByteString -> ByteString -> ByteString)
        lenVal     = flip Luna.toValue (fromIntegral . ByteString.length :: ByteString -> Integer)
        takeVal    = flip Luna.toValue (flip (ByteString.take . fromIntegral) :: ByteString -> Integer -> ByteString)
        dropVal    = flip Luna.toValue (flip (ByteString.drop . fromIntegral) :: ByteString -> Integer -> ByteString)

    toText <- makeFunctionPure toTextVal ["Binary"] "Text"
    eq <- makeFunctionPure eqVal ["Binary", "Binary"] "Binary"
    plus <- makeFunctionPure plusVal ["Binary", "Binary"] "Binary"
    len <- makeFunctionPure lenVal ["Binary"] "Int"
    take <- makeFunctionPure takeVal ["Binary"] "Binary"
    drop <- makeFunctionPure dropVal ["Binary"] "Binary"
    return $ Map.fromList [ ("primBinaryToText",   toText)
                          , ("primBinaryEquals",   eq)
                          , ("primBinaryConcat",   plus)
                          , ("primBinaryLength",   len)
                          , ("primBinaryTake",     take)
                          , ("primBinaryDrop",     drop)
                          ]

primText :: IO (Map IR.Name Def.Def)
primText = do
    let plusVal       = flip Luna.toValue ((<>) :: Text -> Text -> Text)
        shortRepVal   = flip Luna.toValue (Text.take 100 :: Text -> Text)
        eqVal         = flip Luna.toValue ((==) :: Text -> Text -> Bool)
        gtVal         = flip Luna.toValue ((>)  :: Text -> Text -> Bool)
        ltVal         = flip Luna.toValue ((<)  :: Text -> Text -> Bool)
        isEmptyVal    = flip Luna.toValue (Text.null :: Text -> Bool)
        lengthVal     = flip Luna.toValue (fromIntegral . Text.length :: Text -> Integer)
        hasPrefixVal  = flip Luna.toValue (flip Text.isPrefixOf)
        wordsVal      = flip Luna.toValue Text.words
        linesVal      = flip Luna.toValue Text.lines
        lowercaseVal  = flip Luna.toValue Text.toLower
        uppercaseVal  = flip Luna.toValue Text.toUpper
        reverseVal    = flip Luna.toValue Text.reverse
        charsVal      = flip Luna.toValue (Text.chunksOf 1)
        toBinaryVal   = flip Luna.toValue (Text.encodeUtf8 :: Text -> ByteString)
        escapeJSONVal = flip Luna.toValue (Text.decodeUtf8 . convert . Aeson.encode :: Text -> Text)
        toIntVal      = flip Luna.toValue (readMaybe . convert :: Text -> Maybe Integer)
        toRealVal     = flip Luna.toValue (readMaybe . convert :: Text -> Maybe Double)

    plus <- makeFunctionPure plusVal ["Text", "Text"] "Text"
    shortRep <- makeFunctionPure shortRepVal ["Text"] "Text"
    eq <- makeFunctionPure eqVal ["Text", "Text"] "Bool"
    gt <- makeFunctionPure gtVal ["Text", "Text"] "Bool"
    lt <- makeFunctionPure ltVal ["Text", "Text"] "Bool"
    isEmpty <- makeFunctionPure isEmptyVal ["Text"] "Bool"
    length <- makeFunctionPure lengthVal ["Text"] "Int"
    hasPrefix <- makeFunctionPure hasPrefixVal ["Text", "Text"] "Bool"
    words <- makeFunctionPure wordsVal ["Text"] (listLT "Text")
    lines <- makeFunctionPure linesVal ["Text"] (listLT "Text")
    chars <- makeFunctionPure charsVal ["Text"] (listLT "Text")
    lowercase <- makeFunctionPure lowercaseVal ["Text"] "Text"
    uppercase <- makeFunctionPure uppercaseVal ["Text"] "Text"
    reverse <- makeFunctionPure reverseVal ["Text"] "Text"
    toBinary <- makeFunctionPure toBinaryVal ["Text"] "Binary"
    escapeJSON <- makeFunctionPure escapeJSONVal ["Text"] "Text"
    toInt <- makeFunctionPure toIntVal ["Text"] (maybeLT "Int")
    toReal <- makeFunctionPure toRealVal ["Text"] (maybeLT "Real")


    return $ Map.fromList [ ("primTextConcat",     plus)
                          , ("primTextEquals",     eq)
                          , ("primTextIsEmpty",    isEmpty)
                          , ("primTextLength",     length)
                          , ("primTextGt",         gt)
                          , ("primTextLt",         lt)
                          , ("primTextHasPrefix",  hasPrefix)
                          , ("primTextWords",      words)
                          , ("primTextLines",      lines)
                          , ("primTextCharacters", chars)
                          , ("primTextLowercase",  lowercase)
                          , ("primTextUppercase",  uppercase)
                          , ("primTextReverse",    reverse)
                          , ("primTextShortRep",   shortRep)
                          , ("primTextEscapeJSON", escapeJSON)
                          , ("primTextToBinary",   toBinary)
                          , ("primTextToInt",      toInt)
                          , ("primTextToReal",     toReal)
                          ]

operators :: IO (Map IR.Name Def.Def)
operators = do
    let iffVal = (\a b c -> if a then b else c) :: Bool -> Luna.Value -> Luna.Value -> Luna.Value
    iff <- makeFunctionPure (flip Luna.toValue iffVal) ["Bool", "a", "a"] "a"
    return $ Map.fromList [ ("if.then.else", iff) ]

io :: FinalizersCtx -> IO (Map IR.Name Def.Def)
io finalizersCtx = do

    let noneT   = "None"
        realT   = "Real"
        textT   = "Text"
        intT    = "Int"
        binaryT = "Binary"

    let putStr :: Text -> IO ()
        putStr = putStrLn . convert
    printLn <- makeFunctionIO (flip Luna.toValue putStr) ["Text"] "None"

    let runErrVal = flip Luna.toValue $ \(x :: Luna.Value) -> ((_Left %~ unwrap) <$> Luna.runError x :: Luna.Eff (Either Text Luna.Data))
    runErr <- makeFunctionPure runErrVal ["a"] (eitherLT "Text" "a")

    let errVal = flip Luna.toValue $ \(a :: Text) -> (Luna.throw a :: Luna.Value)
    err <- makeFunctionPure errVal ["Text"] "a"

    let expandPathVal :: Text -> IO Text
        expandPathVal = fmap convert . canonicalizePath . convert
    expandPathF <- makeFunctionIO (flip Luna.toValue expandPathVal) [textT] textT

    let readFileVal :: Text -> IO Text
        readFileVal = fmap convert . readFile . convert
    readFileF <- makeFunctionIO (flip Luna.toValue readFileVal) [textT] textT

    let readBinaryVal :: Text -> IO ByteString
        readBinaryVal = ByteString.readFile . convert
    readBinaryF <- makeFunctionIO (flip Luna.toValue readBinaryVal) [textT] binaryT

    let parseJSONVal :: Text -> Either Text Aeson.Value
        parseJSONVal = Bifunc.first convert . Aeson.eitherDecode . convert . Text.encodeUtf8
    parseJSON <- makeFunctionPure (flip Luna.toValue parseJSONVal) ["Text"] (eitherLT "Text" "JSON")

    let randomRealVal = randomIO :: IO Double
    randomReal <- makeFunctionIO (flip Luna.toValue randomRealVal) [] realT

    let sleepVal = threadDelay . int
    sleep <- makeFunctionIO (flip Luna.toValue sleepVal) [intT] noneT

    let forkVal :: Luna.Eff () -> IO ()
        forkVal act = mdo
            uid <- registerFinalizer finalizersCtx $ Async.uninterruptibleCancel a
            a   <- Async.async $ void (Luna.runIO $ Luna.runError act) `Exception.finally` cancelFinalizer finalizersCtx uid
            return ()
    fork <- makeFunctionIO (flip Luna.toValue forkVal) [noneT] noneT

    let newEmptyMVarVal :: IO (MVar Luna.Data)
        newEmptyMVarVal = newEmptyMVar
    newEmptyMVar' <- makeFunctionIO (flip Luna.toValue newEmptyMVarVal) [] (LCons "MVar" [LVar "a"])

    let putMVarVal :: MVar Luna.Data -> Luna.Data -> IO ()
        putMVarVal = putMVar
    putMVar' <- makeFunctionIO (flip Luna.toValue putMVarVal) [LCons "MVar" [LVar "a"], LVar "a"] noneT

    let takeMVarVal :: MVar Luna.Data -> IO Luna.Data
        takeMVarVal = takeMVar
        readMVarVal :: MVar Luna.Data -> IO Luna.Data
        readMVarVal = readMVar
    takeMVar' <- makeFunctionIO (flip Luna.toValue takeMVarVal) [LCons "MVar" [LVar "a"]] (LVar "a")


    let writeFileVal :: Text -> Text -> IO ()
        writeFileVal p c = writeFile (convert p) (convert c)
    writeFile' <- makeFunctionIO (flip Luna.toValue writeFileVal) [textT, textT] noneT

    let evaluateVal :: Text -> IO Text
        evaluateVal t = do
            Exception.evaluate $ rnf t
            return t
    evaluate' <- makeFunctionIO (flip Luna.toValue evaluateVal) [textT] textT


    let pathSepVal :: Text
        pathSepVal = convert pathSeparator
    pathSep <- makeFunctionPure (flip Luna.toValue pathSepVal) [] textT

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
                          , ("primReadMVar", takeMVar' & Def._Precompiled . Def.value .~ flip Luna.toValue readMVarVal)
                          , ("primEvaluate", evaluate')
                          ]

exports :: FinalizersCtx -> IO (Map IR.Name Def.Def)
exports finalizersCtx = do
    realFuns <- primReal
    intFuns  <- primInt
    textFuns <- primText
    binFuns  <- primBinary
    opFuns   <- operators
    ioFuns   <- io finalizersCtx
    return $ Map.unions [realFuns, intFuns, textFuns, binFuns, opFuns, ioFuns]

type instance Luna.RuntimeRepOf (IMap.Map a b) = Luna.AsClass (IMap.Map a b) ('Luna.ClassRep "Std.Base" "Map")
instance (Luna.ToData a, Luna.ToData b) => Luna.ToObject (IMap.Map a b) where
    toConstructor imps IMap.Tip = Luna.Constructor "Tip" []
    toConstructor imps (IMap.Bin s k v l r) = Luna.Constructor "Bin" [Luna.toData imps $ integer s, Luna.toData imps k, Luna.toData imps v, Luna.toData imps l, Luna.toData imps r]

type instance Luna.RuntimeRepOf Scientific = Luna.AsClass Scientific ('Luna.ClassRep "Std.Base" "Scientific")
instance Luna.ToObject Scientific where
    toConstructor imps s = Luna.Constructor "Scientific" [Luna.toData imps $ coefficient s, Luna.toData imps $ integer $ base10Exponent s]

type instance Luna.RuntimeRepOf Aeson.Value = Luna.AsClass Aeson.Value ('Luna.ClassRep "Std.Base" "JSON")
instance Luna.ToObject Aeson.Value where
    toConstructor imps (Aeson.Array  a) = Luna.Constructor "JSONArray"  [Luna.toData imps . Foldable.toList $ a]
    toConstructor imps (Aeson.String a) = Luna.Constructor "JSONString" [Luna.toData imps a]
    toConstructor imps (Aeson.Number a) = Luna.Constructor "JSONNumber" [Luna.toData imps a]
    toConstructor imps (Aeson.Bool   a) = Luna.Constructor "JSONBool"   [Luna.toData imps a]
    toConstructor imps  Aeson.Null      = Luna.Constructor "JSONNull"   []
    toConstructor imps (Aeson.Object a) = Luna.Constructor "JSONObject" [Luna.toData imps $ (Map.fromList $ HM.toList a :: Map Text Aeson.Value)]

