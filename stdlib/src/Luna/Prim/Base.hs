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

import qualified Luna.Std.Builder            as Builder
import qualified Luna.Runtime as Luna
import qualified Luna.Pass.Sourcing.Data.Def as Def

import           Luna.Std.Builder            (makeFunctionPure, makeFunctionIO, maybeLT, listLT, eitherLT, integer, int)
import           Luna.Std.Finalizers         (FinalizersCtx, registerFinalizer, cancelFinalizer)
import           Text.Read                   (readMaybe)

import           System.Random               (randomIO)
import           System.Directory            (canonicalizePath)
import           System.FilePath             (pathSeparator)

primReal :: forall graph m. Builder.StdBuilder graph m => m (Map IR.Name Def.Def)
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

    plus    <- makeFunctionPure @graph plusVal    [Builder.realLT, Builder.realLT] Builder.realLT
    times   <- makeFunctionPure @graph timesVal   [Builder.realLT, Builder.realLT] Builder.realLT
    minus   <- makeFunctionPure @graph minusVal   [Builder.realLT, Builder.realLT] Builder.realLT
    div     <- makeFunctionPure @graph divVal     [Builder.realLT, Builder.realLT] Builder.realLT
    logBase <- makeFunctionPure @graph logBaseVal [Builder.realLT, Builder.realLT] Builder.realLT
    pow     <- makeFunctionPure @graph powVal     [Builder.realLT, Builder.realLT] Builder.realLT

    eq <- makeFunctionPure @graph eqVal [Builder.realLT, Builder.realLT] Builder.boolLT
    lt <- makeFunctionPure @graph ltVal [Builder.realLT, Builder.realLT] Builder.boolLT
    gt <- makeFunctionPure @graph gtVal [Builder.realLT, Builder.realLT] Builder.boolLT

    round <- makeFunctionPure @graph roundVal [Builder.realLT, Builder.intLT] Builder.realLT
    show  <- makeFunctionPure @graph showVal  [Builder.realLT] Builder.textLT

    floor   <- makeFunctionPure @graph floorVal   [Builder.realLT] Builder.intLT
    ceiling <- makeFunctionPure @graph ceilingVal [Builder.realLT] Builder.intLT

    sin <- makeFunctionPure @graph sinVal [Builder.realLT] Builder.realLT
    cos <- makeFunctionPure @graph cosVal [Builder.realLT] Builder.realLT
    tan <- makeFunctionPure @graph tanVal [Builder.realLT] Builder.realLT

    asin <- makeFunctionPure @graph asinVal [Builder.realLT] Builder.realLT
    acos <- makeFunctionPure @graph acosVal [Builder.realLT] Builder.realLT
    atan <- makeFunctionPure @graph atanVal [Builder.realLT] Builder.realLT

    sinh <- makeFunctionPure @graph sinhVal [Builder.realLT] Builder.realLT
    cosh <- makeFunctionPure @graph coshVal [Builder.realLT] Builder.realLT
    tanh <- makeFunctionPure @graph tanhVal [Builder.realLT] Builder.realLT

    asinh <- makeFunctionPure @graph asinhVal [Builder.realLT] Builder.realLT
    acosh <- makeFunctionPure @graph acoshVal [Builder.realLT] Builder.realLT
    atanh <- makeFunctionPure @graph atanhVal [Builder.realLT] Builder.realLT

    exp    <- makeFunctionPure @graph expVal    [Builder.realLT] Builder.realLT
    sqrt   <- makeFunctionPure @graph sqrtVal   [Builder.realLT] Builder.realLT
    log    <- makeFunctionPure @graph logVal    [Builder.realLT] Builder.realLT
    uminus <- makeFunctionPure @graph uminusVal [Builder.realLT] Builder.realLT

    toScientific <- makeFunctionPure @graph toScientificVal [Builder.realLT] Builder.scientificLT

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

primInt :: forall graph m. Builder.StdBuilder graph m => m (Map IR.Name Def.Def)
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

    plus <- makeFunctionPure @graph plusVal [Builder.intLT, Builder.intLT] Builder.intLT
    times <- makeFunctionPure @graph timesVal [Builder.intLT, Builder.intLT] Builder.intLT
    minus <- makeFunctionPure @graph minusVal [Builder.intLT, Builder.intLT] Builder.intLT
    div <- makeFunctionPure @graph divVal [Builder.intLT, Builder.intLT] Builder.intLT
    mod <- makeFunctionPure @graph modVal [Builder.intLT, Builder.intLT] Builder.intLT

    eq <- makeFunctionPure @graph eqVal [Builder.intLT, Builder.intLT] Builder.boolLT
    lt <- makeFunctionPure @graph ltVal [Builder.intLT, Builder.intLT] Builder.boolLT
    gt <- makeFunctionPure @graph gtVal [Builder.intLT, Builder.intLT] Builder.boolLT

    pred <- makeFunctionPure @graph predVal [Builder.intLT] Builder.intLT
    succ <- makeFunctionPure @graph succVal [Builder.intLT] Builder.intLT
    negate <- makeFunctionPure @graph negateVal [Builder.intLT] Builder.intLT

    show <- makeFunctionPure @graph showVal [Builder.intLT] Builder.textLT
    toReal <- makeFunctionPure @graph toRealVal [Builder.intLT] Builder.realLT

    shift <- makeFunctionPure @graph shiftVal [Builder.intLT, Builder.intLT] Builder.intLT


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

primBinary :: forall graph m. Builder.StdBuilder graph m => m (Map IR.Name Def.Def)
primBinary = do
    let toTextVal  = flip Luna.toValue Text.decodeUtf8
        eqVal      = flip Luna.toValue ((==) :: ByteString -> ByteString -> Bool)
        plusVal    = flip Luna.toValue ((<>) :: ByteString -> ByteString -> ByteString)
        lenVal     = flip Luna.toValue (fromIntegral . ByteString.length :: ByteString -> Integer)
        takeVal    = flip Luna.toValue (flip (ByteString.take . fromIntegral) :: ByteString -> Integer -> ByteString)
        dropVal    = flip Luna.toValue (flip (ByteString.drop . fromIntegral) :: ByteString -> Integer -> ByteString)

    toText <- makeFunctionPure @graph toTextVal [Builder.binaryLT] Builder.textLT
    eq <- makeFunctionPure @graph eqVal [Builder.binaryLT, Builder.binaryLT] Builder.boolLT
    plus <- makeFunctionPure @graph plusVal [Builder.binaryLT, Builder.binaryLT] Builder.binaryLT
    len <- makeFunctionPure @graph lenVal [Builder.binaryLT] Builder.intLT
    take <- makeFunctionPure @graph takeVal [Builder.binaryLT] Builder.binaryLT
    drop <- makeFunctionPure @graph dropVal [Builder.binaryLT] Builder.binaryLT
    return $ Map.fromList [ ("primBinaryToText",   toText)
                          , ("primBinaryEquals",   eq)
                          , ("primBinaryConcat",   plus)
                          , ("primBinaryLength",   len)
                          , ("primBinaryTake",     take)
                          , ("primBinaryDrop",     drop)
                          ]

primText :: forall graph m. Builder.StdBuilder graph m => m (Map IR.Name Def.Def)
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

    plus <- makeFunctionPure @graph plusVal [Builder.textLT, Builder.textLT] Builder.textLT
    shortRep <- makeFunctionPure @graph shortRepVal [Builder.textLT] Builder.textLT
    eq <- makeFunctionPure @graph eqVal [Builder.textLT, Builder.textLT] Builder.boolLT
    gt <- makeFunctionPure @graph gtVal [Builder.textLT, Builder.textLT] Builder.boolLT
    lt <- makeFunctionPure @graph ltVal [Builder.textLT, Builder.textLT] Builder.boolLT
    isEmpty <- makeFunctionPure @graph isEmptyVal [Builder.textLT] Builder.boolLT
    length <- makeFunctionPure @graph lengthVal [Builder.textLT] Builder.intLT
    hasPrefix <- makeFunctionPure @graph hasPrefixVal [Builder.textLT, Builder.textLT] Builder.boolLT
    words <- makeFunctionPure @graph wordsVal [Builder.textLT] (listLT Builder.textLT)
    lines <- makeFunctionPure @graph linesVal [Builder.textLT] (listLT Builder.textLT)
    chars <- makeFunctionPure @graph charsVal [Builder.textLT] (listLT Builder.textLT)
    lowercase <- makeFunctionPure @graph lowercaseVal [Builder.textLT] Builder.textLT
    uppercase <- makeFunctionPure @graph uppercaseVal [Builder.textLT] Builder.textLT
    reverse <- makeFunctionPure @graph reverseVal [Builder.textLT] Builder.textLT
    toBinary <- makeFunctionPure @graph toBinaryVal [Builder.textLT] Builder.binaryLT
    escapeJSON <- makeFunctionPure @graph escapeJSONVal [Builder.textLT] Builder.textLT
    toInt <- makeFunctionPure @graph toIntVal [Builder.textLT] (maybeLT Builder.intLT)
    toReal <- makeFunctionPure @graph toRealVal [Builder.textLT] (maybeLT Builder.realLT)


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

operators :: forall graph m. Builder.StdBuilder graph m => m (Map IR.Name Def.Def)
operators = do
    let iffVal = (\a b c -> if a then b else c) :: Bool -> Luna.Value -> Luna.Value -> Luna.Value
    iff <- makeFunctionPure @graph (flip Luna.toValue iffVal) [Builder.boolLT, "a", "a"] "a"
    let uminusVal = flip Luna.toValue $ Luna.dispatchMethod Builder.uminusMethodName
    uminusHdr <- Builder.makeUnaryMinusType @graph
    let uminus = Def.Precompiled $ Def.PrecompiledDef uminusVal uminusHdr

    return $ Map.fromList [ ("if.then.else", iff)
                          , (Builder.uminusFunName, uminus)
                          ]

io :: forall graph m. Builder.StdBuilder graph m
   => FinalizersCtx -> m (Map IR.Name Def.Def)
io finalizersCtx = do

    let noneT   = Builder.noneLT
        realT   = Builder.realLT
        textT   = Builder.textLT
        intT    = Builder.intLT
        binaryT = Builder.binaryLT

    let putStr :: Text -> IO ()
        putStr = putStrLn . convert
    printLn <- makeFunctionIO @graph (flip Luna.toValue putStr) [Builder.textLT] Builder.noneLT

    let runErrVal = flip Luna.toValue $ \(x :: Luna.Value) -> ((_Left %~ unwrap) <$> Luna.runError (Luna.force x) :: Luna.Eff (Either Text Luna.Data))
    runErr <- makeFunctionPure @graph runErrVal ["a"] (eitherLT Builder.textLT "a")

    let errVal = flip Luna.toValue $ \(a :: Text) -> (Luna.throw a :: Luna.Value)
    err <- makeFunctionPure @graph errVal [Builder.textLT] "a"

    let expandPathVal :: Text -> IO Text
        expandPathVal = fmap convert . canonicalizePath . convert
    expandPathF <- makeFunctionIO @graph (flip Luna.toValue expandPathVal) [textT] textT

    let readFileVal :: Text -> IO Text
        readFileVal = fmap convert . readFile . convert
    readFileF <- makeFunctionIO @graph (flip Luna.toValue readFileVal) [textT] textT

    let readBinaryVal :: Text -> IO ByteString
        readBinaryVal = ByteString.readFile . convert
    readBinaryF <- makeFunctionIO @graph (flip Luna.toValue readBinaryVal) [textT] binaryT

    let parseJSONVal :: Text -> Either Text Aeson.Value
        parseJSONVal = Bifunc.first convert . Aeson.eitherDecode . convert . Text.encodeUtf8
    parseJSON <- makeFunctionPure @graph (flip Luna.toValue parseJSONVal) [Builder.textLT] (eitherLT Builder.textLT Builder.jsonLT)

    let randomRealVal = randomIO :: IO Double
    randomReal <- makeFunctionIO @graph (flip Luna.toValue randomRealVal) [] realT

    let sleepVal = threadDelay . int
    sleep <- makeFunctionIO @graph (flip Luna.toValue sleepVal) [intT] noneT

    let forkVal :: Luna.Eff () -> IO ()
        forkVal act = mdo
            uid <- registerFinalizer finalizersCtx $ Async.uninterruptibleCancel a
            a   <- Async.async $ void (Luna.runIO $ Luna.runError act) `Exception.finally` cancelFinalizer finalizersCtx uid
            return ()
    fork <- makeFunctionIO @graph (flip Luna.toValue forkVal) [noneT] noneT

    let newEmptyMVarVal :: IO (MVar Luna.Data)
        newEmptyMVarVal = newEmptyMVar
    newEmptyMVar' <- makeFunctionIO @graph (flip Luna.toValue newEmptyMVarVal) [] (Builder.mvarLT "a")

    let putMVarVal :: MVar Luna.Data -> Luna.Data -> IO ()
        putMVarVal = putMVar
    putMVar' <- makeFunctionIO @graph (flip Luna.toValue putMVarVal) [Builder.mvarLT "a", "a"] noneT

    let takeMVarVal :: MVar Luna.Data -> IO Luna.Data
        takeMVarVal = takeMVar
        readMVarVal :: MVar Luna.Data -> IO Luna.Data
        readMVarVal = readMVar
    takeMVar' <- makeFunctionIO @graph (flip Luna.toValue takeMVarVal) [Builder.mvarLT "a"] "a"


    let writeFileVal :: Text -> Text -> IO ()
        writeFileVal p c = writeFile (convert p) (convert c)
    writeFile' <- makeFunctionIO @graph (flip Luna.toValue writeFileVal) [textT, textT] noneT

    let evaluateVal :: Text -> IO Text
        evaluateVal t = do
            Exception.evaluate $ rnf t
            return t
    evaluate' <- makeFunctionIO @graph (flip Luna.toValue evaluateVal) [textT] textT


    let pathSepVal :: Text
        pathSepVal = convert pathSeparator
    pathSep <- makeFunctionPure @graph (flip Luna.toValue pathSepVal) [] textT

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

exports :: forall graph m. Builder.StdBuilder graph m
        => FinalizersCtx -> m (Map IR.Name Def.Def)
exports finalizersCtx = do
    realFuns <- primReal   @graph
    intFuns  <- primInt    @graph
    textFuns <- primText   @graph
    binFuns  <- primBinary @graph
    opFuns   <- operators  @graph
    ioFuns   <- io @graph finalizersCtx
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

