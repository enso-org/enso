{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module Luna.Builtin.Std where


import qualified Prelude (read)

import           Control.Concurrent
import           Control.DeepSeq                              (rnf)
import qualified Control.Exception                            as Exception
import           Control.Monad.Except
import           Control.Monad.Trans.State                    (evalStateT, get)
import           System.FilePath                              (pathSeparator)

import qualified Data.Aeson                                   as Aeson
import qualified Data.Bifunctor                               as Bifunc
import           Data.ByteString.Char8                        (pack)
import qualified Data.ByteString                              as StrictByteString
import           Data.ByteString.Lazy                         (ByteString)
import qualified Data.ByteString.Lazy                         as ByteString
import qualified Data.CaseInsensitive                         as CI
import           Data.Char                                    (isUpper)
import           Data.Fixed                                   (Pico)
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
import qualified Data.Time.Calendar                           as Time
import qualified Data.Time.Format                             as Time
import           Data.Text.Lazy                               (Text)
import qualified Data.Text                                    as AnyText
import qualified Data.Text.Lazy                               as Text
import qualified Data.Text.Lazy.Encoding                      as Text
import qualified Data.Text.Encoding                           as T
import           Data.TypeDesc
import           Data.UUID                                    (UUID)
import qualified Data.UUID.V4                                 as UUID

import           Foreign.C                                    (ePIPE, Errno (Errno))

import           GHC.IO.Exception                             (IOErrorType (ResourceVanished), IOException (..))
import           GHC.IO.Handle                                (Handle, BufferMode (..))
import qualified GHC.IO.Handle                                as Handle

import           Luna.Builtin.Data.Class                      (Class (..))
import           Luna.Builtin.Data.Function                   as Function
import           Luna.Builtin.Data.LunaEff                    (LunaEff, performIO, runError, runIO, throw)
import           Luna.Builtin.Data.LunaValue                  (constructor, Constructor (..), fields, force', LunaData (..), LunaValue, Object (..), tag, force)
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
import           Luna.Prelude                                 as P hiding (cons, Constructor, nothing, Text, toList, force)
import           Luna.Test.IR.Runner
import           Luna.Test.Utils

import qualified Network.HTTP.Client                          as HTTP
import qualified Network.HTTP.Client.TLS                      as HTTP
import qualified Network.HTTP.Simple                          as HTTP
import qualified Network.HTTP.Types                           as HTTP
import qualified Network.HTTP.Types.Header                    as HTTP
import           Network.Socket                               (withSocketsDo, PortNumber, SockAddr, Socket)
import qualified Network.Socket                               as Socket
import qualified Network.WebSockets                           as WebSocket
import qualified Web.Authenticate.OAuth                       as OAuth
import qualified Wuss                                         as WebSocket

import           OCI.IR.Combinators
import           OCI.IR.Name.Qualified
import           OCI.Pass                                     (SubPass)
import qualified OCI.Pass                                     as Pass

import           System.Exit                                  (ExitCode (ExitFailure, ExitSuccess))
import           System.Process                               (CreateProcess, ProcessHandle, StdStream (CreatePipe, Inherit, NoStream, UseHandle))
import qualified System.Process                               as Process

import           System.Random                                (randomIO)
import           System.Directory                             (canonicalizePath)
import           Text.Read                                    (readMaybe)


stdlibImports :: [QualName]
stdlibImports = [ ["Std", "Base"] , ["Std", "HTTP"], ["Std", "System"], ["Std", "Time"], ["Std", "WebSockets"] ]

data LTp = LVar Name | LCons Name [LTp]

instance IsString LTp where
    fromString ""      = error "LTp.fromString: empty string does not represent a type"
    fromString s@(c:_) = let s' = convert s in if isUpper c then LCons s' [] else LVar s'

varNamesFromType :: LTp -> Set Name
varNamesFromType (LVar n)    = Set.singleton n
varNamesFromType (LCons n s) = varNamesFromTypes s

varNamesFromTypes :: [LTp] -> Set Name
varNamesFromTypes = Set.unions . fmap varNamesFromType

listLT :: LTp -> LTp
listLT t  = LCons "List"  [t]

maybeLT :: LTp -> LTp
maybeLT t = LCons "Maybe" [t]

eitherLT :: LTp -> LTp -> LTp
eitherLT l r = LCons "Either" [l, r]

int :: Integer -> Int
int = fromIntegral

integer :: Int -> Integer
integer = fromIntegral

real :: Real a => a -> Double
real = realToFrac

pico :: Double -> Pico
pico = realToFrac

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

makeType :: [LTp] -> LTp -> Name -> IO (Assumptions, Rooted SomeExpr)
makeType args out outMonadName = do
    res <- runPM False $ do
        runRegs
        Pass.eval' @TestPass $ do
            let varNames = Set.toList $ varNamesFromTypes $ out : args
            vars <- fmap generalize <$> mapM var varNames
            let varMap = Map.fromList $ zip varNames vars
            fieldTypes <- mapM (mkType varMap) args
            (outType, outM) <- mkType varMap out
            pure       <- cons_ @Draft "Pure"
            outMonad   <- cons_ @Draft outMonadName
            let lamInPure o i = do
                    l  <- lam i o
                    lm <- monadic l pure
                    return $ generalize lm
            funType <- foldM lamInPure outType $ fst <$> reverse fieldTypes
            monads  <- scanM (fmap generalize .: unify) (unsafeGeneralize outMonad) (snd <$> fieldTypes)
            let lastMonad = head $ reverse monads
            replace lastMonad outM
            res <- compile $ generalize funType
            return (Assumptions def (tail monads) def def, res)
    case res of
        Right r -> return r
        Left e  -> error $ show e

makeTypePure :: [LTp] -> LTp -> IO (Assumptions, Rooted SomeExpr)
makeTypePure args out = makeType args out "IO"

makeFunction :: LunaValue -> [LTp] -> LTp -> Name -> IO Function
makeFunction val args out outMonad = do
    (assumptions, rooted) <- makeType args out outMonad
    return $ Function rooted val assumptions

makeFunctionPure :: LunaValue -> [LTp] -> LTp -> IO Function
makeFunctionPure val args out = makeFunction val args out "Pure"

makeFunctionIO :: LunaValue -> [LTp] -> LTp -> IO Function
makeFunctionIO val args out = makeFunction val args out "IO"

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
        return $ Function rooted (evalStateT val def) (Assumptions (unwrap unifies) (unwrap merges) (unwrap apps) (getAccs accs))
    return res

withExceptions :: IO a -> LunaEff a
withExceptions a = do
    res <- performIO $ catchAll (Right <$> a) (return . Left . show)
    case res of
        Left a  -> throw a
        Right r -> return r

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
        milisecondsVal = toLunaValue imps ((/ 1000) . realToFrac :: Integer -> Time.DiffTime)
    return $ Map.fromList [ ("primIntAdd",         Function boxed3Ints plusVal        boxed3IntsAssumptions)
                          , ("primIntMultiply",    Function boxed3Ints timeVal        boxed3IntsAssumptions)
                          , ("primIntSubtract",    Function boxed3Ints minusVal       boxed3IntsAssumptions)
                          , ("primIntDivide",      Function boxed3Ints divVal         boxed3IntsAssumptions)
                          , ("primIntModulo",      Function boxed3Ints modVal         boxed3IntsAssumptions)
                          , ("primIntPred",        Function boxed2Ints predVal        boxed2IntsAssumptions)
                          , ("primIntSucc",        Function boxed2Ints succVal        boxed2IntsAssumptions)
                          , ("primIntNegate",      Function boxed2Ints negateVal      boxed2IntsAssumptions)
                          , ("primIntMiliseconds", Function int2TI     milisecondsVal int2TIAssumptions    )
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

preludeUnaryOp op = compileFunction def $ do
    a     <- var "a"
    acpl  <- acc a op
    l1    <- lam a acpl
    tpA   <- var "a"
    monA  <- var "monA"
    monB  <- var "monB"
    montA <- monadic tpA monA
    montB <- monadic tpA monB
    tl1   <- lam montA montB
    pure  <- cons_ @Draft "Pure"
    tl1M  <- monadic tl1 pure
    reconnectLayer' @UserType (Just tl1M) l1
    return $ generalize l1

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

prelude :: Imports -> IO (Map Name Function)
prelude imps = mdo
    minus    <- preludeArithOp "-"
    times    <- preludeArithOp "*"
    mod      <- preludeArithOp "%"
    plus     <- preludeArithOp "+"
    div      <- preludeArithOp "/"
    pow      <- preludeArithOp "^"
    uminus   <- preludeUnaryOp "negate"
    gt       <- preludeCmpOp importBoxes ">"
    lt       <- preludeCmpOp importBoxes "<"
    gte      <- preludeCmpOp importBoxes ">="
    lte      <- preludeCmpOp importBoxes "<="
    eq       <- preludeCmpOp importBoxes "=="
    realFuns <- primReal     importBoxes
    intFuns  <- primInt      importBoxes
    textFuns <- primText     importBoxes
    binFuns  <- primBinary   importBoxes
    let opMap  = Map.fromList [("+", plus), ("-", minus), ("*", times), ("^", pow), (">", gt), ("<", lt), (">=", gte), ("<=", lte), ("==", eq), ("%", mod), ("/", div), ("#uminus#", uminus)]
        funMap = Map.unions   [realFuns, intFuns, textFuns, binFuns, opMap]
    let importBoxes = unionImports imps $ Imports def (WithDocumentation def . Right <$> funMap)
    return funMap


stdlib :: Imports -> IO (Map Name Function)
stdlib = prelude

newtype GCState = GCState (Map UUID (IO ()))
makeWrapped ''GCState

instance Default GCState where
    def = GCState def

cleanupGC :: GCState -> IO GCState
cleanupGC st = sequence (unwrap st) >> return def

data WSConnection = WSConnection { wsConn :: WebSocket.Connection, sem :: MVar () }
data WSServer     = WSServer     { wsServerSock   :: Socket
                                 , wsServerConns  :: MVar (Map SockAddr WSConnection)
                                 , wsServerMsg    :: MVar (WSConnection, Text)
                                 }

systemStd :: Imports -> IO (IO (), Map Name Function)
systemStd std = do
    stdFuncs <- stdlib std

    gcState <- newMVar $ GCState Map.empty

    -- wrappers for simple types
    let noneT   = LCons "None"   []
        boolT   = LCons "Bool"   []
        intT    = LCons "Int"    []
        realT   = LCons "Real"   []
        textT   = LCons "Text"   []
        binaryT = LCons "Binary" []
        tuple2T t1 t2       = LCons "Tuple2" [t1, t2]
        tuple3T t1 t2 t3    = LCons "Tuple3" [t1, t2, t3]
        tuple4T t1 t2 t3 t4 = LCons "Tuple4" [t1, t2, t3, t4]

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
    printLn <- makeFunctionIO (toLunaValue std putStr) ["Text"] "None"

    let runErrVal = toLunaValue std $ \(x :: LunaValue) -> ((_Left %~ convert) <$> runError (force x) :: LunaEff (Either Text LunaData))
    runErr <- makeFunctionPure runErrVal ["a"] (eitherLT "Text" "a")

    let errVal = toLunaValue std $ \(a :: Text) -> (throw $ convert a :: LunaValue)
    err <- makeFunctionPure errVal ["Text"] "a"

    let expandPathVal :: Text -> LunaEff Text
        expandPathVal = withExceptions . fmap convert . canonicalizePath . convert
    expandPathF <- makeFunctionIO (toLunaValue std expandPathVal) [textT] textT

    let readFileVal :: Text -> LunaEff Text
        readFileVal = withExceptions . fmap convert . readFile . convert
    readFileF <- makeFunctionIO (toLunaValue std readFileVal) [textT] textT

    let readBinaryVal :: Text -> LunaEff ByteString
        readBinaryVal = withExceptions . ByteString.readFile . convert
    readBinaryF <- makeFunctionIO (toLunaValue std readBinaryVal) [textT] binaryT

    let parseJSONVal :: Text -> LunaEff (Either Text Aeson.Value)
        parseJSONVal = return . Bifunc.first convert . Aeson.eitherDecode . Text.encodeUtf8
    parseJSON <- makeFunctionPure (toLunaValue std parseJSONVal) ["Text"] (eitherLT "Text" "JSON")

    let encodeMsgPackVal = toLunaValue std (MsgPack.pack :: MsgPack.Object -> ByteString)
    encodeMsgPack <- makeFunctionPure encodeMsgPackVal ["MsgPack"] "Binary"

    let parseMsgPackVal :: ByteString -> LunaEff MsgPack.Object
        parseMsgPackVal = withExceptions . MsgPack.unpack
    parseMsgPack <- makeFunctionIO (toLunaValue std parseMsgPackVal) [binaryT] $ LCons "MsgPack" []

    let signOAuth1 :: (Text, Text, Text, Text) -> HTTP.Request -> IO HTTP.Request
        signOAuth1 (cak, cas, ot, ots) req = do
            let oauth = OAuth.newOAuth { OAuth.oauthConsumerKey = convert cak, OAuth.oauthConsumerSecret = convert cas }
                creds = OAuth.newCredential (convert ot) (convert ots)
            OAuth.signOAuth oauth creds req

    let primPerformHttpVal :: Text -> Text -> [(Text, Text)] -> Maybe (Text, Text) -> Maybe (Text, Text, Text, Text) -> [(Text, Maybe Text)] -> ByteString -> LunaEff (HTTP.Response HTTP.BodyReader)
        primPerformHttpVal uri method headers auth oauth params body = withExceptions $ do
            let packHeader (k, v) = (CI.mk $ convert k, convert v)
                packParam  (k, v) = (convert k, convert <$> v)
            baseReq <- HTTP.parseRequest (convert uri)
            let newHeaders = map packHeader headers
                oldHeaders = HTTP.requestHeaders baseReq
                oldParams  = HTTP.getRequestQueryString baseReq
            req <- baseReq
                    & HTTP.setRequestBodyLBS body
                    & HTTP.setRequestMethod  (convert method)
                    & HTTP.setRequestHeaders (oldHeaders <> newHeaders)
                    & HTTP.addRequestHeader  HTTP.hAccept (pack "*/*")
                    & HTTP.setRequestQueryString (oldParams <> map packParam params)
                    & case auth of
                        Just (u, p) -> HTTP.setRequestBasicAuth (convert u) (convert p)
                        Nothing     -> id
                    & case oauth of
                        Just oauthData -> signOAuth1 oauthData
                        Nothing        -> return
            let managerSettings = if HTTP.secure req then HTTP.tlsManagerSettings else HTTP.defaultManagerSettings
            manager <- HTTP.newManager managerSettings
            HTTP.responseOpen req manager

    let tupleT          = tuple2T textT textT
        oauthT          = maybeLT $ tuple4T textT textT textT textT
        maybeLTupleT    = maybeLT tupleT
        tupleListT      = listLT  tupleT
        tupleMaybeListT = listLT $ tuple2T textT $ maybeLT textT
    primPerformHttp <- makeFunctionIO (toLunaValue std primPerformHttpVal)
                                    [textT, textT, tupleListT, maybeLTupleT, oauthT, tupleMaybeListT, binaryT]
                                    (LCons "HttpResponse" [])

    -- web sockets state and connections --
    let wrapWSConnection :: WebSocket.Connection -> IO WSConnection
        wrapWSConnection conn = WSConnection conn <$> newEmptyMVar

        unregisterConnection :: WSConnection -> IO ()
        unregisterConnection (WSConnection _ sem) = putMVar sem ()

        waitForConnectionClose :: WSConnection -> IO ()
        waitForConnectionClose (WSConnection _ sem) = readMVar sem

        runClient :: (Integral p) => P.String -> p -> P.String -> Bool -> WebSocket.ClientApp a -> IO a
        runClient host port path secure client = if secure
            then WebSocket.runSecureClient host (fromIntegral port :: PortNumber) path client
            else WebSocket.runClient       host (fromIntegral port :: Int)        path client

        primWebSocketConnectVal :: Text -> Integer -> Text -> Bool -> LunaEff WSConnection
        primWebSocketConnectVal host port path secure = withExceptions $ do
            connection <- newEmptyMVar :: IO (MVar WSConnection)
            let app :: WebSocket.ClientApp ()
                app conn = do
                    wsConn <- wrapWSConnection conn
                    putMVar connection wsConn
                    waitForConnectionClose wsConn
                    WebSocket.sendClose conn ("bye" :: Text)

            let strippedHost = Text.stripPrefix "ws://" host <|> Text.stripPrefix "wss://" host <|> Just host & fromJust
            forkIO $ withSocketsDo $ runClient (convert strippedHost) port (convert path) secure app
            wsConnection <- readMVar connection
            let cleanup = unregisterConnection wsConnection
            registerGC cleanup
            return wsConnection

    let wsConnectionT = LCons "WSConnection" []

    primWebSocketConnect <- makeFunctionIO (toLunaValue std primWebSocketConnectVal)
                                         [textT, intT, textT, boolT] wsConnectionT

    let primWebSocketReadVal :: WSConnection -> LunaEff ByteString
        primWebSocketReadVal (WSConnection conn _) = withExceptions $ WebSocket.receiveData conn
    primWebSocketRead <- makeFunctionIO (toLunaValue std primWebSocketReadVal)
                                      [wsConnectionT] binaryT

    let primWebSocketWriteVal :: WSConnection -> Text -> LunaEff ()
        primWebSocketWriteVal (WSConnection conn _) s = withExceptions $ WebSocket.sendTextData conn s
    primWebSocketWrite <- makeFunctionIO (toLunaValue std primWebSocketWriteVal)
                                       [wsConnectionT, textT] noneT

    let primWebSocketWriteBinVal :: WSConnection -> ByteString -> LunaEff ()
        primWebSocketWriteBinVal (WSConnection conn _) s = withExceptions $ WebSocket.sendBinaryData conn s
    primWebSocketWriteBin <- makeFunctionIO (toLunaValue std primWebSocketWriteVal)
                                       [wsConnectionT, binaryT] noneT

    let primWebSocketCloseVal :: WSConnection -> LunaEff ()
        primWebSocketCloseVal = withExceptions . unregisterConnection
    primWebSocketClose <- makeFunctionIO (toLunaValue std primWebSocketCloseVal)
                                       [wsConnectionT] noneT

    -- websocket servers --
    let wsServerT = LCons "WSServer" []

        addWSServerConn :: SockAddr -> WSConnection -> MVar (Map SockAddr WSConnection) -> IO ()
        addWSServerConn saddr conn conns = modifyMVar_ conns $ return . Map.insert saddr conn

        initServer :: Text -> Integer -> IO WSServer
        initServer host port = do
            serverSock <- newEmptyMVar      :: IO (MVar Socket)
            conns      <- newMVar Map.empty :: IO (MVar (Map SockAddr WSConnection))
            msg        <- newEmptyMVar      :: IO (MVar (WSConnection, Text))
            sock       <- WebSocket.makeListenSocket (convert host) (int port)
            let wss = WSServer sock conns msg
            putMVar serverSock sock
            return wss

        receiveMessage :: WSServer -> WSConnection -> IO ()
        receiveMessage wss connection = do
            msg <- WebSocket.receiveData $ wsConn connection :: IO Text
            putMVar (wsServerMsg wss) (connection, msg)

        awaitConnections :: WSServer -> IO ()
        awaitConnections wss@(WSServer sock conns msg) = do
            mvar <- newEmptyMVar :: IO (MVar ())
            (flip forkFinally) (\_ -> putMVar mvar ()) $ forever $ do
                (sc, saddr) <- Socket.accept sock
                pc          <- WebSocket.makePendingConnection sc WebSocket.defaultConnectionOptions
                conn        <- WSConnection <$> WebSocket.acceptRequest pc <*> (newEmptyMVar :: IO (MVar ()))
                addWSServerConn saddr conn conns
                forkIO $ forever $ receiveMessage wss conn
            void $ readMVar mvar

        disconnectClients :: WSServer -> IO ()
        disconnectClients wss = do
            let conns = wsServerConns wss
            connMap <- readMVar conns
            forM_ connMap (\c -> WebSocket.sendClose (wsConn c) ("bye" :: Text))
            putMVar conns Map.empty

        primCreateWSServerVal :: Text -> Integer -> LunaEff WSServer
        primCreateWSServerVal host port = withExceptions $ do
            server <- initServer host port
            forkIO $ Exception.bracket_ (return ()) (disconnectClients server) (awaitConnections server)
            return server
    primCreateWSServer <- makeFunctionIO (toLunaValue std primCreateWSServerVal)
                                       [textT, intT] wsServerT

    let primWSSBroadcastTextVal :: WSServer -> Text -> LunaEff WSServer
        primWSSBroadcastTextVal wss msg = withExceptions $ do
            connMap <- readMVar $ wsServerConns wss
            forM_ connMap (\c -> WebSocket.sendTextData (wsConn c) msg)
            return wss
    primWSSBroadcastText <- makeFunctionIO (toLunaValue std primWSSBroadcastTextVal)
                                         [wsServerT, textT] wsServerT

    let primWSSBroadcastBinaryVal :: WSServer -> ByteString -> LunaEff WSServer
        primWSSBroadcastBinaryVal wss msg = withExceptions $ do
            connMap <- readMVar $ wsServerConns wss
            forM_ connMap (\c -> WebSocket.sendBinaryData (wsConn c) msg)
            return wss
    primWSSBroadcastBinary <- makeFunctionIO (toLunaValue std primWSSBroadcastBinaryVal)
                                           [wsServerT, binaryT] wsServerT

    let primWSSGetMessageVal :: WSServer -> LunaEff (WSConnection, Text)
        primWSSGetMessageVal = withExceptions . readMVar . wsServerMsg
    primWSSGetMessage <- makeFunctionIO (toLunaValue std primWSSGetMessageVal)
                                       [wsServerT] (LCons "MVar" [textT])

    let primUrlEncodeVal :: Text -> Text
        primUrlEncodeVal = convert . HTTP.urlEncode False . convert
    primUrlEncode <- makeFunctionPure (toLunaValue std primUrlEncodeVal) ["Text"] "Text"

    let primGetCurrentTimeVal :: LunaEff Time.ZonedTime
        primGetCurrentTimeVal = withExceptions Time.getZonedTime
    primGetCurrentTime <- makeFunctionIO (toLunaValue std primGetCurrentTimeVal) [] $ LCons "Time" []

    let primGetCurrentTimeZoneVal :: LunaEff Time.TimeZone
        primGetCurrentTimeZoneVal = withExceptions Time.getCurrentTimeZone
    primGetCurrentTimeZone <- makeFunctionIO (toLunaValue std primGetCurrentTimeZoneVal) [] $ LCons "TimeZone" []

    let primTimeToUTCVal = toLunaValue std Time.zonedTimeToUTC
    primTimeToUTC <- makeFunctionPure primTimeToUTCVal ["Time"] "UTCTime"

    let primTimeFromUTCVal = toLunaValue std Time.utcToZonedTime
    primTimeFromUTC <- makeFunctionPure primTimeFromUTCVal ["TimeZone", "UTCTime"] "Time"

    let primDiffTimesVal = toLunaValue std Time.diffUTCTime
    primDiffTimes <- makeFunctionPure primDiffTimesVal ["UTCTime", "UTCTime"] "TimeInterval"

    let fmtTime :: Text -> Time.ZonedTime -> Text
        fmtTime fmt = convert . Time.formatTime Time.defaultTimeLocale (convert fmt :: P.String)
    primFormatTime <- makeFunctionPure (toLunaValue std fmtTime) ["Text", "Time"] "Text"

    let fmtUTCTime :: Text -> Time.UTCTime -> Text
        fmtUTCTime fmt = convert . Time.formatTime Time.defaultTimeLocale (convert fmt :: P.String)
    primFormatUTCTime <- makeFunctionPure (toLunaValue std fmtUTCTime) ["Text", "UTCTime"] "Text"

    let primTimesEqVal = toLunaValue std ((==) :: Time.UTCTime -> Time.UTCTime -> Bool)
    primTimesEq <- makeFunctionPure primTimesEqVal ["UTCTime", "UTCTime"] "Bool"

    let primAddUTCTimeVal = toLunaValue std Time.addUTCTime
        primSubUTCTimeVal = toLunaValue std (\d t -> Time.addUTCTime (-d) t)
    primAddUTCTime <- makeFunctionPure primAddUTCTimeVal ["TimeInterval", "UTCTime"] "UTCTime"
    primSubUTCTime <- makeFunctionPure primSubUTCTimeVal ["TimeInterval", "UTCTime"] "UTCTime"

    let primTimeOfDayVal :: Time.DiffTime -> (Integer, Integer, Double)
        primTimeOfDayVal t = let Time.TimeOfDay h m s = Time.timeToTimeOfDay t in (convert h, convert m, realToFrac s)
    primTimeOfDay <- makeFunctionPure (toLunaValue std primTimeOfDayVal) [LCons "TimeInterval" []] $ tuple3T intT intT intT

    let primTimeOfYearVal :: Time.Day -> (Integer, Integer, Integer)
        primTimeOfYearVal t = let (y, m, d) = Time.toGregorian t in (y, integer m, integer d)
    primTimeOfYear <- makeFunctionPure (toLunaValue std primTimeOfYearVal) [intT] $ tuple3T intT intT intT

    let primFromTimeOfYearVal :: Integer -> Integer -> Integer -> Time.Day
        primFromTimeOfYearVal y m d = Time.fromGregorian y (int m) (int d)
    primFromTimeOfYear <- makeFunctionPure (toLunaValue std primFromTimeOfYearVal) [intT, intT, intT] intT

    let primMonthLengthVal :: Integer -> Integer -> Integer
        primMonthLengthVal y m = integer $ Time.gregorianMonthLength y (int m)
    primMonthLength <- makeFunctionPure (toLunaValue std primMonthLengthVal) [intT, intT] intT

    let parseTime :: Text -> Text -> Maybe Time.ZonedTime
        parseTime fmt str = Time.parseTime Time.defaultTimeLocale (convert fmt) (convert str)
    primParseTime <- makeFunctionPure (toLunaValue std parseTime) ["Text", "Text"] (maybeLT "Time")

    let randomRealVal = performIO randomIO :: LunaEff Double
    randomReal <- makeFunctionIO (toLunaValue std randomRealVal) [] realT

    let sleepVal = performIO . threadDelay . int
    sleep <- makeFunctionIO (toLunaValue std sleepVal) [intT] noneT

    let forkVal :: LunaEff () -> LunaEff ()
        forkVal act = performIO $ mdo
            uid <- registerGC $ killThread tid
            tid <- forkFinally (void $ runIO $ runError act) $ const (deregisterGC uid)
            return ()
    fork <- makeFunctionIO (toLunaValue std forkVal) [noneT] noneT

    let newEmptyMVarVal :: LunaEff (MVar LunaData)
        newEmptyMVarVal = performIO newEmptyMVar
    newEmptyMVar' <- makeFunctionIO (toLunaValue std newEmptyMVarVal) [] (LCons "MVar" [LVar "a"])

    let putMVarVal :: MVar LunaData -> LunaData -> LunaEff ()
        putMVarVal = performIO .: putMVar
    putMVar' <- makeFunctionIO (toLunaValue std putMVarVal) [LCons "MVar" [LVar "a"], LVar "a"] noneT

    let takeMVarVal :: MVar LunaData -> LunaValue
        takeMVarVal = performIO . takeMVar
        readMVarVal :: MVar LunaData -> LunaValue
        readMVarVal = performIO . readMVar
    takeMVar' <- makeFunctionIO (toLunaValue std takeMVarVal) [LCons "MVar" [LVar "a"]] (LVar "a")

    let fileHandleT = LCons "FileHandle" []

    let writeFileVal :: Text -> Text -> LunaEff ()
        writeFileVal p c = withExceptions $ writeFile (convert p) (convert c)
    writeFile' <- makeFunctionIO (toLunaValue std writeFileVal) [textT, textT] noneT

    let runProcessVal :: CreateProcess -> LunaEff (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
        runProcessVal = withExceptions . Process.createProcess
    runProcess' <- makeFunctionIO (toLunaValue std runProcessVal) [ LCons "ProcessDescription" [] ] ( LCons "Process" [ LCons "Maybe" [ LCons "FileHandle" [] ]
                                                                                                                    , LCons "Maybe" [ LCons "FileHandle" [] ]
                                                                                                                    , LCons "Maybe" [ LCons "FileHandle" [] ]
                                                                                                                    , LCons "ProcessHandle" [] ] )
    let hIsOpenVal :: Handle -> LunaEff Bool
        hIsOpenVal = withExceptions . Handle.hIsOpen
    hIsOpen' <- makeFunctionIO (toLunaValue std hIsOpenVal) [fileHandleT] boolT

    let hIsClosedVal :: Handle -> LunaEff Bool
        hIsClosedVal = withExceptions . Handle.hIsClosed
    hIsClosed' <- makeFunctionIO (toLunaValue std hIsClosedVal) [fileHandleT] boolT

    let ignoreSigPipe :: IO () -> IO ()
        ignoreSigPipe = Exception.handle $ \e -> case e of
            IOError { ioe_type  = ResourceVanished
                    , ioe_errno = Just ioe }
                | Errno ioe == ePIPE -> return ()
            _ -> Exception.throwIO e
        hCloseVal :: Handle -> LunaEff ()
        hCloseVal = withExceptions . ignoreSigPipe . Handle.hClose

    hClose' <- makeFunctionIO (toLunaValue std hCloseVal) [fileHandleT] noneT

    let evaluateVal :: Text -> LunaEff Text
        evaluateVal t = withExceptions $ do
            Exception.evaluate $ rnf t
            return t
    evaluate' <- makeFunctionIO (toLunaValue std evaluateVal) [textT] textT

    let hGetContentsVal :: Handle -> LunaEff Text
        hGetContentsVal = fmap Text.pack . withExceptions . Handle.hGetContents
    hGetContents' <- makeFunctionIO (toLunaValue std hGetContentsVal) [fileHandleT] textT

    let hGetLineVal :: Handle -> LunaEff Text
        hGetLineVal = fmap Text.pack . withExceptions . Handle.hGetLine
    hGetLine' <- makeFunctionIO (toLunaValue std hGetLineVal) [fileHandleT] textT

    let hPutTextVal :: Handle -> Text -> LunaEff ()
        hPutTextVal h = withExceptions . Handle.hPutStr h . Text.unpack
    hPutText' <- makeFunctionIO (toLunaValue std hPutTextVal) [fileHandleT, textT] noneT

    let hFlushVal :: Handle -> LunaEff ()
        hFlushVal = withExceptions . Handle.hFlush
    hFlush' <- makeFunctionIO (toLunaValue std hFlushVal) [fileHandleT] noneT

    let waitForProcessVal :: ProcessHandle -> LunaEff ExitCode
        waitForProcessVal = withExceptions . Process.waitForProcess
    waitForProcess' <- makeFunctionIO (toLunaValue std waitForProcessVal) [LCons "ProcessHandle" []] (LCons "ExitCode" [])

    let hSetBufferingVal :: Handle -> BufferMode -> LunaEff ()
        hSetBufferingVal = withExceptions .: Handle.hSetBuffering
    hSetBuffering' <- makeFunctionIO (toLunaValue std hSetBufferingVal) [fileHandleT, LCons "BufferMode" []] noneT

    let pathSepVal :: Text
        pathSepVal = convert pathSeparator
    pathSep <- makeFunctionPure (toLunaValue std pathSepVal) [] textT

    let systemFuncs = Map.fromList [ ("putStr", printLn)
                                   , ("errorStr", err)
                                   , ("runError", runErr)
                                   , ("primReadFile", readFileF)
                                   , ("expandPath", expandPathF)
                                   , ("readBinary", readBinaryF)
                                   , ("pathSeparator", pathSep)
                                   , ("primWriteFile", writeFile')
                                   , ("parseJSON", parseJSON)
                                   , ("parseMsgPack", parseMsgPack)
                                   , ("encodeMsgPack", encodeMsgPack)
                                   , ("primPerformHttp", primPerformHttp)
                                   , ("primWebSocketConnect", primWebSocketConnect)
                                   , ("primWebSocketRead", primWebSocketRead)
                                   , ("primWebSocketWrite", primWebSocketWrite)
                                   , ("primWebSocketWriteBin", primWebSocketWriteBin)
                                   , ("primWebSocketClose", primWebSocketClose)
                                   , ("primCreateWSServer", primCreateWSServer)
                                   , ("primWSSBroadcastText", primWSSBroadcastText)
                                   , ("primWSSBroadcastBinary", primWSSBroadcastBinary)
                                   , ("primWSSGetMessage", primWSSGetMessage)
                                   , ("primUrlEncode", primUrlEncode)
                                   , ("primGetCurrentTime", primGetCurrentTime)
                                   , ("primGetCurrentTimeZone", primGetCurrentTimeZone)
                                   , ("primTimeToUTC", primTimeToUTC)
                                   , ("primTimeFromUTC", primTimeFromUTC)
                                   , ("primDiffTimes", primDiffTimes)
                                   , ("primFormatTime", primFormatTime)
                                   , ("primFormatUTCTime", primFormatUTCTime)
                                   , ("primTimesEq", primTimesEq)
                                   , ("primAddUTCTime", primAddUTCTime)
                                   , ("primSubUTCTime", primSubUTCTime)
                                   , ("primTimeOfDay", primTimeOfDay)
                                   , ("primTimeOfYear", primTimeOfYear)
                                   , ("primFromTimeOfYear", primFromTimeOfYear)
                                   , ("primMonthLength", primMonthLength)
                                   , ("primParseTime", primParseTime)
                                   , ("randomReal", randomReal)
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
                                   , ("primHFlush", hFlush')
                                   , ("primHSetBuffering", hSetBuffering')
                                   , ("primWaitForProcess", waitForProcess')
                                   , ("primEvaluate", evaluate')
                                   ]

    return (cleanup, Map.union systemFuncs stdFuncs)

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

type instance RuntimeRepOf WSConnection = AsNative "WSConnection"
type instance RuntimeRepOf WSServer     = AsNative "WSServer"

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

instance FromLunaData CreateProcess where
    fromLunaData v = let errorMsg = "Expected a ProcessDescription luna object, got unexpected constructor" in
        force' v >>= \case
            LunaObject obj -> case obj ^. constructor . fields of
                [command, args, stdin, stdout, stderr] -> do
                    p       <- Process.proc <$> fmap Text.unpack (fromLunaData command) <*> fmap (map Text.unpack) (fromLunaData args)
                    stdin'  <- fromLunaData stdin
                    stdout' <- fromLunaData stdout
                    stderr' <- fromLunaData stderr
                    return $ p { Process.std_in = stdin', Process.std_out = stdout', Process.std_err = stderr' }
                _ -> throw errorMsg
            _ -> throw errorMsg

instance FromLunaData StdStream where
    fromLunaData v = let errorMsg = "Expected a PipeRequest luna object, got unexpected constructor: " in
        force' v >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "Inherit"    -> return Inherit
                "UseHandle"  -> UseHandle <$> (fromLunaData . head $ obj ^. constructor . fields)
                "CreatePipe" -> return CreatePipe
                "NoStream"   -> return NoStream
                c            -> throw (errorMsg <> convert c)
            c -> throw (errorMsg <> "Not a LunaObject")

instance FromLunaData BufferMode where
    fromLunaData v = let errorMsg = "Expected a BufferMode luna object, got unexpected constructor: " in
        force' v >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "NoBuffering"    -> return NoBuffering
                "LineBuffering"  -> return LineBuffering
                "BlockBuffering" -> fmap (BlockBuffering . fmap int) . fromLunaData . head $ obj ^. constructor . fields
                c                -> throw (errorMsg <> convert c)
            c -> throw (errorMsg <> "Not a LunaObject")

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

type instance RuntimeRepOf Handle        = AsNative "FileHandle"
type instance RuntimeRepOf ProcessHandle = AsNative "ProcessHandle"

instance {-# OVERLAPS #-} ToLunaData (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) where
    toLunaData imps (hin, hout, herr, ph) = LunaObject $ Object (Constructor "Process" [toLunaData imps hin, toLunaData imps hout, toLunaData imps herr, toLunaData imps ph]) $ getObjectMethodMap "Process" imps

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

instance FromLunaData Time.Day where
    fromLunaData d = Time.ModifiedJulianDay <$> fromLunaData d

instance FromLunaData Time.UTCTime where
    fromLunaData t = let errorMsg = "Expected a UTCTime luna object, got unexpected constructor" in
        force' t >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "UTCTimeVal" -> Time.UTCTime <$> fromLunaData days <*> fromLunaData diff where [days, diff] = obj ^. constructor . fields
                _            -> throw errorMsg
            _ -> throw errorMsg

instance FromLunaData Time.TimeOfDay where
    fromLunaData tod = let errorMsg = "Expected a TimeOfDay Luna object, got unexpected constructor" in
        force' tod >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "TimeOfDayVal" -> Time.TimeOfDay <$> (int <$> fromLunaData h) <*> (int <$> fromLunaData m) <*> (pico <$> fromLunaData s)
                                      where [h, m, s] = obj ^. constructor . fields
                _              -> throw $ (show $ obj ^. constructor . tag) <> errorMsg
            _ -> throw errorMsg

instance FromLunaData Time.TimeZone where
    fromLunaData tz = let errorMsg = "Excpected a TimeZone Luna object, got unexpected constructor" in
        force' tz >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "TimeZoneVal" -> Time.TimeZone <$> (int <$> fromLunaData mins) <*> fromLunaData summer <*> ((convert :: Text -> P.String) <$> fromLunaData name)
                                     where [mins, summer, name] = obj ^. constructor . fields
                _             -> throw errorMsg
            _ -> throw errorMsg

instance FromLunaData Time.ZonedTime where
    fromLunaData zt = let errorMsg = "Expected a Time Luna object, got unexpected constructor" in
        force' zt >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "TimeVal" -> Time.ZonedTime <$> localTime <*> (fromLunaData tz)
                                      where [days, tod, tz] = obj ^. constructor . fields
                                            localTime       = Time.LocalTime <$> fromLunaData days <*> fromLunaData tod
                _              -> throw errorMsg
            _ -> throw errorMsg

instance ToLunaData Time.DiffTime where
    toLunaData imps diffTime = LunaObject $
        Object (Constructor "TimeInterval" [toLunaData imps $ Time.diffTimeToPicoseconds diffTime])
               (getObjectMethodMap "TimeInterval" imps)

instance ToLunaData Time.NominalDiffTime where
    toLunaData imps nDiffTime = toLunaData imps (realToFrac nDiffTime :: Time.DiffTime)

instance ToLunaData Time.Day where
    toLunaData imps day = toLunaData imps $ Time.toModifiedJulianDay day

instance ToLunaData Time.UTCTime where
    toLunaData imps (Time.UTCTime days diff) = LunaObject $
        Object (Constructor "UTCTimeVal" [toLunaData imps days, toLunaData imps diff])
               (getObjectMethodMap "UTCTime" imps)

instance ToLunaData Time.TimeOfDay where
    toLunaData imps (Time.TimeOfDay h m s) = LunaObject $
        Object (Constructor "TimeOfDayVal" [toLunaData imps (integer h), toLunaData imps (integer m), toLunaData imps $ real s])
               (getObjectMethodMap "TimeOfDay" imps)

instance ToLunaData Time.TimeZone where
    toLunaData imps (Time.TimeZone mins summer name) = LunaObject $
        Object (Constructor "TimeZoneVal" [toLunaData imps (integer mins), toLunaData imps summer, toLunaData imps (convert name :: Text)])
               (getObjectMethodMap "TimeZone" imps)

instance ToLunaData Time.ZonedTime where
    toLunaData imps (Time.ZonedTime (Time.LocalTime days tod) tz) = LunaObject $
        Object (Constructor "TimeVal" [toLunaData imps days, toLunaData imps tod, toLunaData imps tz])
               (getObjectMethodMap "Time" imps)
