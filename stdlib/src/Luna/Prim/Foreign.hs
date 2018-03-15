{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.Foreign where

import qualified Prelude                     as P
import           Luna.Prelude                hiding (Text, throwM)
import           Luna.IR

import           Control.Exception.Safe      (handleAny, throwString)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as Text
import           Foreign.C.Types             (CDouble, CFloat, CChar, CUChar,
                                              CWchar, CInt, CUInt, CLong,
                                              CULong, CSize, CTime)
import           Foreign.C.String            (peekCString, newCString, CString)
import qualified Foreign.LibFFI              as LibFFI
import           Foreign.Marshal.Alloc       (mallocBytes, free)
import           Foreign.Ptr                 (Ptr, FunPtr, castPtr,
                                              castPtrToFunPtr, nullPtr,
                                              plusPtr)
import           Foreign.Storable            (Storable(..))
import qualified Luna.Prim.DynamicLinker     as Linker
import           Luna.Builtin.Data.Function  (Function)
import           Luna.Builtin.Data.Module    (Imports, getObjectMethodMap)
import           Luna.Builtin.Data.LunaValue (LunaData (LunaObject),
                                              Constructor (..), Object (..),
                                              constructor, tag, fields, force')
import           Luna.Builtin.Data.LunaEff   (LunaEff, throw, runIO, runError)
import           Luna.Builtin.Prim           (RuntimeRep(..), RuntimeRepOf,
                                              ToLunaValue, toLunaValue,
                                              ToLunaData, toLunaData,
                                              FromLunaData, fromLunaData,
                                              IsBoxed, classNameOf)
import           Luna.Std.Builder            (makeFunctionIO, makeFunctionPure,
                                              maybeLT, LTp (..), int, integer,
                                              real)

ptrT :: LTp -> LTp
ptrT t = LCons "Ptr" [t]

retTypeT :: LTp -> LTp
retTypeT t = LCons "RetType" [t]

exports :: Imports -> IO (Map Name Function)
exports std = do
    let primLookupSymbolVal :: Text -> Text -> IO (FunPtr LunaData)
        primLookupSymbolVal (convert -> dll) (convert -> symbol) = do
            dl       <- Linker.loadLibrary dll
            sym      <- Linker.loadSymbol dl symbol
            return sym
    primLookupSymbol <- makeFunctionIO (toLunaValue std primLookupSymbolVal)
                            ["Text", "Text"] "FunPtr"

    let primCallFunPtrVal :: FunPtr LunaData -> LibFFI.RetType LunaData
                          -> [LibFFI.Arg] -> IO LunaData
        primCallFunPtrVal = LibFFI.callFFI
    primCallFunPtr <- makeFunctionIO (toLunaValue std primCallFunPtrVal)
                          ["FunPtr", retTypeT "a", LCons "List" ["Arg"]] "a"


    let primVoidRetTypeVal :: LibFFI.RetType LunaData
        primVoidRetTypeVal = fmap (toLunaData std) $ LibFFI.retVoid
    primVoidRetType <- makeFunctionPure (toLunaValue std primVoidRetTypeVal)
                           [] (retTypeT "None")

    let local = Map.fromList [ ("primLookupSymbol", primLookupSymbol)
                             , ("primCallFunPtr", primCallFunPtr)
                             , ("primVoidRetType", primVoidRetType)
                             ]

    ptr     <- primPtr std
    cstring <- primCString std

    cchar  <- Map.unions <$> sequence [ primStorable @CChar  std
                                      , primNum      @CChar  std
                                      , primIntegral @CChar  std
                                      ]
    cuchar <- Map.unions <$> sequence [ primStorable @CUChar std
                                      , primNum      @CUChar std
                                      , primIntegral @CUChar std
                                      ]
    cwchar <- Map.unions <$> sequence [ primStorable @CWchar std
                                      , primNum      @CWchar std
                                      , primIntegral @CWchar std
                                      ]
    cint   <- Map.unions <$> sequence [ primStorable @CInt   std
                                      , primNum      @CInt   std
                                      , primIntegral @CInt   std
                                      ]
    cuint  <- Map.unions <$> sequence [ primStorable @CUInt  std
                                      , primNum      @CUInt  std
                                      , primIntegral @CUInt  std
                                      ]
    clong  <- Map.unions <$> sequence [ primStorable @CLong  std
                                      , primNum      @CLong  std
                                      , primIntegral @CLong  std
                                      ]
    culong <- Map.unions <$> sequence [ primStorable @CULong std
                                      , primNum      @CULong std
                                      , primIntegral @CULong std
                                      ]
    csize  <- Map.unions <$> sequence [ primStorable @CSize  std
                                      , primNum      @CSize  std
                                      , primIntegral @CSize  std
                                      ]

    ctime  <- Map.unions <$> sequence [ primStorable @CTime  std
                                      , primNum      @CTime  std
                                      , primReal     @CTime  std
                                      ]

    cdouble <- Map.unions <$> sequence [ primStorable @CDouble std
                                       , primNum      @CDouble std
                                       , primReal     @CDouble std
                                       , primFrac     @CDouble std
                                       ]
    cfloat  <- Map.unions <$> sequence [ primStorable @CFloat  std
                                       , primNum      @CFloat  std
                                       , primReal     @CFloat  std
                                       , primFrac     @CFloat  std
                                       ]

    return $ Map.unions [ local, ptr
                        , cstring, cchar, cuchar, cwchar
                        , cint, cuint, clong, culong
                        , csize, ctime
                        , cdouble, cfloat
                        ]

primPtr :: Imports -> IO (Map Name Function)
primPtr std = do
    let primPtrToCArgVal :: Ptr LunaData -> LibFFI.Arg
        primPtrToCArgVal ptr = LibFFI.argPtr ptr
    primPtrToCArg <- makeFunctionPure (toLunaValue std primPtrToCArgVal)
                         [ptrT "a"] "Arg"

    let primPtrPlusVal :: Ptr LunaData -> Integer -> Ptr LunaData
        primPtrPlusVal ptr bytes = ptr `plusPtr` fromIntegral bytes
    primPtrPlus <- makeFunctionPure (toLunaValue std primPtrPlusVal)
                       [ptrT "a", "Int"] (ptrT "a")

    let primPtrRetTypeVal :: LibFFI.RetType LunaData -> LibFFI.RetType LunaData
        primPtrRetTypeVal = fmap (toLunaData std) . LibFFI.retPtr
    primPtrRetType <- makeFunctionPure (toLunaValue std primPtrRetTypeVal)
                          [retTypeT "a"] (retTypeT (ptrT "a"))

    let primPtrByteSizeVal :: Integer
        primPtrByteSizeVal = fromIntegral $ sizeOf (undefined :: Ptr ())
    primPtrByteSize <- makeFunctionPure (toLunaValue std primPtrByteSizeVal)
                           [] "Int"

    let primPtrCastVal :: Ptr LunaData -> Ptr LunaData
        primPtrCastVal = id
    primPtrCast <- makeFunctionPure (toLunaValue std primPtrCastVal)
                       [ptrT "a"] (ptrT "b")

    let primPtrReadPtrVal :: Ptr LunaData -> IO (Ptr LunaData)
        primPtrReadPtrVal p = peek (castPtr p :: Ptr (Ptr LunaData))
    primPtrReadPtr <- makeFunctionIO (toLunaValue std primPtrReadPtrVal)
                          [ptrT (ptrT "a")] (ptrT "a")

    let primPtrWritePtrVal :: Ptr LunaData -> Ptr LunaData -> IO ()
        primPtrWritePtrVal p = poke (castPtr p :: Ptr (Ptr LunaData))
    primPtrWritePtr <- makeFunctionIO (toLunaValue std primPtrWritePtrVal)
                           [ptrT (ptrT "a"), ptrT "a"] "None"

    let primPtrEqVal :: Ptr LunaData -> Ptr LunaData -> Bool
        primPtrEqVal = (==)
    primPtrEq <- makeFunctionPure (toLunaValue std primPtrEqVal)
                     [ptrT "a", ptrT "a"] "Bool"

    primNullPtr <- makeFunctionPure (toLunaValue std (nullPtr :: Ptr LunaData))
                       [] (ptrT "a")

    let primMallocVal :: Integer -> IO (Ptr LunaData)
        primMallocVal = mallocBytes . fromIntegral
    primMalloc <- makeFunctionIO (toLunaValue std primMallocVal)
                      ["Int"] (ptrT "a")

    let primFreeVal :: Ptr LunaData -> IO ()
        primFreeVal = free
    primFree <- makeFunctionIO (toLunaValue std primFreeVal) [ptrT "a"] "None"


    return $ Map.fromList [ ("primNullPtr", primNullPtr)
                          , ("primPtrToCArg", primPtrToCArg)
                          , ("primPtrRetType", primPtrRetType)
                          , ("primPtrByteSize", primPtrByteSize)
                          , ("primPtrReadPtr", primPtrReadPtr)
                          , ("primPtrWritePtr", primPtrWritePtr)
                          , ("primPtrCast", primPtrCast)
                          , ("primPtrPlus", primPtrPlus)
                          , ("primPtrEq", primPtrEq)
                          , ("primMalloc", primMalloc)
                          , ("primFree", primFree)
                          ]

primCString :: Imports -> IO (Map Name Function)
primCString std = do
    let primCStringFromTextVal :: Text -> IO (Ptr LunaData)
        primCStringFromTextVal = fmap castPtr . newCString . convert
    primCStringFromText <- makeFunctionIO
                               (toLunaValue std primCStringFromTextVal)
                               ["Text"]
                               (ptrT "CChar")

    let primCStringToTextVal :: Ptr LunaData -> IO Text
        primCStringToTextVal = fmap convert . peekCString . castPtr
    primCStringToText <- makeFunctionIO (toLunaValue std primCStringToTextVal)
                             [ptrT "CChar"] "Text"

    return $ Map.fromList [ ("primCStringFromText", primCStringFromText)
                          , ("primCStringToText", primCStringToText)
                          ]

class PrimCFFI a where
    retType :: LibFFI.RetType a
    toArg   :: a -> LibFFI.Arg

makePrimFunName :: Name -> Name -> Name
makePrimFunName cls fun = "prim" <> cls <> fun

primStorable :: forall a. ( PrimCFFI a
                          , Storable a
                          , IsBoxed a
                          , ToLunaData a
                          , ToLunaValue a
                          , ToLunaData (a -> IO ())
                          , ToLunaData (a -> Integer)
                          , ToLunaData (a -> LibFFI.Arg)
                          ) => Imports -> IO (Map Name Function)
primStorable std = do
    let typeName = classNameOf @a
        tp       = LCons typeName []
    !primToArg   <- makeFunctionPure (toLunaValue std (toArg @a)) [tp] "Arg"
    !primRetType <- makeFunctionPure
                        (toLunaValue std (toLunaData std <$> (retType @a)))
                        []
                        (retTypeT tp)

    let primWritePtrVal :: Ptr LunaData -> a -> IO ()
        primWritePtrVal p = poke $ coerce p
    !primWritePtr <- makeFunctionIO (toLunaValue std primWritePtrVal)
                         [ptrT tp, tp] "None"

    let primReadPtrVal :: Ptr LunaData -> IO a
        primReadPtrVal p = peek $ coerce p
    !primReadPtr <- makeFunctionIO (toLunaValue std primReadPtrVal) [ptrT tp] tp

    let primByteSizeVal :: Integer
        primByteSizeVal = fromIntegral $ sizeOf (undefined :: a)
    !primByteSize <- makeFunctionIO (toLunaValue std primByteSizeVal) [] "Int"

    let mkName = makePrimFunName typeName

    return $! Map.fromList [ (mkName "ToArg",    primToArg)
                           , (mkName "RetType",  primRetType)
                           , (mkName "WritePtr", primWritePtr)
                           , (mkName "ReadPtr",  primReadPtr)
                           , (mkName "ByteSize", primByteSize)
                           ]

primNum :: forall a. ( Num a
                     , Ord a
                     , Show a
                     , IsBoxed a
                     , ToLunaData a
                     , ToLunaValue a
                     , ToLunaValue (a -> a)
                     , ToLunaValue (a -> Text)
                     , ToLunaValue (a -> a -> a)
                     , ToLunaValue (a -> a -> Bool)
                     ) => Imports -> IO (Map Name Function)
primNum std = do
    let typeName = classNameOf @a
        tp       = LCons typeName []

    primPlus <- makeFunctionPure (toLunaValue std ((+) :: a -> a -> a))
                    [tp, tp] tp
    primMul  <- makeFunctionPure (toLunaValue std ((*) :: a -> a -> a))
                    [tp, tp] tp
    primSub  <- makeFunctionPure (toLunaValue std ((-) :: a -> a -> a))
                    [tp, tp] tp

    primToText <- makeFunctionPure
                      (toLunaValue std (convert . show :: a -> Text))
                      [tp]
                      "Text"

    primEq <- makeFunctionPure (toLunaValue std ((==) :: a -> a -> Bool))
                  [tp, tp] "Bool"
    primLt <- makeFunctionPure (toLunaValue std ((<)  :: a -> a -> Bool))
                  [tp, tp] "Bool"
    primGt <- makeFunctionPure (toLunaValue std ((>)  :: a -> a -> Bool))
                  [tp, tp] "Bool"

    primNegate <- makeFunctionPure (toLunaValue std (negate :: a -> a)) [tp] tp
    primAbs    <- makeFunctionPure (toLunaValue std (abs    :: a -> a)) [tp] tp

    let mkName = makePrimFunName typeName

    return $ Map.fromList [ (mkName "Plus", primPlus)
                          , (mkName "Mul", primMul)
                          , (mkName "Sub", primSub)
                          , (mkName "ToText", primToText)
                          , (mkName "Eq", primEq)
                          , (mkName "Lt", primLt)
                          , (mkName "Gt", primGt)
                          , (mkName "Negate", primNegate)
                          , (mkName "Abs", primAbs)
                          ]

primIntegral :: forall a. ( Integral a
                          , IsBoxed a
                          , ToLunaData a
                          , ToLunaValue a
                          , ToLunaValue (a -> Integer)
                          , ToLunaValue (a -> a -> a)
                          ) => Imports -> IO (Map Name Function)
primIntegral std = do
    let typeName = classNameOf @a
        tp       = LCons typeName []

    primToInt   <- makeFunctionPure
                       (toLunaValue std (fromIntegral :: a -> Integer))
                       [tp]
                       "Int"
    primFromInt <- makeFunctionPure
                       (toLunaValue std (fromIntegral :: Integer -> a))
                       ["Int"]
                       tp

    primDiv <- makeFunctionPure (toLunaValue std (div :: a -> a -> a))
                   [tp, tp] tp
    primMod <- makeFunctionPure (toLunaValue std (mod :: a -> a -> a))
                   [tp, tp] tp

    let mkName = makePrimFunName typeName

    return $ Map.fromList [ (mkName "ToInt", primToInt)
                          , (mkName "FromInt", primFromInt)
                          , (mkName "Div", primDiv)
                          , (mkName "Mod", primMod)
                          ]

primFrac :: forall a. ( Real a
                      , Fractional a
                      , IsBoxed a
                      , ToLunaData a
                      , ToLunaValue a
                      , ToLunaValue (a -> a -> a)
                      ) => Imports -> IO (Map Name Function)
primFrac std = do
    let typeName = classNameOf @a
        tp       = LCons typeName []

    primDiv      <- makeFunctionPure
                        (toLunaValue std ((/) :: a -> a -> a))
                        [tp, tp]
                        tp
    primFromReal <- makeFunctionPure
                        (toLunaValue std (realToFrac :: Double -> a))
                        ["Real"]
                        tp

    let mkName = makePrimFunName typeName

    return $ Map.fromList [ (mkName "Div", primDiv)
                          , (mkName "FromReal", primFromReal)
                          ]

primReal :: forall a. ( Real a
                      , IsBoxed a
                      , ToLunaValue a
                      , ToLunaValue (a -> Double)
                      ) => Imports -> IO (Map Name Function)
primReal std = do
    let typeName = classNameOf @a
        tp       = LCons typeName []

    primToReal  <- makeFunctionPure
                       (toLunaValue std (realToFrac :: a -> Double))
                       [tp]
                       "Real"
    primFromInt <- makeFunctionPure
                       (toLunaValue std (fromIntegral :: Integer -> a))
                       ["Int"]
                       tp

    let mkName = makePrimFunName typeName

    return $ Map.fromList [ (mkName "ToReal", primToReal)
                          , (mkName "FromInt", primFromInt)]

type instance RuntimeRepOf CChar = AsNative "CChar"
instance PrimCFFI CChar where
    retType = LibFFI.retCChar ; {-# INLINE retType #-}
    toArg   = LibFFI.argCChar ; {-# INLINE toArg   #-}

type instance RuntimeRepOf CUChar = AsNative "CUChar"
instance PrimCFFI CUChar where
    retType = LibFFI.retCUChar ; {-# INLINE retType #-}
    toArg   = LibFFI.argCUChar ; {-# INLINE toArg   #-}

type instance RuntimeRepOf CWchar = AsNative "CWChar"
instance PrimCFFI CWchar where
    retType = LibFFI.retCWchar ; {-# INLINE retType #-}
    toArg   = LibFFI.argCWchar ; {-# INLINE toArg   #-}

type instance RuntimeRepOf CInt = AsNative "CInt"
instance PrimCFFI CInt where
    retType = LibFFI.retCInt ; {-# INLINE retType #-}
    toArg   = LibFFI.argCInt ; {-# INLINE toArg   #-}

type instance RuntimeRepOf CUInt = AsNative "CUInt"
instance PrimCFFI CUInt where
    retType = LibFFI.retCUInt ; {-# INLINE retType #-}
    toArg   = LibFFI.argCUInt ; {-# INLINE toArg   #-}

type instance RuntimeRepOf CLong = AsNative "CLong"
instance PrimCFFI CLong where
    retType = LibFFI.retCLong ; {-# INLINE retType #-}
    toArg   = LibFFI.argCLong ; {-# INLINE toArg   #-}

type instance RuntimeRepOf CULong = AsNative "CULong"
instance PrimCFFI CULong where
    retType = LibFFI.retCULong ; {-# INLINE retType #-}
    toArg   = LibFFI.argCULong ; {-# INLINE toArg   #-}

type instance RuntimeRepOf CSize = AsNative "CSize"
instance PrimCFFI CSize where
    retType = LibFFI.retCSize ; {-# INLINE retType #-}
    toArg   = LibFFI.argCSize ; {-# INLINE toArg   #-}

type instance RuntimeRepOf CTime = AsNative "CTime"
instance PrimCFFI CTime where
    retType = LibFFI.retCTime ; {-# INLINE retType #-}
    toArg   = LibFFI.argCTime ; {-# INLINE toArg   #-}

type instance RuntimeRepOf CFloat = AsNative "CFloat"
instance PrimCFFI CFloat where
    retType = LibFFI.retCFloat ; {-# INLINE retType #-}
    toArg   = LibFFI.argCFloat ; {-# INLINE toArg   #-}

type instance RuntimeRepOf CDouble = AsNative "CDouble"
instance PrimCFFI CDouble where
    retType = LibFFI.retCDouble ; {-# INLINE retType #-}
    toArg   = LibFFI.argCDouble ; {-# INLINE toArg   #-}


type instance RuntimeRepOf LibFFI.Arg                = AsNative "Arg"
type instance RuntimeRepOf (LibFFI.RetType LunaData) = AsNative "RetType"
type instance RuntimeRepOf (FunPtr LunaData)         = AsNative "FunPtr"
type instance RuntimeRepOf (Ptr    LunaData)         = AsNative "Ptr"
