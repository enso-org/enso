{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash         #-}

module Luna.Prim.Foreign where

import Prologue

import           Control.Exception.Safe      (handleAny, throwString)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import           Foreign.C.Types             (CDouble, CFloat, CChar, CUChar,
                                              CWchar, CInt, CUInt, CLong,
                                              CULong, CSize, CTime)
import           Foreign.C.String            (peekCString, newCString, CString)
import           Foreign.ForeignPtr.Unsafe   (unsafeForeignPtrToPtr)
import qualified Foreign.ForeignPtr          as ForeignPtr
import qualified Foreign.LibFFI              as LibFFI
import           Foreign.Marshal.Alloc       (mallocBytes, finalizerFree, free)
import           Foreign.Ptr                 (Ptr, FunPtr, castFunPtr, castPtr,
                                              castPtrToFunPtr, nullPtr,
                                              plusPtr)
import           Foreign.Storable            (Storable(..))
import           GHC.ForeignPtr              (ForeignPtr(..))
import           GHC.Exts                    (Int(..), plusAddr#)
import           Luna.Prim.CTypes            (CInt8(..), CInt16(..),
                                              CInt32(..), CInt64(..),
                                              CUInt8(..), CUInt16(..),
                                              CUInt32(..), CUInt64(..))
import qualified Luna.IR as IR
import qualified Luna.Runtime as Luna
import qualified Luna.Prim.DynamicLinker     as Linker
import qualified Luna.Pass.Sourcing.Data.Def as Def
import           Luna.Std.Builder            (makeFunctionIO, makeFunctionPure,
                                              maybeLT, LTp (..), int, integer,
                                              real)
import           Luna.Std.Finalizers         (FinalizersCtx, registerFinalizer)
import qualified System.Mem.Weak             as Weak


ptrT :: LTp -> LTp
ptrT t = LCons "Ptr" [t]

fPtrT :: LTp -> LTp
fPtrT t = LCons "ForeignPtr" [t]

retTypeT :: LTp -> LTp
retTypeT t = LCons "RetType" [t]

exports :: FinalizersCtx -> IO (Map IR.Name Def.Def)
exports finalizersCtx = do
    let primLookupSymbolVal :: Text -> Text -> IO (FunPtr Luna.Data)
        primLookupSymbolVal (convert -> dll) (convert -> symbol) = do
            dl       <- Linker.loadLibrary dll
            sym      <- Linker.loadSymbol dl symbol
            return sym
    primLookupSymbol <- makeFunctionIO (flip Luna.toValue primLookupSymbolVal)
                            ["Text", "Text"] "FunPtr"

    let primCallFunPtrVal :: FunPtr Luna.Data -> LibFFI.RetType Luna.Data
                          -> [LibFFI.Arg] -> IO Luna.Data
        primCallFunPtrVal = LibFFI.callFFI
    primCallFunPtr <- makeFunctionIO (flip Luna.toValue primCallFunPtrVal)
                          ["FunPtr", retTypeT "a", LCons "List" ["Arg"]] "a"


    let primVoidRetTypeVal :: Luna.Units -> LibFFI.RetType Luna.Data
        primVoidRetTypeVal std = fmap (Luna.toData std) $ LibFFI.retVoid
    primVoidRetType <- makeFunctionPure (\std -> Luna.toValue std $ primVoidRetTypeVal std)
                           [] (retTypeT "None")

    let local = Map.fromList [ ("primLookupSymbol", primLookupSymbol)
                             , ("primCallFunPtr", primCallFunPtr)
                             , ("primVoidRetType", primVoidRetType)
                             ]

    ptr     <- primPtr
    fptr    <- primForeignPtr finalizersCtx
    cstring <- primCString

    cchar  <- Map.unions <$> sequence [ primStorable @CChar
                                      , primNum      @CChar
                                      , primIntegral @CChar
                                      ]
    cuchar <- Map.unions <$> sequence [ primStorable @CUChar
                                      , primNum      @CUChar
                                      , primIntegral @CUChar
                                      ]
    cwchar <- Map.unions <$> sequence [ primStorable @CWchar
                                      , primNum      @CWchar
                                      , primIntegral @CWchar
                                      ]
    cint   <- Map.unions <$> sequence [ primStorable @CInt
                                      , primNum      @CInt
                                      , primIntegral @CInt
                                      ]
    cint8  <- Map.unions <$> sequence [ primStorable @CInt8
                                      , primNum      @CInt8
                                      , primIntegral @CInt8
                                      ]
    cint16 <- Map.unions <$> sequence [ primStorable @CInt16
                                      , primNum      @CInt16
                                      , primIntegral @CInt16
                                      ]
    cint32 <- Map.unions <$> sequence [ primStorable @CInt32
                                      , primNum      @CInt32
                                      , primIntegral @CInt32
                                      ]
    cint64 <- Map.unions <$> sequence [ primStorable @CInt64
                                      , primNum      @CInt64
                                      , primIntegral @CInt64
                                      ]
    cuint  <- Map.unions <$> sequence [ primStorable @CUInt
                                      , primNum      @CUInt
                                      , primIntegral @CUInt
                                      ]
    cuint8  <- Map.unions <$> sequence [ primStorable @CUInt8
                                       , primNum      @CUInt8
                                       , primIntegral @CUInt8
                                       ]
    cuint16 <- Map.unions <$> sequence [ primStorable @CUInt16
                                       , primNum      @CUInt16
                                       , primIntegral @CUInt16
                                       ]
    cuint32 <- Map.unions <$> sequence [ primStorable @CUInt32
                                       , primNum      @CUInt32
                                       , primIntegral @CUInt32
                                       ]
    cuint64 <- Map.unions <$> sequence [ primStorable @CUInt64
                                       , primNum      @CUInt64
                                       , primIntegral @CUInt64
                                       ]
    clong  <- Map.unions <$> sequence [ primStorable @CLong
                                      , primNum      @CLong
                                      , primIntegral @CLong
                                      ]
    culong <- Map.unions <$> sequence [ primStorable @CULong
                                      , primNum      @CULong
                                      , primIntegral @CULong
                                      ]
    csize  <- Map.unions <$> sequence [ primStorable @CSize
                                      , primNum      @CSize
                                      , primIntegral @CSize
                                      ]

    ctime  <- Map.unions <$> sequence [ primStorable @CTime
                                      , primNum      @CTime
                                      , primReal     @CTime
                                      ]

    cdouble <- Map.unions <$> sequence [ primStorable @CDouble
                                       , primNum      @CDouble
                                       , primReal     @CDouble
                                       , primFrac     @CDouble
                                       ]
    cfloat  <- Map.unions <$> sequence [ primStorable @CFloat
                                       , primNum      @CFloat
                                       , primReal     @CFloat
                                       , primFrac     @CFloat
                                       ]

    return $ Map.unions [ local, ptr, fptr
                        , cstring, cchar, cuchar, cwchar
                        , cint, cuint, clong, culong
                        , cint8, cint16, cint32, cint64
                        , cuint8, cuint16, cuint32, cuint64
                        , csize, ctime
                        , cdouble, cfloat
                        ]

primPtr :: IO (Map IR.Name Def.Def)
primPtr = do
    let primPtrToCArgVal :: Ptr Luna.Data -> LibFFI.Arg
        primPtrToCArgVal ptr = LibFFI.argPtr ptr
    primPtrToCArg <- makeFunctionPure (flip Luna.toValue primPtrToCArgVal)
                         [ptrT "a"] "Arg"

    let primPtrPlusVal :: Ptr Luna.Data -> Integer -> Ptr Luna.Data
        primPtrPlusVal ptr bytes = ptr `plusPtr` fromIntegral bytes
    primPtrPlus <- makeFunctionPure (flip Luna.toValue primPtrPlusVal)
                       [ptrT "a", "Int"] (ptrT "a")

    let primPtrRetTypeVal :: Luna.Units -> LibFFI.RetType Luna.Data -> LibFFI.RetType Luna.Data
        primPtrRetTypeVal std = fmap (Luna.toData std) . LibFFI.retPtr
    primPtrRetType <- makeFunctionPure (\std -> Luna.toValue std $ primPtrRetTypeVal std)
                          [retTypeT "a"] (retTypeT (ptrT "a"))

    let primPtrByteSizeVal :: Integer
        primPtrByteSizeVal = fromIntegral $ sizeOf (undefined :: Ptr ())
    primPtrByteSize <- makeFunctionPure (flip Luna.toValue primPtrByteSizeVal)
                           [] "Int"

    let primPtrCastVal :: Ptr Luna.Data -> Ptr Luna.Data
        primPtrCastVal = id
    primPtrCast <- makeFunctionPure (flip Luna.toValue primPtrCastVal)
                       [ptrT "a"] (ptrT "b")

    let primPtrReadPtrVal :: Ptr Luna.Data -> IO (Ptr Luna.Data)
        primPtrReadPtrVal p = peek (castPtr p :: Ptr (Ptr Luna.Data))
    primPtrReadPtr <- makeFunctionIO (flip Luna.toValue primPtrReadPtrVal)
                          [ptrT (ptrT "a")] (ptrT "a")

    let primPtrWritePtrVal :: Ptr Luna.Data -> Ptr Luna.Data -> IO ()
        primPtrWritePtrVal p = poke (castPtr p :: Ptr (Ptr Luna.Data))
    primPtrWritePtr <- makeFunctionIO (flip Luna.toValue primPtrWritePtrVal)
                           [ptrT (ptrT "a"), ptrT "a"] "None"

    let primPtrEqVal :: Ptr Luna.Data -> Ptr Luna.Data -> Bool
        primPtrEqVal = (==)
    primPtrEq <- makeFunctionPure (flip Luna.toValue primPtrEqVal)
                     [ptrT "a", ptrT "a"] "Bool"

    primNullPtr <- makeFunctionPure (flip Luna.toValue (nullPtr :: Ptr Luna.Data))
                       [] (ptrT "a")

    let primMallocVal :: Integer -> IO (Ptr Luna.Data)
        primMallocVal = mallocBytes . fromIntegral
    primMalloc <- makeFunctionIO (flip Luna.toValue primMallocVal)
                      ["Int"] (ptrT "a")

    let primFreeVal :: Ptr Luna.Data -> IO ()
        primFreeVal = free
    primFree <- makeFunctionIO (flip Luna.toValue primFreeVal) [ptrT "a"] "None"


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

primCString :: IO (Map IR.Name Def.Def)
primCString = do
    let primCStringFromTextVal :: Text -> IO (Ptr Luna.Data)
        primCStringFromTextVal = fmap castPtr . newCString . convert
    primCStringFromText <- makeFunctionIO
                               (flip Luna.toValue primCStringFromTextVal)
                               ["Text"]
                               (ptrT "CChar")

    let primCStringToTextVal :: Ptr Luna.Data -> IO Text
        primCStringToTextVal = fmap convert . peekCString . castPtr
    primCStringToText <- makeFunctionIO (flip Luna.toValue primCStringToTextVal)
                             [ptrT "CChar"] "Text"

    return $ Map.fromList [ ("primCStringFromText", primCStringFromText)
                          , ("primCStringToText", primCStringToText)
                          ]

class PrimCFFI a where
    retType :: LibFFI.RetType a
    toArg   :: a -> LibFFI.Arg

makePrimFunName :: IR.Name -> IR.Name -> IR.Name
makePrimFunName cls fun = "prim" <> cls <> fun

primStorable :: forall a. ( PrimCFFI a
                          , Storable a
                          , Luna.IsNative a
                          , Luna.ToData a
                          , Luna.ToValue a
                          , Luna.ToData (a -> IO ())
                          , Luna.ToData (a -> Integer)
                          , Luna.ToData (a -> LibFFI.Arg)
                          ) => IO (Map IR.Name Def.Def)
primStorable = do
    let typeName = Luna.classNameOf @a
        tp       = LCons typeName []
    !primToArg   <- makeFunctionPure (flip Luna.toValue (toArg @a)) [tp] "Arg"
    !primRetType <- makeFunctionPure
                        (\std -> Luna.toValue std (Luna.toData std <$> (retType @a)))
                        []
                        (retTypeT tp)

    let primWritePtrVal :: Ptr Luna.Data -> a -> IO ()
        primWritePtrVal p = poke $ coerce p
    !primWritePtr <- makeFunctionIO (flip Luna.toValue primWritePtrVal)
                         [ptrT tp, tp] "None"

    let primReadPtrVal :: Ptr Luna.Data -> IO a
        primReadPtrVal p = peek $ coerce p
    !primReadPtr <- makeFunctionIO (flip Luna.toValue primReadPtrVal) [ptrT tp] tp

    let primByteSizeVal :: Integer
        primByteSizeVal = fromIntegral $ sizeOf (undefined :: a)
    !primByteSize <- makeFunctionIO (flip Luna.toValue primByteSizeVal) [] "Int"

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
                     , Luna.IsNative a
                     , Luna.ToData a
                     , Luna.ToValue a
                     , Luna.ToValue (a -> a)
                     , Luna.ToValue (a -> Text)
                     , Luna.ToValue (a -> a -> a)
                     , Luna.ToValue (a -> a -> Bool)
                     ) => IO (Map IR.Name Def.Def)
primNum = do
    let typeName = Luna.classNameOf @a
        tp       = LCons typeName []

    primPlus <- makeFunctionPure (flip Luna.toValue ((+) :: a -> a -> a))
                    [tp, tp] tp
    primMul  <- makeFunctionPure (flip Luna.toValue ((*) :: a -> a -> a))
                    [tp, tp] tp
    primSub  <- makeFunctionPure (flip Luna.toValue ((-) :: a -> a -> a))
                    [tp, tp] tp

    primToText <- makeFunctionPure
                      (flip Luna.toValue (convert . show :: a -> Text))
                      [tp]
                      "Text"

    primEq <- makeFunctionPure (flip Luna.toValue ((==) :: a -> a -> Bool))
                  [tp, tp] "Bool"
    primLt <- makeFunctionPure (flip Luna.toValue ((<)  :: a -> a -> Bool))
                  [tp, tp] "Bool"
    primGt <- makeFunctionPure (flip Luna.toValue ((>)  :: a -> a -> Bool))
                  [tp, tp] "Bool"

    primNegate <- makeFunctionPure (flip Luna.toValue (negate :: a -> a)) [tp] tp
    primAbs    <- makeFunctionPure (flip Luna.toValue (abs    :: a -> a)) [tp] tp

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
                          , Luna.IsNative a
                          , Luna.ToData a
                          , Luna.ToValue a
                          , Luna.ToValue (a -> Integer)
                          , Luna.ToValue (a -> a -> a)
                          ) => IO (Map IR.Name Def.Def)
primIntegral = do
    let typeName = Luna.classNameOf @a
        tp       = LCons typeName []

    primToInt   <- makeFunctionPure
                       (flip Luna.toValue (fromIntegral :: a -> Integer))
                       [tp]
                       "Int"
    primFromInt <- makeFunctionPure
                       (flip Luna.toValue (fromIntegral :: Integer -> a))
                       ["Int"]
                       tp

    primDiv <- makeFunctionPure (flip Luna.toValue (div :: a -> a -> a))
                   [tp, tp] tp
    primMod <- makeFunctionPure (flip Luna.toValue (mod :: a -> a -> a))
                   [tp, tp] tp

    let mkName = makePrimFunName typeName

    return $ Map.fromList [ (mkName "ToInt", primToInt)
                          , (mkName "FromInt", primFromInt)
                          , (mkName "Div", primDiv)
                          , (mkName "Mod", primMod)
                          ]

primFrac :: forall a. ( Real a
                      , Fractional a
                      , Luna.IsNative a
                      , Luna.ToData a
                      , Luna.ToValue a
                      , Luna.ToValue (a -> a -> a)
                      ) => IO (Map IR.Name Def.Def)
primFrac = do
    let typeName = Luna.classNameOf @a
        tp       = LCons typeName []

    primDiv      <- makeFunctionPure
                        (flip Luna.toValue ((/) :: a -> a -> a))
                        [tp, tp]
                        tp
    primFromReal <- makeFunctionPure
                        (flip Luna.toValue (realToFrac :: Double -> a))
                        ["Real"]
                        tp

    let mkName = makePrimFunName typeName

    return $ Map.fromList [ (mkName "Div", primDiv)
                          , (mkName "FromReal", primFromReal)
                          ]

primReal :: forall a. ( Real a
                      , Luna.IsNative a
                      , Luna.ToValue a
                      , Luna.ToValue (a -> Double)
                      ) => IO (Map IR.Name Def.Def)
primReal = do
    let typeName = Luna.classNameOf @a
        tp       = LCons typeName []

    primToReal  <- makeFunctionPure
                       (flip Luna.toValue (realToFrac :: a -> Double))
                       [tp]
                       "Real"
    primFromInt <- makeFunctionPure
                       (flip Luna.toValue (fromIntegral :: Integer -> a))
                       ["Int"]
                       tp

    let mkName = makePrimFunName typeName

    return $ Map.fromList [ (mkName "ToReal", primToReal)
                          , (mkName "FromInt", primFromInt)]

-- FIXME[MM]: remove when GHC 8.2
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr c) (I# d) = ForeignPtr (plusAddr# addr d) c

primForeignPtr :: FinalizersCtx -> IO (Map IR.Name Def.Def)
primForeignPtr finalizersCtx = do
    let primForeignPtrToCArgVal :: ForeignPtr Luna.Data -> LibFFI.Arg
        primForeignPtrToCArgVal fptr =
            LibFFI.argPtr $ unsafeForeignPtrToPtr fptr
    primForeignPtrToCArg <- makeFunctionIO
                                (flip Luna.toValue primForeignPtrToCArgVal)
                                [fPtrT "a"]
                                "Arg"

    let primForeignPtrCastVal :: ForeignPtr Luna.Data -> ForeignPtr Luna.Data
        primForeignPtrCastVal = id
    primForeignPtrCast <- makeFunctionPure
                              (flip Luna.toValue primForeignPtrCastVal)
                              [fPtrT "a"]
                              (fPtrT "b")

    let primForeignPtrEqVal :: ForeignPtr Luna.Data
                            -> ForeignPtr Luna.Data
                            -> Bool
        primForeignPtrEqVal = (==)
    primForeignPtrEq <- makeFunctionPure (flip Luna.toValue primForeignPtrEqVal)
                            [fPtrT "a", fPtrT "a"] "Bool"

    let primNullForeignPtrVal :: IO (ForeignPtr Luna.Data)
        primNullForeignPtrVal = ForeignPtr.newForeignPtr_ nullPtr
    primNullForeignPtr <- makeFunctionIO (flip Luna.toValue primNullForeignPtrVal)
                              [] (fPtrT "a")

    let primForeignPtrPlusVal :: ForeignPtr Luna.Data
                              -> Integer
                              -> ForeignPtr Luna.Data
        primForeignPtrPlusVal fptr i = plusForeignPtr fptr (fromIntegral i)
    primForeignPtrPlus <- makeFunctionPure
                              (flip Luna.toValue primForeignPtrPlusVal)
                              [fPtrT "a", "Int"]
                              (fPtrT "a")

    let primForeignPtrFreeVal :: ForeignPtr Luna.Data -> IO ()
        primForeignPtrFreeVal fptr = ForeignPtr.finalizeForeignPtr fptr
    primForeignPtrFree <- makeFunctionIO (flip Luna.toValue primForeignPtrFreeVal)
                              [fPtrT "a"] "None"

    let attachFinalizer :: ForeignPtr a -> IO ()
        attachFinalizer fptr = do
            weak <- Weak.mkWeakPtr fptr Nothing
            registerFinalizer finalizersCtx $ do
                fptrAlive <- Weak.deRefWeak weak
                case fptrAlive of
                    Just f -> ForeignPtr.finalizeForeignPtr f
                    _      -> return ()
            return ()

    let primNewForeignPtrVal :: FunPtr Luna.Data
                             -> Ptr Luna.Data
                             -> IO (ForeignPtr Luna.Data)
        primNewForeignPtrVal finalizer ptr = do
            fptr <- ForeignPtr.newForeignPtr (castFunPtr finalizer) ptr
            attachFinalizer fptr
            return fptr
    primNewForeignPtr <- makeFunctionIO (flip Luna.toValue primNewForeignPtrVal)
                             ["FunPtr", ptrT "a"] (fPtrT "a")

    let primMallocForeignPtrBytesVal :: Integer -> IO (ForeignPtr Luna.Data)
        primMallocForeignPtrBytesVal bytes = do
            ptr  <- mallocBytes (fromIntegral bytes)
            fptr <- ForeignPtr.newForeignPtr finalizerFree ptr
            attachFinalizer fptr
            return fptr
    primMallocForeignPtrBytes <- makeFunctionIO
        (flip Luna.toValue primMallocForeignPtrBytesVal) ["Int"] (fPtrT "a")

    let primForeignPtrToPtrVal :: ForeignPtr Luna.Data -> Ptr Luna.Data
        primForeignPtrToPtrVal = unsafeForeignPtrToPtr
    primForeignPtrToPtr <- makeFunctionPure
                               (flip Luna.toValue primForeignPtrToPtrVal)
                               [fPtrT "a"]
                               (ptrT "a")

    return $ Map.fromList
        [ ("primForeignPtrToCArg", primForeignPtrToCArg)
        , ("primForeignPtrCast", primForeignPtrCast)
        , ("primForeignPtrEq", primForeignPtrEq)
        , ("primNullForeignPtr", primNullForeignPtr)
        , ("primForeignPtrPlus", primForeignPtrPlus)
        , ("primForeignPtrFree", primForeignPtrFree)
        , ("primNewForeignPtr", primNewForeignPtr)
        , ("primMallocForeignPtrBytes", primMallocForeignPtrBytes)
        , ("primForeignPtrToPtr", primForeignPtrToPtr)
        ]


type instance Luna.RuntimeRepOf CChar = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CChar")
instance PrimCFFI CChar where
    retType = LibFFI.retCChar ; {-# INLINE retType #-}
    toArg   = LibFFI.argCChar ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CUChar = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CUChar")
instance PrimCFFI CUChar where
    retType = LibFFI.retCUChar ; {-# INLINE retType #-}
    toArg   = LibFFI.argCUChar ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CWchar = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CWChar")
instance PrimCFFI CWchar where
    retType = LibFFI.retCWchar ; {-# INLINE retType #-}
    toArg   = LibFFI.argCWchar ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CInt = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CInt")
instance PrimCFFI CInt where
    retType = LibFFI.retCInt ; {-# INLINE retType #-}
    toArg   = LibFFI.argCInt ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CInt8 = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CInt8")
instance PrimCFFI CInt8 where
    retType = fmap coerce LibFFI.retInt8 ; {-# INLINE retType #-}
    toArg   = LibFFI.argInt8 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CInt16 = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CInt16")
instance PrimCFFI CInt16 where
    retType = fmap coerce LibFFI.retInt16 ; {-# INLINE retType #-}
    toArg   = LibFFI.argInt16 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CInt32 = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CInt32")
instance PrimCFFI CInt32 where
    retType = fmap coerce LibFFI.retInt32 ; {-# INLINE retType #-}
    toArg   = LibFFI.argInt32 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CInt64 = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CInt64")
instance PrimCFFI CInt64 where
    retType = fmap coerce LibFFI.retInt64 ; {-# INLINE retType #-}
    toArg   = LibFFI.argInt64 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CUInt = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CUInt")
instance PrimCFFI CUInt where
    retType = LibFFI.retCUInt ; {-# INLINE retType #-}
    toArg   = LibFFI.argCUInt ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CUInt8 = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CUInt8")
instance PrimCFFI CUInt8 where
    retType = fmap coerce LibFFI.retWord8 ; {-# INLINE retType #-}
    toArg   = LibFFI.argWord8  . coerce   ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CUInt16 = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CUInt16")
instance PrimCFFI CUInt16 where
    retType = fmap coerce LibFFI.retWord16 ; {-# INLINE retType #-}
    toArg   = LibFFI.argWord16 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CUInt32 = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CUInt32")
instance PrimCFFI CUInt32 where
    retType = fmap coerce LibFFI.retWord32 ; {-# INLINE retType #-}
    toArg   = LibFFI.argWord32 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CUInt64 = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CUInt64")
instance PrimCFFI CUInt64 where
    retType = fmap coerce LibFFI.retWord64 ; {-# INLINE retType #-}
    toArg   = LibFFI.argWord64 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CLong = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CLong")
instance PrimCFFI CLong where
    retType = LibFFI.retCLong ; {-# INLINE retType #-}
    toArg   = LibFFI.argCLong ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CULong = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CULong")
instance PrimCFFI CULong where
    retType = LibFFI.retCULong ; {-# INLINE retType #-}
    toArg   = LibFFI.argCULong ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CSize = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CSize")
instance PrimCFFI CSize where
    retType = LibFFI.retCSize ; {-# INLINE retType #-}
    toArg   = LibFFI.argCSize ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CTime = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CTime")
instance PrimCFFI CTime where
    retType = LibFFI.retCTime ; {-# INLINE retType #-}
    toArg   = LibFFI.argCTime ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CFloat = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CFloat")
instance PrimCFFI CFloat where
    retType = LibFFI.retCFloat ; {-# INLINE retType #-}
    toArg   = LibFFI.argCFloat ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CDouble = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "CDouble")
instance PrimCFFI CDouble where
    retType = LibFFI.retCDouble ; {-# INLINE retType #-}
    toArg   = LibFFI.argCDouble ; {-# INLINE toArg   #-}


type instance Luna.RuntimeRepOf LibFFI.Arg                = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "Arg")
type instance Luna.RuntimeRepOf (LibFFI.RetType Luna.Data) = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "RetType")
type instance Luna.RuntimeRepOf (FunPtr         Luna.Data) = Luna.AsNative ('Luna.ClassRep "Std.Foreign" "FunPtr")
type instance Luna.RuntimeRepOf (Ptr            Luna.Data) = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "Ptr")
type instance Luna.RuntimeRepOf (ForeignPtr     Luna.Data) = Luna.AsNative ('Luna.ClassRep "Std.Foreign.C.Value" "ForeignPtr")
