{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.Foreign where

import Prologue

import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import qualified Foreign.ForeignPtr          as ForeignPtr
import qualified Foreign.LibFFI              as LibFFI
import qualified Luna.IR                     as IR
import qualified Luna.Pass.Sourcing.Data.Def as Def
import qualified Luna.Prim.DynamicLinker     as Linker
import qualified Luna.Runtime                as Luna
import qualified Luna.Std.Builder            as Builder
import qualified OCI.Data.Name               as Name
import qualified System.Mem.Weak             as Weak

import Control.Exception.Safe    (handleAny, throwString)
import Data.Map                  (Map)
import Foreign.C.String          (CString, newCString, peekCString)
import Foreign.C.Types           (CChar, CDouble, CFloat, CInt, CLong, CSize,
                                  CTime, CUChar, CUInt, CULong, CWchar)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc     (finalizerFree, free, mallocBytes)
import Foreign.Ptr               (FunPtr, Ptr, castFunPtr, castPtr,
                                  castPtrToFunPtr, nullPtr, plusPtr)
import Foreign.Storable          (Storable (..))
import GHC.Exts                  (Int (..), plusAddr#)
import GHC.ForeignPtr            (ForeignPtr (..))
import Luna.Prim.CTypes          (CInt16 (..), CInt32 (..), CInt64 (..),
                                  CInt8 (..), CUInt16 (..), CUInt32 (..),
                                  CUInt64 (..), CUInt8 (..))
import Luna.Std.Builder          (LTp (..), int, integer, makeFunctionIO,
                                  makeFunctionPure, maybeLT, real)
import Luna.Std.Finalizers       (FinalizersCtx, registerFinalizer)

type ForeignValModule = "Std.Foreign.C.Value"

foreignValModule :: Name.Qualified
foreignValModule = Name.qualFromSymbol @ForeignValModule

type ForeignModule = "Std.Foreign"

foreignModule :: Name.Qualified
foreignModule = Name.qualFromSymbol @ForeignModule


ptrT :: LTp -> LTp
ptrT = LCons foreignValModule "Ptr" . pure

fPtrT :: LTp -> LTp
fPtrT = LCons foreignValModule "ForeignPtr" . pure

retTypeT :: LTp -> LTp
retTypeT = LCons foreignValModule "RetType" . pure

argT :: LTp
argT = LCons foreignValModule "Arg" []

ccharT :: LTp
ccharT = LCons foreignValModule "CChar" []

funPtrT :: LTp
funPtrT = LCons foreignModule "FunPtr" []


primIntSeq :: ∀ graph a m.
    ( Builder.StdBuilder graph m
    , PrimStorable a
    , PrimNum a
    , PrimIntegral a
    ) => [m (Map IR.Name Def.Def)]
primIntSeq  = [primStorable @graph @a, primNum @graph @a, primIntegral @graph @a]

primRealSeq :: ∀ graph a m.
    ( Builder.StdBuilder graph m
    , PrimStorable a
    , PrimNum a
    , PrimReal a
    ) => [m (Map IR.Name Def.Def)]
primRealSeq = [primStorable @graph @a, primNum @graph @a, primReal @graph @a]

primFracSeq :: ∀ graph a m.
    ( Builder.StdBuilder graph m
    , PrimStorable a
    , PrimNum a
    , PrimReal a
    , PrimFrac a
    ) => [m (Map IR.Name Def.Def)]
primFracSeq = [ primStorable @graph @a
              , primNum      @graph @a
              , primReal     @graph @a
              , primFrac     @graph @a
              ]

exports :: forall graph m.
    ( Builder.StdBuilder graph m
    ) => FinalizersCtx -> m (Map IR.Name Def.Def)
exports finalizersCtx = do
    let primLookupSymbolVal :: Text -> Text -> IO (FunPtr Luna.Data)
        primLookupSymbolVal (convert -> dll) (convert -> symbol) = do
            dl       <- Linker.loadLibrary dll
            sym      <- Linker.loadSymbol dl symbol
            return sym
    primLookupSymbol <- makeFunctionIO @graph (flip Luna.toValue primLookupSymbolVal)
                            [Builder.textLT, Builder.textLT] funPtrT

    let primCallFunPtrVal :: FunPtr Luna.Data -> LibFFI.RetType Luna.Data
                          -> [LibFFI.Arg] -> IO Luna.Data
        primCallFunPtrVal = LibFFI.callFFI
    primCallFunPtr <- makeFunctionIO @graph (flip Luna.toValue primCallFunPtrVal)
                          [funPtrT, retTypeT "a", Builder.listLT argT] "a"


    let primVoidRetTypeVal :: Luna.Units -> LibFFI.RetType Luna.Data
        primVoidRetTypeVal std = fmap (Luna.toData std) $ LibFFI.retVoid
    primVoidRetType <- makeFunctionPure @graph (\std -> Luna.toValue std $ primVoidRetTypeVal std)
                           [] (retTypeT Builder.noneLT)

    let local = Map.fromList [ ("primLookupSymbol", primLookupSymbol)
                             , ("primCallFunPtr", primCallFunPtr)
                             , ("primVoidRetType", primVoidRetType)
                             ]

    ptr     <- primPtr        @graph
    fptr    <- primForeignPtr @graph finalizersCtx
    cstring <- primCString    @graph

    cchar   <- Map.unions <$> sequence (primIntSeq  @graph @CChar)
    cuchar  <- Map.unions <$> sequence (primIntSeq  @graph @CUChar)
    cwchar  <- Map.unions <$> sequence (primIntSeq  @graph @CWchar)
    cint    <- Map.unions <$> sequence (primIntSeq  @graph @CInt)
    cint8   <- Map.unions <$> sequence (primIntSeq  @graph @CInt8)
    cint16  <- Map.unions <$> sequence (primIntSeq  @graph @CInt16)
    cint32  <- Map.unions <$> sequence (primIntSeq  @graph @CInt32)
    cint64  <- Map.unions <$> sequence (primIntSeq  @graph @CInt64)
    cuint   <- Map.unions <$> sequence (primIntSeq  @graph @CUInt)
    cuint8  <- Map.unions <$> sequence (primIntSeq  @graph @CUInt8)
    cuint16 <- Map.unions <$> sequence (primIntSeq  @graph @CUInt16)
    cuint32 <- Map.unions <$> sequence (primIntSeq  @graph @CUInt32)
    cuint64 <- Map.unions <$> sequence (primIntSeq  @graph @CUInt64)
    clong   <- Map.unions <$> sequence (primIntSeq  @graph @CLong)
    culong  <- Map.unions <$> sequence (primIntSeq  @graph @CULong)
    csize   <- Map.unions <$> sequence (primIntSeq  @graph @CSize)
    ctime   <- Map.unions <$> sequence (primRealSeq @graph @CTime)
    cdouble <- Map.unions <$> sequence (primFracSeq @graph @CDouble)
    cfloat  <- Map.unions <$> sequence (primFracSeq @graph @CFloat)

    return $ Map.unions
        [ local, ptr, fptr, cstring, cchar, cuchar, cwchar, cint, cuint, clong
        , culong, cint8, cint16, cint32, cint64, cuint8, cuint16, cuint32
        , cuint64, csize, ctime, cdouble, cfloat
        ]

primPtr :: forall graph m.
    ( Builder.StdBuilder graph m
    ) => m (Map IR.Name Def.Def)
primPtr = do
    let primPtrToCArgVal :: Ptr Luna.Data -> LibFFI.Arg
        primPtrToCArgVal ptr = LibFFI.argPtr ptr
    primPtrToCArg <- makeFunctionPure @graph (flip Luna.toValue primPtrToCArgVal)
                         [ptrT "a"] argT

    let primPtrPlusVal :: Ptr Luna.Data -> Integer -> Ptr Luna.Data
        primPtrPlusVal ptr bytes = ptr `plusPtr` fromIntegral bytes
    primPtrPlus <- makeFunctionPure @graph (flip Luna.toValue primPtrPlusVal)
                       [ptrT "a", Builder.intLT] (ptrT "a")

    let primPtrRetTypeVal :: Luna.Units -> LibFFI.RetType Luna.Data -> LibFFI.RetType Luna.Data
        primPtrRetTypeVal std = fmap (Luna.toData std) . LibFFI.retPtr
    primPtrRetType <- makeFunctionPure @graph (\std -> Luna.toValue std $ primPtrRetTypeVal std)
                          [retTypeT "a"] (retTypeT (ptrT "a"))

    let primPtrByteSizeVal :: Integer
        primPtrByteSizeVal = fromIntegral $ sizeOf (undefined :: Ptr ())
    primPtrByteSize <- makeFunctionPure @graph (flip Luna.toValue primPtrByteSizeVal)
                           [] Builder.intLT

    let primPtrCastVal :: Ptr Luna.Data -> Ptr Luna.Data
        primPtrCastVal = id
    primPtrCast <- makeFunctionPure @graph (flip Luna.toValue primPtrCastVal)
                       [ptrT "a"] (ptrT "b")

    let primPtrReadPtrVal :: Ptr Luna.Data -> IO (Ptr Luna.Data)
        primPtrReadPtrVal p = peek (castPtr p :: Ptr (Ptr Luna.Data))
    primPtrReadPtr <- makeFunctionIO @graph (flip Luna.toValue primPtrReadPtrVal)
                          [ptrT (ptrT "a")] (ptrT "a")

    let primPtrWritePtrVal :: Ptr Luna.Data -> Ptr Luna.Data -> IO ()
        primPtrWritePtrVal p = poke (castPtr p :: Ptr (Ptr Luna.Data))
    primPtrWritePtr <- makeFunctionIO @graph (flip Luna.toValue primPtrWritePtrVal)
                           [ptrT (ptrT "a"), ptrT "a"] Builder.noneLT

    let primPtrEqVal :: Ptr Luna.Data -> Ptr Luna.Data -> Bool
        primPtrEqVal = (==)
    primPtrEq <- makeFunctionPure @graph (flip Luna.toValue primPtrEqVal)
                     [ptrT "a", ptrT "a"] Builder.boolLT

    primNullPtr <- makeFunctionPure @graph (flip Luna.toValue (nullPtr :: Ptr Luna.Data))
                       [] (ptrT "a")

    let primMallocVal :: Integer -> IO (Ptr Luna.Data)
        primMallocVal = mallocBytes . fromIntegral
    primMalloc <- makeFunctionIO @graph (flip Luna.toValue primMallocVal)
                      [Builder.intLT] (ptrT "a")

    let primFreeVal :: Ptr Luna.Data -> IO ()
        primFreeVal = free
    primFree <- makeFunctionIO @graph (flip Luna.toValue primFreeVal) [ptrT "a"] Builder.noneLT


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

primCString :: forall graph m.
    ( Builder.StdBuilder graph m
    ) => m (Map IR.Name Def.Def)
primCString = do
    let primCStringFromTextVal :: Text -> IO (Ptr Luna.Data)
        primCStringFromTextVal = fmap castPtr . newCString . convert
    primCStringFromText <- makeFunctionIO @graph
                               (flip Luna.toValue primCStringFromTextVal)
                               [Builder.textLT]
                               (ptrT ccharT)

    let primCStringToTextVal :: Ptr Luna.Data -> IO Text
        primCStringToTextVal = fmap convert . peekCString . castPtr
    primCStringToText <- makeFunctionIO @graph (flip Luna.toValue primCStringToTextVal)
                             [ptrT ccharT] Builder.textLT

    return $ Map.fromList [ ("primCStringFromText", primCStringFromText)
                          , ("primCStringToText", primCStringToText)
                          ]

class PrimCFFI a where
    retType :: LibFFI.RetType a
    toArg   :: a -> LibFFI.Arg

makePrimFunName :: IR.Name -> IR.Name -> IR.Name
makePrimFunName cls fun = "prim" <> cls <> fun

type PrimStorable a =
    ( PrimCFFI a
    , Storable a
    , Luna.IsNative a
    , Luna.ToData a
    , Luna.ToValue a
    , Luna.ToData (a -> IO ())
    , Luna.ToData (a -> Integer)
    , Luna.ToData (a -> LibFFI.Arg)
    )

primStorable :: forall graph a m.
    ( Builder.StdBuilder graph m
    , PrimStorable a
    ) => m (Map IR.Name Def.Def)
primStorable = do
    let typeName = Luna.classNameOf @a
        modName  = Luna.moduleNameOf   @a
        tp       = LCons modName typeName []
    !primToArg   <- makeFunctionPure @graph (flip Luna.toValue (toArg @a)) [tp] argT
    !primRetType <- makeFunctionPure @graph
                        (\std -> Luna.toValue std (Luna.toData std <$> (retType @a)))
                        []
                        (retTypeT tp)

    let primWritePtrVal :: Ptr Luna.Data -> a -> IO ()
        primWritePtrVal p = poke $ coerce p
    !primWritePtr <- makeFunctionIO @graph (flip Luna.toValue primWritePtrVal)
                         [ptrT tp, tp] Builder.noneLT

    let primReadPtrVal :: Ptr Luna.Data -> IO a
        primReadPtrVal p = peek $ coerce p
    !primReadPtr <- makeFunctionIO @graph (flip Luna.toValue primReadPtrVal) [ptrT tp] tp

    let primByteSizeVal :: Integer
        primByteSizeVal = fromIntegral $ sizeOf (undefined :: a)
    !primByteSize <- makeFunctionIO @graph (flip Luna.toValue primByteSizeVal) [] Builder.intLT

    let mkName = makePrimFunName typeName

    return $! Map.fromList [ (mkName "ToArg",    primToArg)
                           , (mkName "RetType",  primRetType)
                           , (mkName "WritePtr", primWritePtr)
                           , (mkName "ReadPtr",  primReadPtr)
                           , (mkName "ByteSize", primByteSize)
                           ]

type PrimNum a =
    ( Num a
    , Ord a
    , Show a
    , Luna.IsNative a
    , Luna.ToData a
    , Luna.ToValue a
    , Luna.ToValue (a -> a)
    , Luna.ToValue (a -> Text)
    , Luna.ToValue (a -> a -> a)
    , Luna.ToValue (a -> a -> Bool)
    )

primNum :: forall graph a m. 
    ( Builder.StdBuilder graph m
    , PrimNum a
    ) => m (Map IR.Name Def.Def)
primNum = do
    let typeName = Luna.classNameOf @a
        modName  = Luna.moduleNameOf   @a
        tp       = LCons modName typeName []

    primPlus <- makeFunctionPure @graph (flip Luna.toValue ((+) :: a -> a -> a))
                    [tp, tp] tp
    primMul  <- makeFunctionPure @graph (flip Luna.toValue ((*) :: a -> a -> a))
                    [tp, tp] tp
    primSub  <- makeFunctionPure @graph (flip Luna.toValue ((-) :: a -> a -> a))
                    [tp, tp] tp

    primToText <- makeFunctionPure @graph
                      (flip Luna.toValue (convert . show :: a -> Text))
                      [tp]
                      Builder.textLT

    primEq <- makeFunctionPure @graph (flip Luna.toValue ((==) :: a -> a -> Bool))
                  [tp, tp] Builder.boolLT
    primLt <- makeFunctionPure @graph (flip Luna.toValue ((<)  :: a -> a -> Bool))
                  [tp, tp] Builder.boolLT
    primGt <- makeFunctionPure @graph (flip Luna.toValue ((>)  :: a -> a -> Bool))
                  [tp, tp] Builder.boolLT

    primNegate <- makeFunctionPure @graph (flip Luna.toValue (negate :: a -> a)) [tp] tp
    primAbs    <- makeFunctionPure @graph (flip Luna.toValue (abs    :: a -> a)) [tp] tp

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

type PrimIntegral a =
    ( Integral a
    , Luna.IsNative a
    , Luna.ToData a
    , Luna.ToValue a
    , Luna.ToValue (a -> Integer)
    , Luna.ToValue (a -> a -> a)
    )

primIntegral :: forall graph a m.
    ( Builder.StdBuilder graph m
    , PrimIntegral a
    ) => m (Map IR.Name Def.Def)
primIntegral = do
    let typeName = Luna.classNameOf @a
        modName  = Luna.moduleNameOf   @a
        tp       = LCons modName typeName []

    primToInt   <- makeFunctionPure @graph
                       (flip Luna.toValue (fromIntegral :: a -> Integer))
                       [tp]
                       Builder.intLT
    primFromInt <- makeFunctionPure @graph
                       (flip Luna.toValue (fromIntegral :: Integer -> a))
                       [Builder.intLT]
                       tp

    primDiv <- makeFunctionPure @graph (flip Luna.toValue (div :: a -> a -> a))
                   [tp, tp] tp
    primMod <- makeFunctionPure @graph (flip Luna.toValue (mod :: a -> a -> a))
                   [tp, tp] tp

    let mkName = makePrimFunName typeName

    return $ Map.fromList [ (mkName "ToInt", primToInt)
                          , (mkName "FromInt", primFromInt)
                          , (mkName "Div", primDiv)
                          , (mkName "Mod", primMod)
                          ]

type PrimFrac a =
    ( Real a
    , Fractional a
    , Luna.IsNative a
    , Luna.ToData a
    , Luna.ToValue a
    , Luna.ToValue (a -> a -> a)
    )

primFrac :: forall graph a m.
    ( Builder.StdBuilder graph m
    , PrimFrac a
    ) => m (Map IR.Name Def.Def)
primFrac = do
    let typeName = Luna.classNameOf @a
        modName  = Luna.moduleNameOf   @a
        tp       = LCons modName typeName []

    primDiv      <- makeFunctionPure @graph
                        (flip Luna.toValue ((/) :: a -> a -> a))
                        [tp, tp]
                        tp
    primFromReal <- makeFunctionPure @graph
                        (flip Luna.toValue (realToFrac :: Double -> a))
                        [Builder.realLT]
                        tp

    let mkName = makePrimFunName typeName

    return $ Map.fromList [ (mkName "Div", primDiv)
                          , (mkName "FromReal", primFromReal)
                          ]

type PrimReal a =
    ( Real a
    , Luna.IsNative a
    , Luna.ToValue a
    , Luna.ToValue (a -> Double)
    )

primReal :: forall graph a m.
    ( Builder.StdBuilder graph m
    , PrimReal a
    ) => m (Map IR.Name Def.Def)
primReal = do
    let typeName = Luna.classNameOf @a
        modName  = Luna.moduleNameOf   @a
        tp       = LCons modName typeName []

    primToReal  <- makeFunctionPure @graph
                       (flip Luna.toValue (realToFrac :: a -> Double))
                       [tp]
                       Builder.realLT
    primFromInt <- makeFunctionPure @graph
                       (flip Luna.toValue (fromIntegral :: Integer -> a))
                       [Builder.intLT]
                       tp

    let mkName = makePrimFunName typeName

    return $ Map.fromList [ (mkName "ToReal", primToReal)
                          , (mkName "FromInt", primFromInt)]

-- FIXME[MM]: remove when GHC 8.2
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr c) (I# d) = ForeignPtr (plusAddr# addr d) c

primForeignPtr :: forall graph m.
    ( Builder.StdBuilder graph m
    ) => FinalizersCtx -> m (Map IR.Name Def.Def)
primForeignPtr finalizersCtx = do
    let primForeignPtrToCArgVal :: ForeignPtr Luna.Data -> LibFFI.Arg
        primForeignPtrToCArgVal fptr =
            LibFFI.argPtr $ unsafeForeignPtrToPtr fptr
    primForeignPtrToCArg <- makeFunctionIO @graph
                                (flip Luna.toValue primForeignPtrToCArgVal)
                                [fPtrT "a"]
                                argT

    let primForeignPtrCastVal :: ForeignPtr Luna.Data -> ForeignPtr Luna.Data
        primForeignPtrCastVal = id
    primForeignPtrCast <- makeFunctionPure @graph
                              (flip Luna.toValue primForeignPtrCastVal)
                              [fPtrT "a"]
                              (fPtrT "b")

    let primForeignPtrEqVal :: ForeignPtr Luna.Data
                            -> ForeignPtr Luna.Data
                            -> Bool
        primForeignPtrEqVal = (==)
    primForeignPtrEq <- makeFunctionPure @graph (flip Luna.toValue primForeignPtrEqVal)
                            [fPtrT "a", fPtrT "a"] Builder.boolLT

    let primNullForeignPtrVal :: IO (ForeignPtr Luna.Data)
        primNullForeignPtrVal = ForeignPtr.newForeignPtr_ nullPtr
    primNullForeignPtr <- makeFunctionIO @graph (flip Luna.toValue primNullForeignPtrVal)
                              [] (fPtrT "a")

    let primForeignPtrPlusVal :: ForeignPtr Luna.Data
                              -> Integer
                              -> ForeignPtr Luna.Data
        primForeignPtrPlusVal fptr i = plusForeignPtr fptr (fromIntegral i)
    primForeignPtrPlus <- makeFunctionPure @graph
                              (flip Luna.toValue primForeignPtrPlusVal)
                              [fPtrT "a", Builder.intLT]
                              (fPtrT "a")

    let primForeignPtrFreeVal :: ForeignPtr Luna.Data -> IO ()
        primForeignPtrFreeVal fptr = ForeignPtr.finalizeForeignPtr fptr
    primForeignPtrFree <- makeFunctionIO @graph (flip Luna.toValue primForeignPtrFreeVal)
                              [fPtrT "a"] Builder.noneLT

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
    primNewForeignPtr <- makeFunctionIO @graph (flip Luna.toValue primNewForeignPtrVal)
                             [funPtrT, ptrT "a"] (fPtrT "a")

    let primMallocForeignPtrBytesVal :: Integer -> IO (ForeignPtr Luna.Data)
        primMallocForeignPtrBytesVal bytes = do
            ptr  <- mallocBytes (fromIntegral bytes)
            fptr <- ForeignPtr.newForeignPtr finalizerFree ptr
            attachFinalizer fptr
            return fptr
    primMallocForeignPtrBytes <- makeFunctionIO @graph
        (flip Luna.toValue primMallocForeignPtrBytesVal) [Builder.intLT] (fPtrT "a")

    let primForeignPtrToPtrVal :: ForeignPtr Luna.Data -> Ptr Luna.Data
        primForeignPtrToPtrVal = unsafeForeignPtrToPtr
    primForeignPtrToPtr <- makeFunctionPure @graph
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


type instance Luna.RuntimeRepOf CChar = Luna.AsNative ('Luna.ClassRep ForeignValModule "CChar")
instance PrimCFFI CChar where
    retType = LibFFI.retCChar ; {-# INLINE retType #-}
    toArg   = LibFFI.argCChar ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CUChar = Luna.AsNative ('Luna.ClassRep ForeignValModule "CUChar")
instance PrimCFFI CUChar where
    retType = LibFFI.retCUChar ; {-# INLINE retType #-}
    toArg   = LibFFI.argCUChar ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CWchar = Luna.AsNative ('Luna.ClassRep ForeignValModule "CWChar")
instance PrimCFFI CWchar where
    retType = LibFFI.retCWchar ; {-# INLINE retType #-}
    toArg   = LibFFI.argCWchar ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CInt = Luna.AsNative ('Luna.ClassRep ForeignValModule "CInt")
instance PrimCFFI CInt where
    retType = LibFFI.retCInt ; {-# INLINE retType #-}
    toArg   = LibFFI.argCInt ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CInt8 = Luna.AsNative ('Luna.ClassRep ForeignValModule "CInt8")
instance PrimCFFI CInt8 where
    retType = fmap coerce LibFFI.retInt8 ; {-# INLINE retType #-}
    toArg   = LibFFI.argInt8 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CInt16 = Luna.AsNative ('Luna.ClassRep ForeignValModule "CInt16")
instance PrimCFFI CInt16 where
    retType = fmap coerce LibFFI.retInt16 ; {-# INLINE retType #-}
    toArg   = LibFFI.argInt16 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CInt32 = Luna.AsNative ('Luna.ClassRep ForeignValModule "CInt32")
instance PrimCFFI CInt32 where
    retType = fmap coerce LibFFI.retInt32 ; {-# INLINE retType #-}
    toArg   = LibFFI.argInt32 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CInt64 = Luna.AsNative ('Luna.ClassRep ForeignValModule "CInt64")
instance PrimCFFI CInt64 where
    retType = fmap coerce LibFFI.retInt64 ; {-# INLINE retType #-}
    toArg   = LibFFI.argInt64 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CUInt = Luna.AsNative ('Luna.ClassRep ForeignValModule "CUInt")
instance PrimCFFI CUInt where
    retType = LibFFI.retCUInt ; {-# INLINE retType #-}
    toArg   = LibFFI.argCUInt ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CUInt8 = Luna.AsNative ('Luna.ClassRep ForeignValModule "CUInt8")
instance PrimCFFI CUInt8 where
    retType = fmap coerce LibFFI.retWord8 ; {-# INLINE retType #-}
    toArg   = LibFFI.argWord8  . coerce   ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CUInt16 = Luna.AsNative ('Luna.ClassRep ForeignValModule "CUInt16")
instance PrimCFFI CUInt16 where
    retType = fmap coerce LibFFI.retWord16 ; {-# INLINE retType #-}
    toArg   = LibFFI.argWord16 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CUInt32 = Luna.AsNative ('Luna.ClassRep ForeignValModule "CUInt32")
instance PrimCFFI CUInt32 where
    retType = fmap coerce LibFFI.retWord32 ; {-# INLINE retType #-}
    toArg   = LibFFI.argWord32 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CUInt64 = Luna.AsNative ('Luna.ClassRep ForeignValModule "CUInt64")
instance PrimCFFI CUInt64 where
    retType = fmap coerce LibFFI.retWord64 ; {-# INLINE retType #-}
    toArg   = LibFFI.argWord64 . coerce    ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CLong = Luna.AsNative ('Luna.ClassRep ForeignValModule "CLong")
instance PrimCFFI CLong where
    retType = LibFFI.retCLong ; {-# INLINE retType #-}
    toArg   = LibFFI.argCLong ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CULong = Luna.AsNative ('Luna.ClassRep ForeignValModule "CULong")
instance PrimCFFI CULong where
    retType = LibFFI.retCULong ; {-# INLINE retType #-}
    toArg   = LibFFI.argCULong ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CSize = Luna.AsNative ('Luna.ClassRep ForeignValModule "CSize")
instance PrimCFFI CSize where
    retType = LibFFI.retCSize ; {-# INLINE retType #-}
    toArg   = LibFFI.argCSize ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CTime = Luna.AsNative ('Luna.ClassRep ForeignValModule "CTime")
instance PrimCFFI CTime where
    retType = LibFFI.retCTime ; {-# INLINE retType #-}
    toArg   = LibFFI.argCTime ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CFloat = Luna.AsNative ('Luna.ClassRep ForeignValModule "CFloat")
instance PrimCFFI CFloat where
    retType = LibFFI.retCFloat ; {-# INLINE retType #-}
    toArg   = LibFFI.argCFloat ; {-# INLINE toArg   #-}

type instance Luna.RuntimeRepOf CDouble = Luna.AsNative ('Luna.ClassRep ForeignValModule "CDouble")
instance PrimCFFI CDouble where
    retType = LibFFI.retCDouble ; {-# INLINE retType #-}
    toArg   = LibFFI.argCDouble ; {-# INLINE toArg   #-}


type instance Luna.RuntimeRepOf LibFFI.Arg                 = Luna.AsNative ('Luna.ClassRep ForeignValModule "Arg")
type instance Luna.RuntimeRepOf (LibFFI.RetType Luna.Data) = Luna.AsNative ('Luna.ClassRep ForeignValModule "RetType")
type instance Luna.RuntimeRepOf (FunPtr         Luna.Data) = Luna.AsNative ('Luna.ClassRep ForeignModule    "FunPtr")
type instance Luna.RuntimeRepOf (Ptr            Luna.Data) = Luna.AsNative ('Luna.ClassRep ForeignValModule "Ptr")
type instance Luna.RuntimeRepOf (ForeignPtr     Luna.Data) = Luna.AsNative ('Luna.ClassRep ForeignValModule "ForeignPtr")
