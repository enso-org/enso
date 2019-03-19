cint = ["CChar", "CSChar", "CUChar", "CShort", "CUShort", "CInt", "CUInt", "CLong",
        "CULong", "CSize", "CWchar", "CLLong", "CULLong"]
cfloat = ["CFloat", "CDouble", "CLDouble"]
hsint = ["Int", "Int8", "Int16", "Int32", "Int64", "Word", "Word8", "Word16", "Word32",
         "Word64"]
hsfloat = ["Double", "Float", "Rational"]

printFP (f, i) = 
    "instance Convertible " ++ f ++ " " ++ i ++ " where \n\
    \    safeConvert = boundedConversion (return . truncate)\n\
    \instance Convertible " ++ i ++ " " ++ f ++ " where \n\
    \    safeConvert = return . fromIntegral\n"

printIntegerF f =
    "instance Convertible " ++ f ++ " Integer where\n\
     \    safeConvert = return . truncate\n\
     \instance Convertible Integer " ++ f ++ " where\n\
     \    safeConvert = return . fromIntegral\n"

printIntegerI i =
    "instance Convertible " ++ i ++ " Integer where\n\
    \    safeConvert = return . fromIntegral\n\
    \instance Convertible Integer " ++ i ++ " where\n\
    \    safeConvert = boundedConversion (return . fromIntegral)\n"

printCharI i =
    "instance Convertible " ++ i ++ " Char where\n\
    \    safeConvert = boundedConversion (return . toEnum . fromIntegral)\n\
    \instance Convertible Char " ++ i ++ " where\n\
    \    safeConvert = boundedConversion (return . fromIntegral . fromEnum)\n"

printFP1 (f1, f2) = 
    "instance Convertible " ++ f1 ++ " " ++ f2 ++ " where\n\
    \    safeConvert = return . realToFrac\n"

printFPFP (f1, f2) = printFP1 (f1, f2) ++ printFP1 (f2, f1)

printInt (i1, i2) =
    "instance Convertible " ++ i1 ++ " " ++ i2 ++ " where\n\
    \    safeConvert = boundedConversion (return . fromIntegral)\n"

printIntInt (i1, i2) = printInt (i1, i2) ++ printInt (i2, i1)

main = do putStrLn "-- Section 1"
          mapM_ (putStrLn . printFP) (concatMap (\x -> map (\y -> (x, y)) hsint) cfloat)
          putStrLn "-- Section 2"
          mapM_ (putStrLn . printFPFP) (concatMap (\x -> map (\y -> (x, y)) hsfloat) cfloat)
          putStrLn "-- Section 3"
          mapM_ (putStrLn . printIntInt) (concatMap (\x -> map (\y -> (x, y)) hsint) cint)
          putStrLn "-- Section 4"
          mapM_ (putStrLn . printInt) . filter (\(a, b) -> a /= b) $ 
                (concatMap (\x -> map (\y -> (x, y)) cint) cint)
          putStrLn "-- Section 5"
          mapM_ (putStrLn . printFP1) . filter (\(a, b) -> a /= b) $
                (concatMap (\x -> map (\y -> (x, y)) cfloat) cfloat)
          putStrLn "-- Section 6"
          mapM_ (putStrLn . printIntegerF) cfloat
          putStrLn "-- Section 7"
          mapM_ (putStrLn . printIntegerI) cint
          putStrLn "-- Section 8o"
          mapM_ (putStrLn . printCharI) cint




          