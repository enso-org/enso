module Example where

add :: Int -> Int -> Int
add = (+)

sub :: Int -> Int -> Int
sub = subtract

exn :: IO Int
exn = ioError $ userError "exn"
