module Luna.Test.Bench.Test where

import Prologue

import Data.AutoVector.Mutable.Storable as Vector



main :: IO ()
main = do
    print "vvvvvv"
    (v :: Vector Int) <- Vector.new 10
    print =<< Vector.unsafeRead v 0
    print =<< Vector.unsafeRead v 1
    print =<< Vector.unsafeRead v 2
    print =<< Vector.unsafeRead v 3
    Vector.pushBack v 7
    Vector.pushBack v 8
    Vector.pushBack v 9
    print =<< Vector.unsafeRead v 0
    print =<< Vector.unsafeRead v 1
    print =<< Vector.unsafeRead v 2
    print =<< Vector.unsafeRead v 3
    print =<< Vector.readArrayLength v
    print =<< Vector.readArraySize v
    Vector.grow v
    print =<< Vector.unsafeRead v 0
    print =<< Vector.unsafeRead v 1
    print =<< Vector.unsafeRead v 2
    print =<< Vector.unsafeRead v 3
    print =<< Vector.readArrayLength v
    print =<< Vector.readArraySize v
    Vector.free v
    print "^^^^^^"
