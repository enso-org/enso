{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Error (module Type.Error, module X) where

import GHC.TypeLits as X (ErrorMessage ((:$$:), (:<>:), ShowType), TypeError)
import GHC.TypeLits (ErrorMessage (Text))
import Prelude


-- === ErrMsg === --

type Str = 'Text


-- === Assertions === --
class TypeErrorIf (ok :: Bool) (err :: ErrorMessage)
instance TypeError err
      => TypeErrorIf 'False err
instance TypeErrorIf 'True  err

type TypeAssert ok = TypeErrorIf ok (Str "Assertion failed.")


-- === Formatters === --

type Sentence a     = a :<>: Str "."
type Ticked   a     = Between' "`" a
type Parensed a     = Between "(" ")" a
type Between  l r a = Str l :<>: a :<>: Str r
type Between' s   a = Between s s a
type a :<+>: b      = a :<>: Str " " :<>: b
type Type     a     = Ticked (ShowType a)

