{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Data.Reprx ( module Data.Reprx
                 , module X
                 ) where

import Prologue hiding (Repr, repr)
import Data.Monoid
import Data.Text.CodeBuilder
import Data.Text.CodeBuilder     as X (Builder)
import Data.Text.CodeBuilder.Tok as X (Tok)

import Data.Text.CodeBuilder as X ((<+>))
import GHC.Prim (Constraint)

class Repr  s a        where repr  ::       a -> Builder s Tok
class ReprT s (a :: k) where reprT :: Proxy a -> Builder s Tok

--instance {-# OVERLAPPABLE #-} Show a => Repr a where
    --repr = show

--instance {-# OVERLAPPABLE #-} Repr a => Repr [a] where
--    repr lst = "[" <> intercalate ", " (fmap repr lst) <> "]"


--instance {-# OVERLAPPABLE #-} Repr a => Repr (Maybe a) where
--    repr (Just a) = "Just (" <> repr a <> ")"
--    repr Nothing  = "Nothing"

--instance Repr Char where repr = show
--instance Repr Int  where repr = show

--instance (Repr t1, Repr t2) => Repr (t1,t2) where repr (t1, t2) = "(" <> repr t1 <> ", " <> repr t2

type family Reprs s lst :: Constraint where
    Reprs s '[]       = ()
    Reprs s (t ': ts) = (Repr s t, Reprs s ts)

reprStyled s a = renderStr s $ repr a

reprSimple = reprStyled SimpleStyle

showRepr = fromString . show

instance Repr s String where repr = showRepr
instance Repr s Int    where repr = showRepr
instance Repr s Float  where repr = showRepr
instance Repr s Double where repr = showRepr

instance Repr s a => Repr s (Maybe a) where
    repr = \case
        Just  a -> "Just" <+> repr a
        Nothing -> "Nothing"

instance (Repr s l, Repr s r) => Repr s (Either l r) where
    repr = \case
        Left  a -> "Left"  <+> repr a
        Right a -> "Right" <+> repr a

instance {-# OVERLAPPABLE #-} Repr s t => Repr s [t] where repr = bracked . intercalate ", " . fmap repr

instance (Repr s t1, Repr s t2)                                                                              => Repr s (t1,t2)                      where repr (t1,t2)                      = parensed $ intercalate ", " [repr t1, repr t2]
instance (Repr s t1, Repr s t2, Repr s t3)                                                                   => Repr s (t1,t2,t3)                   where repr (t1,t2,t3)                   = parensed $ intercalate ", " [repr t1, repr t2, repr t3]
instance (Repr s t1, Repr s t2, Repr s t3, Repr s t4)                                                        => Repr s (t1,t2,t3,t4)                where repr (t1,t2,t3,t4)                = parensed $ intercalate ", " [repr t1, repr t2, repr t3, repr t4]
instance (Repr s t1, Repr s t2, Repr s t3, Repr s t4, Repr s t5)                                             => Repr s (t1,t2,t3,t4,t5)             where repr (t1,t2,t3,t4,t5)             = parensed $ intercalate ", " [repr t1, repr t2, repr t3, repr t4, repr t5]
instance (Repr s t1, Repr s t2, Repr s t3, Repr s t4, Repr s t5, Repr s t6)                                  => Repr s (t1,t2,t3,t4,t5,t6)          where repr (t1,t2,t3,t4,t5,t6)          = parensed $ intercalate ", " [repr t1, repr t2, repr t3, repr t4, repr t5, repr t6]
instance (Repr s t1, Repr s t2, Repr s t3, Repr s t4, Repr s t5, Repr s t6, Repr s t7)                       => Repr s (t1,t2,t3,t4,t5,t6,t7)       where repr (t1,t2,t3,t4,t5,t6,t7)       = parensed $ intercalate ", " [repr t1, repr t2, repr t3, repr t4, repr t5, repr t6, repr t7]
instance (Repr s t1, Repr s t2, Repr s t3, Repr s t4, Repr s t5, Repr s t6, Repr s t7, Repr s t8)            => Repr s (t1,t2,t3,t4,t5,t6,t7,t8)    where repr (t1,t2,t3,t4,t5,t6,t7,t8)    = parensed $ intercalate ", " [repr t1, repr t2, repr t3, repr t4, repr t5, repr t6, repr t7, repr t8]
instance (Repr s t1, Repr s t2, Repr s t3, Repr s t4, Repr s t5, Repr s t6, Repr s t7, Repr s t8, Repr s t9) => Repr s (t1,t2,t3,t4,t5,t6,t7,t8,t9) where repr (t1,t2,t3,t4,t5,t6,t7,t8,t9) = parensed $ intercalate ", " [repr t1, repr t2, repr t3, repr t4, repr t5, repr t6, repr t7, repr t8, repr t9]







data LstRpr a

instance ReprT s () where reprT _ = "()"

instance {-# OVERLAPPABLE #-}  ReprT s (LstRpr lst) => ReprT s (lst :: [k]) where reprT _ = "[" <> reprT (Proxy :: Proxy (LstRpr lst)) <> "]"
instance {-# OVERLAPPABLE #-}  KnownNat n           => ReprT s (n :: Nat)   where reprT _ = fromString $ show $ natVal (Proxy :: Proxy n)

instance {-# OVERLAPPABLE #-}                                     ReprT s (LstRpr '[])        where reprT _ = ""
instance {-# OVERLAPPABLE #-} (ReprT s l, ReprT s (LstRpr ls)) => ReprT s (LstRpr (l ': ls))  where reprT _ = reprT (Proxy :: Proxy l) <> ", " <> reprT (Proxy :: Proxy (LstRpr ls))
instance {-# OVERLAPPABLE #-}  ReprT s l                       => ReprT s (LstRpr (l ': '[])) where reprT _ = reprT (Proxy :: Proxy l)

