module Luna.IR.Function.Argument where

import Prelude.Luna

import Luna.IR.Name.Ident (HasOptIdent, IdentType)

import qualified Luna.IR.Name.Ident      as Ident
import qualified Luna.IR.Name.Ident.Type as IdentType



-- === Definitions === --

data Arg    a = Arg    { __ident_ :: Maybe Ident.Var , __val_  ::       a } deriving (Generic, Show, Read, Eq, Ord, Functor, Foldable, Traversable)
data ArgDef a = ArgDef { __pat_   :: a               , __mval_ :: Maybe a } deriving (Generic, Show, Read, Eq, Ord, Functor, Foldable, Traversable)

makeLenses ''Arg
makeLenses ''ArgDef


-- === Utils === --

arg :: a -> Arg a
arg = Arg Nothing



-- === Instances === --

-- Normal Form
instance NFData a => NFData (Arg    a)
instance NFData a => NFData (ArgDef a)

-- Basic
type instance IdentType   (Arg a) = IdentType.Var
instance      HasOptIdent (Arg a) where optIdent = _ident_

-- Layers
type instance Unlayered (Arg a) = a
instance      Layered   (Arg a) where layered = lens (\(Arg _ a) -> a) (\(Arg n _ ) a -> Arg n a)

-- Repr
instance Repr s a => Repr s (Arg a) where repr (Arg n a) = "Arg" <+> repr n <+> repr a
