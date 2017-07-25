module Luna.IR.Term.Function where

import Luna.Prelude

import OCI.IR.Class as IR
import OCI.IR.Layer.Class
import Luna.IR.Layer.Succs
import Luna.IR.Layer.Type
import OCI.IR.Layer.Model
import OCI.IR.Name
import OCI.IR.Name.Qualified
import OCI.IR.Term
import Data.Event
import Data.Property
import Data.Families (makeLensedTerm)


-------------------------
-- === ASGFunction === --
-------------------------

-- === Definition === --

data TermASGFunction a = ASGFunction { __name :: !a
                                     , __args :: ![a]
                                     , __body :: !a
                                     } deriving (Functor, Foldable, Traversable)
makeLensedTerm ''TermASGFunction


-- === Instances === --

instance Show (TermASGFunction a) where show _ = "Function"



-- ----------------------
-- -- === Function === --
-- ----------------------
--
-- -- === Definition === --
--
-- newtype TermFunction a = Function { __body :: a } deriving (Functor, Foldable, Traversable)
-- makeLensedTerm ''TermFunction
--
--
-- -- === Instances === --
--
-- instance Show (TermFunction a) where show _ = "Function"



----------------------
-- === FunctionSig === --
----------------------

-- === Definition === --

data TermFunctionSig a = FunctionSig { __name :: !a
                                     , __sig  :: !a
                                     } deriving (Functor, Foldable, Traversable)
makeLensedTerm ''TermFunctionSig


-- === Instances === --

instance Show (TermFunctionSig a) where show _ = "FunctionSig"



-------------------------
-- === ASGFunction === --
-------------------------

-- === Definition === --

data TermASGRootedFunction a = ASGRootedFunction { __name :: !a
                                                 , __body :: !(IR.Rooted SomeExpr)
                                                 } deriving (Functor, Foldable, Traversable)
makeLensedTerm ''TermASGRootedFunction


-- === Instances === --

instance Show (TermASGRootedFunction a) where show _ = "Function"



----------------------
-- === Function === --
----------------------

-- === Definition === --

newtype TermRootedFunction a = RootedFunction { __body :: IR.Rooted SomeExpr } deriving (Functor, Foldable, Traversable)
makeLensedTerm ''TermRootedFunction


-- === Instances === --

instance Show (TermRootedFunction a) where show _ = "RootedFunction"
