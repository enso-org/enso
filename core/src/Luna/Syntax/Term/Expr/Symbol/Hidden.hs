{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term.Expr.Symbol.Hidden where

import Prelude.Luna hiding (Symbol)

import Luna.Syntax.Term.Expr.Symbol.Class
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)
import Control.Lens.Property
import Luna.Syntax.Term.Expr.Format (Format)

----------------------------
-- === Hidden Symbols === --
----------------------------
-- Symbols with phantomized layout, used to simplify constraints where layout doesnt matter
-- and the type safety is proven to be kept

-- === Definitions === --

newtype HiddenSymbol atom = HiddenSymbol Any
makeWrapped ''HiddenSymbol


-- === Instances === --

type instance Get Atom   (HiddenSymbol atom) = atom
type instance Get Format (HiddenSymbol atom) = Get Format atom
type instance Get Sym    (HiddenSymbol atom) = Any

instance Getter Sym (HiddenSymbol atom) where get = unwrap' ; {-# INLINE get #-}

-- === Utils === --

hideLayout :: Symbol atom layout -> HiddenSymbol atom
hideLayout = wrap' . unsafeCoerce ; {-# INLINE hideLayout #-}

unsafeRevealLayout :: HiddenSymbol atom -> Symbol atom layout
unsafeRevealLayout =  unsafeCoerce . unwrap' ; {-# INLINE unsafeRevealLayout #-}

unsafeHidden :: Iso (Symbol atom layout) (Symbol atom layout') (HiddenSymbol atom) (HiddenSymbol atom)
unsafeHidden = iso hideLayout unsafeRevealLayout ; {-# INLINE unsafeHidden #-}
