{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Syntax.Text.Parser.Data.Name.Spaced where

import Prologue

import qualified Language.Symbol.Operator.Assoc as Assoc
import qualified Language.Symbol.Operator.Prec  as Prec

import Control.Monad.State.Layered (StateT)
import Luna.Syntax.Text.Scope      (Scope)
import OCI.Data.Name               (Name)


------------------------
-- === SpacedName === --
------------------------

-- === Definition === --

data SpacedName = SpacedName
    { _spacing :: !Spacing
    , _rawName :: !Name
    } deriving (Show)

data Spacing
    = Spaced
    | LSpaced
    | RSpaced
    | Unspaced
    deriving (Show)

makeLenses ''SpacedName


-- === Construction === --

spaced :: Name -> SpacedName
spaced = SpacedName Spaced ; {-# INLINE spaced#-}

lspaced :: Name -> SpacedName
lspaced = SpacedName LSpaced ; {-# INLINE lspaced #-}

rspaced :: Name -> SpacedName
rspaced = SpacedName RSpaced ; {-# INLINE rspaced #-}

unspaced :: Name -> SpacedName
unspaced = SpacedName Unspaced ; {-# INLINE unspaced #-}

spacedIf :: Bool -> Name -> SpacedName
spacedIf b = SpacedName $ if b then Spaced else Unspaced ; {-# INLINE spacedIf #-}

anyLeftSpacing :: Spacing -> Spacing
anyLeftSpacing = \case
    Spaced  -> Spaced
    LSpaced -> Spaced
    _       -> Unspaced
{-# INLINE anyLeftSpacing #-}

anyRightSpacing :: Spacing -> Spacing
anyRightSpacing = \case
    Spaced  -> Spaced
    RSpaced -> Spaced
    _       -> Unspaced
{-# INLINE anyRightSpacing #-}

anyLeftSpaced :: SpacedName -> SpacedName
anyLeftSpaced = spacing %~ anyLeftSpacing ; {-# INLINE anyLeftSpaced #-}

anyRightSpaced :: SpacedName -> SpacedName
anyRightSpaced = spacing %~ anyRightSpacing ; {-# INLINE anyRightSpaced #-}


-- === Instances === --

-- Precedence
instance Monad m => Prec.RelReader SpacedName (StateT Scope m) where
    readRelLabel (SpacedName sa a) (SpacedName sb b) = case (sa, sb) of
         (Spaced  , Spaced  ) -> Prec.readRel a b
         (Unspaced, Unspaced) -> Prec.readRel a b
         (_       , Spaced  ) -> pure GT
         (Spaced, _         ) -> pure LT
         _                    -> Prec.readRel a b
    {-# INLINE readRelLabel #-}

-- Association
instance Monad m => Assoc.Reader SpacedName (StateT Scope m) where
    readLabel (SpacedName _ n) = Assoc.readLabel n ; {-# INLINE readLabel #-}

