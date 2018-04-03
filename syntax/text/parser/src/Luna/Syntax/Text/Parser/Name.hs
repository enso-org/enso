{-# LANGUAGE OverloadedStrings #-}

module Luna.Syntax.Text.Parser.Name where

import Luna.Prelude
import OCI.IR.Name
import qualified Language.Symbol.Operator.Assoc as Assoc
import qualified Language.Symbol.Operator.Prec  as Prec
import Luna.Syntax.Text.Scope
import Control.Monad.State.Dependent


------------------------
-- === SpacedName === --
------------------------

-- === Definition === --

data SpacedName = SpacedName { _spacing :: !Spacing
                             , _rawName :: !Name
                             } deriving (Show)

data Spacing = Spaced
             | LSpaced
             | RSpaced
             | Unspaced
             deriving (Show)

makeLenses ''SpacedName


-- === Construction === --

spaced :: Name -> SpacedName
spaced = SpacedName Spaced

lspaced :: Name -> SpacedName
lspaced = SpacedName LSpaced

rspaced :: Name -> SpacedName
rspaced = SpacedName RSpaced

unspaced :: Name -> SpacedName
unspaced = SpacedName Unspaced

spacingBool :: Bool -> Spacing
spacingBool = bool Unspaced Spaced

spacedNameIf :: Bool -> Name -> SpacedName
spacedNameIf = SpacedName . spacingBool

anyLeftSpacing :: Spacing -> Spacing
anyLeftSpacing = \case
    Spaced  -> Spaced
    LSpaced -> Spaced
    _       -> Unspaced

anyRightSpacing :: Spacing -> Spacing
anyRightSpacing = \case
    Spaced  -> Spaced
    RSpaced -> Spaced
    _       -> Unspaced

anyLeftSpaced :: SpacedName -> SpacedName
anyLeftSpaced = spacing %~ anyLeftSpacing

anyRightSpaced :: SpacedName -> SpacedName
anyRightSpaced = spacing %~ anyRightSpacing


-- === Instances === --

-- Precedence
instance Monad m => Prec.RelReader SpacedName (StateT Scope m) where
    readRelLabel (SpacedName sa a) (SpacedName sb b) = case (sa, sb) of
         (Spaced  , Spaced  ) -> Prec.readRel a b
         (Unspaced, Unspaced) -> Prec.readRel a b
         (_       , Spaced  ) -> return GT
         (Spaced, _         ) -> return LT
         _                    -> Prec.readRel a b

-- Association
instance Monad m => Assoc.Reader SpacedName (StateT Scope m) where
    readLabel (SpacedName _ n) = Assoc.readLabel n
