{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Data.UniqueNameGen where

import Prologue

import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr



---------------------------
-- === UniqueNameGen === --
---------------------------

-- === Definition === --

newtype UniqueNameGen = UniqueNameGen [String]


-- === API === --

allNames :: [String]
allNames = ('a' :) . show <$> ([0..] :: [Integer])

generateName :: Attr.Editor UniqueNameGen m => m IR.Name
generateName = Attr.get >>= \case
    UniqueNameGen (n : ns) -> do
        Attr.put (UniqueNameGen ns)
        pure $ convert n
    UniqueNameGen _ -> error "Should never happen."


-- === Instances === --

type instance Attr.Type UniqueNameGen = Attr.Atomic
instance Default UniqueNameGen where
    def = UniqueNameGen allNames

