{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Data.Text.Diff where


import Data.Text.Position
-- import Data.Text.Class

--
-- ------------------
-- -- === Diff === --
-- ------------------
--
-- -- === Definition === --
--
-- data Diff = Diff { __range :: !Range
--                  , __text  :: !Text
--                  } deriving (Show)
-- makeClassy ''Diff
--
--
-- -- === Instances === --
--
-- instance HasRange Diff where range = diff_range
-- instance HasText  Diff where text  = diff_text
--
--
--
-- ---------------------
-- -- === Pointed === --
-- ---------------------
--
-- -- === Definition === --
--
-- data Located loc a = Located {  _location :: !loc
--                              , __body     :: !a
--                              } deriving (Show, Functor, Foldable, Traversable)
-- makeLenses ''Located
--
--
-- -- === Instances === --
--
-- instance Copointed (Located loc) where
--     copoint = view located_body
