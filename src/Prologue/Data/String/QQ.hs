module Prologue.Data.String.QQ (module Prologue.Data.String.QQ, module X) where

import Language.Haskell.TH.Quote   as X (QuasiQuoter)
import qualified Data.String.QQ    as QQ
import qualified Text.RawString.QQ as QQ
import qualified NeatInterpolation as NeatInterpolation

-- FIXME[WD]: Drop all the QQ's and create proper one - similar to txt but with IsString output
str    :: QuasiQuoter
rawStr :: QuasiQuoter
str    = QQ.s ; {-# INLINE str #-}
rawStr = QQ.r ; {-# INLINE rawStr #-}

txt :: QuasiQuoter
txt = NeatInterpolation.text ; {-# INLINE txt #-}
