module Prologue.Data.String.QQ (module Prologue.Data.String.QQ, module X) where

import Language.Haskell.TH.Quote   as X (QuasiQuoter)
import qualified Data.String.QQ    as QQ
import qualified Text.RawString.QQ as QQ
import qualified NeatInterpolation as NeatInterpolation

-- FIXME[WD]: Drop all the QQ's and create proper one - similar to txt but with IsString output
qqStr    :: QuasiQuoter
qqRawStr :: QuasiQuoter
qqStr    = QQ.s ; {-# INLINE qqStr    #-}
qqRawStr = QQ.r ; {-# INLINE qqRawStr #-}

qqTxt :: QuasiQuoter
qqTxt = NeatInterpolation.text ; {-# INLINE qqTxt #-}
