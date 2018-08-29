{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Typing.Base where

import Prologue

import qualified Luna.IR         as IR
import qualified Luna.Pass       as Pass
import qualified Luna.Pass.Basic as Pass

import Luna.Pass.Data.Layer.Requester (Requester)

data BasePass
type instance Pass.Spec BasePass t = BasePassSpec t
type family   BasePassSpec t where
    BasePassSpec (Pass.In  IR.Terms) = '[IR.Model, IR.Type, IR.Users, Requester]
    BasePassSpec (Pass.Out IR.Terms) = '[IR.Model, IR.Type, IR.Users, Requester]
    BasePassSpec a                   = Pass.BasicPassSpec a

