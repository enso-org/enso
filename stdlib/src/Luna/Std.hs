{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module Luna.Std where

import           Luna.Prelude

import           Data.Map                    (Map)
import qualified Data.Map                    as Map

import           OCI.IR.Name.Qualified       (QualName)
import           OCI.IR.Name                 (Name)
import           Luna.Std.Finalizers         (finalize, initFinalizersCtx)
import           Luna.Builtin.Data.Module    (Imports)
import           Luna.Builtin.Data.Function  (Function)

import qualified Luna.Prim.Base as Base
import qualified Luna.Prim.Time as Time
import qualified Luna.Prim.WebSockets as WebSockets
import qualified Luna.Prim.System as System
import qualified Luna.Prim.HTTP as HTTP
import qualified Luna.Prim.MsgPack as MsgPack

stdlibImports :: [QualName]
stdlibImports = [ ["Std", "Base"]
                , ["Std", "HTTP"]
                , ["Std", "System"]
                , ["Std", "Time"]
                , ["Std", "WebSockets"]
                ]

stdlib :: Imports -> IO (IO (), Map Name Function)
stdlib std = do
    finalizersCtx <- initFinalizersCtx

    baseFuncs <- Base.exports std finalizersCtx
    timeFuncs <- Time.exports std
    wsFuncs   <- WebSockets.exports std finalizersCtx
    sysFuncs  <- System.exports std
    httpFuncs <- HTTP.exports std
    msgPackFuncs <- MsgPack.exports std

    return (finalize finalizersCtx, Map.unions [baseFuncs, timeFuncs, wsFuncs, sysFuncs, httpFuncs, msgPackFuncs])


