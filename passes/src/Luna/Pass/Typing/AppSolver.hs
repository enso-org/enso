{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Typing.AppSolver where

import Prologue

import qualified Data.Graph.Data.Layer.Layout   as Layout
import qualified Data.Set                       as Set
import qualified Luna.IR                        as IR
import qualified Luna.IR.Aliases                as Uni
import qualified Luna.IR.Layer                  as Layer
import qualified Luna.Pass                      as Pass
import qualified Luna.Pass.Attr                 as Attr
import qualified Luna.Pass.Data.Error           as Error
import qualified Luna.Pass.Data.Layer.Requester as Requester
import qualified Luna.Pass.Data.Stage           as TC
import qualified Luna.Pass.Typing.Base          as TC
import qualified Luna.Pass.Typing.Data.AppQueue as AppQueue
import qualified Luna.Pass.Typing.Data.Progress as Progress
import qualified Luna.Pass.Typing.Data.UniQueue as UniQueue

data AppSolver

type instance Pass.Spec AppSolver t = AppSolverSpec t
type family AppSolverSpec t where
    AppSolverSpec (Pass.In  Pass.Attrs) = '[ UniQueue.UniQueue
                                           , AppQueue.AppQueue
                                           ]
    AppSolverSpec (Pass.Out Pass.Attrs) = '[ UniQueue.UniQueue
                                           , AppQueue.AppQueue
                                           , Progress.Progress
                                           ]
    AppSolverSpec t = TC.BasePassSpec t

instance Pass.Definition TC.Stage AppSolver where
    definition = do
        progress <- runSimplification False
        Attr.put $ Progress.Progress progress

runSimplification :: Bool -> TC.Pass AppSolver Bool
runSimplification nthRun = do
    AppQueue.AppQueue q <- Attr.get
    res <- traverse trySimplify q
    Attr.put $ AppQueue.AppQueue $ fmap snd $ filter (not . fst) $ zip res q
    if or res then runSimplification True else return nthRun

trySimplify :: IR.Term IR.App -> TC.Pass AppSolver Bool
trySimplify app = do
    IR.App f a <- IR.modelView app
    fun <- IR.source f
    arg <- IR.source a
    requester <- Requester.getRequester app
    arising   <- Requester.getArising   app
    Layer.read @IR.Model fun >>= \case
        Uni.Lam i o -> do
            inp       <- IR.source i
            out       <- IR.source o
            uni       <- IR.unify inp arg
            requester <- Requester.getRequester app
            arising   <- Requester.getArising   app
            Requester.setRequester requester uni
            Requester.setArising   arising   uni
            UniQueue.register $ Layout.unsafeRelayout uni
            IR.replace out app
            return True
        Uni.ResolvedCons m c _ _ -> do
            Requester.setRequester Nothing app
            IR.deleteSubtree app
            for_ requester $ Error.setError $ Just
                                $ Error.cannotUseObjectAsAFunction m c
                                    & Error.arisingFrom .~ arising
            return True
        _ -> return False

