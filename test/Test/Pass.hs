{-# LANGUAGE UndecidableInstances #-}

module Test.Pass where

-- import Prologue

-- import qualified Control.Monad.Exception as Exception
-- import qualified Data.Tag                as Tag
-- import qualified Data.Tuple.Strict       as Tuple
-- import qualified Data.TypeMap.Strict     as TypeMap
-- import qualified Foreign                 as Ptr
-- import qualified Foreign.Memory.Pool     as MemPool
-- import           Luna.IR.Link            as Link
-- import qualified Luna.IR.Link.TH         as Link
-- import qualified Luna.IR.Term            as Term
-- import qualified OCI.IR.Layer            as Layer
-- import qualified OCI.IR.Layer.Internal   as Layer
-- import qualified OCI.IR.Layout           as Layout
-- import qualified OCI.Pass.Cache          as Pass
-- import qualified OCI.Pass.Manager        as PassManager

-- import Foreign.Marshal.Alloc      (mallocBytes)
-- import Foreign.Ptr                (Ptr)
-- import Foreign.Storable           (peek, poke, pokeByteOff)
-- import Foreign.Storable.Deriving  (deriveStorable)
-- import Foreign.Storable.Utils     (sizeOf')
-- import Foreign.Storable1.Deriving (deriveStorable1)
-- import Luna.IR.Format
-- import Luna.IR.Layout
-- import Luna.IR.Term               (Model, Term, TermCons, Terms)
-- import OCI.IR.Component
-- import OCI.Pass.Class             as Pass
-- import OCI.Pass.Manager           (MonadPassManager)
-- import Type.Data.Ord              (Cmp)


-- import Luna.IR.Term.Core



-- data BasicPass
-- type instance Spec BasicPass t = Spec_BasicPass t
-- type family   Spec_BasicPass t where
--     Spec_BasicPass (In Elems) = '[Terms, Links]
--     Spec_BasicPass (In Terms) = '[Model, Type]
--     Spec_BasicPass (In Links) = '[Source, Target]
--     Spec_BasicPass (Out a)    = Spec_BasicPass (In a)
--     Spec_BasicPass t          = '[]

-- Pass.cache_phase1 ''BasicPass
-- Pass.cache_phase2 ''BasicPass



-- test_pm_run :: MonadIO m => m Pass.PassConfig
-- test_pm_run = Exception.catchAll undefined $ PassManager.evalT test_pm

-- test_pm :: (MonadPassManager m, MonadIO m) => m Pass.PassConfig
-- test_pm = do
--     PassManager.registerComponent @Terms
--     PassManager.registerPrimLayer @Terms @Model
--     PassManager.registerPrimLayer @Terms @Type

--     PassManager.registerComponent @Links
--     PassManager.registerPrimLayer @Links @Source
--     PassManager.registerPrimLayer @Links @Target

--     reg <- State.get @PassManager.Registry
--     passCfg <- PassManager.mkPassConfig reg

--     pure passCfg


-- passTest :: Pass.Pass BasicPass
-- passTest = do
--     v1 <- var 5
--     v2 <- var 7
--     v3 <- var 9
--     l1 <- Link.new v1 v2

--     Layer.write @Type v1 l1

--     s <- Layer.read @Source l1
--     m <- Layer.read @Model s
--     print m
--     pure ()

-- passTest_run :: IO ()
-- passTest_run = do

--     cfg <- test_pm_run
--     xx <- Pass.encodePassState cfg
--     Pass.runPass xx passTest
