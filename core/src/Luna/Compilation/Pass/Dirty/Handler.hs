{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Dirty.Handler where

import           Old.Data.Prop
import           Development.Placeholders
import           Prologue                                        hiding (Getter, Setter, read, (#))

import qualified Data.Graph.Backend.NEC                  as NEC
import           Data.Graph
import           Data.Graph.Builder
import           Luna.Compilation.Pass.Dirty.Data.Env            (Env)
import           Luna.Compilation.Pass.Dirty.Data.Label          (Interpreter(..), InterpreterLayer(..))
import qualified Luna.Compilation.Pass.Dirty.Data.Env            as Env
import qualified Luna.Compilation.Pass.Dirty.Data.Env            as Env
import qualified Luna.Compilation.Pass.Dirty.Data.Label          as Label
import qualified Luna.Compilation.Pass.Dirty.Dirty               as Dirty
import           Luna.Compilation.Pass.Dirty.Monad               (DirtyMonad)
import           Luna.Runtime.Dynamics                         (Dynamic, Static)
import           Old.Luna.Syntax.Term.Class                     (Lam)
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Term
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetLayers)


#define PassCtxDirty(m, ls, term) ( ls    ~ NetLayers                              \
                                  , term  ~ Draft Static                           \
                                  , node  ~ (ls :<: term)                          \
                                  , edge  ~ Link node                              \
                                  , graph ~ Hetero (NEC.Graph n e c)               \
                                  , BiCastable     e edge                          \
                                  , BiCastable     n node                          \
                                  , MonadIO m                                      \
                                  , MonadBuilder graph m                           \
                                  , NodeInferable m node                           \
                                  , TermNode Lam  m node                           \
                                  , HasProp Interpreter    node                    \
                                  , Prop Interpreter       node ~ InterpreterLayer \
                                  , DirtyMonad (Env (Ref Node node)) m             \
                                  , ReferencedM Node graph (m) node                \
                                  , ReferencedM Edge graph (m) edge                \
                                  )


nodesToExecute :: PassCtxDirty(m, ls, term) =>  m [Ref Node (ls :<: term)]
nodesToExecute = do
    mapM_ Dirty.followDirty =<< Env.getReqNodes
    Env.getReqNodes


reset :: DirtyMonad (Env node) m => m ()
reset = Env.clearReqNodes


connect :: PassCtxDirty(m, ls, term) => Ref Node (ls :<: term) -> Ref Node (ls :<: term) -> m ()
connect prev next = do
    nd <- read prev
    isPrevDirty <- Dirty.isDirty <$> read prev
    Dirty.markSuccessors $ if isPrevDirty
        then prev
        else next


markModified :: PassCtxDirty(m, ls, term) => Ref Node (ls :<: term) -> m ()
markModified = Dirty.markSuccessors
