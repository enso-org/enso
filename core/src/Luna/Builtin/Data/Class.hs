module Luna.Builtin.Data.Class where

import Luna.Prelude               hiding (Constructor, Destructor)

import Data.Map                   (Map)
import Luna.Builtin.Data.Function (Function, importRooted)
import OCI.IR.Name
import OCI.IR.Class
import OCI.IR.Term
import Data.Families (makeLensedTerm)
import Data.Property
import Data.Event
import Luna.IR.Layer.Errors (CompileError)

import qualified Data.Map    as Map


-------------------
-- === Class === --
-------------------

-- === Definitions === --


data Class = Class { _constructors :: Map Name (Constructor, Destructor)
                   , _methods      :: Map Name (Either [CompileError] Function)
                   }

data Constructor = Constructor { _constructor :: Rooted SomeExpr }

data Destructor  = Destructor  { _destructor :: Rooted SomeExpr
                               , _argList    :: [SomeExpr]
                               }

makeLenses ''Class
makeLenses ''Constructor
makeLenses ''Destructor


whenHasConstructor :: Name -> Class -> Maybe Class
whenHasConstructor n c@(Class cs _) = const c <$> Map.lookup n cs

lookupConstructor :: Name -> Class -> Maybe Constructor
lookupConstructor n (Class cs _) = fst <$> Map.lookup n cs

lookupDestructor :: Name -> Class -> Maybe Destructor
lookupDestructor  n (Class cs _) = snd <$> Map.lookup n cs

importDestructor :: forall l m. (MonadIR m, MonadRef m, Editors Net '[AnyExpr, AnyExprLink] m, Emitter (Import // AnyExpr) m, Emitter (Import // AnyExprLink) m)
               => Destructor -> m (SomeExpr, [SomeExpr])
importDestructor cd = importRooted (cd ^. destructor) >>= \f -> return (f $ cd ^. destructor . root, fmap f $ cd ^. argList)

importConstructor :: forall l m. (MonadIR m, MonadRef m, Editors Net '[AnyExpr, AnyExprLink] m, Emitter (Import // AnyExpr) m, Emitter (Import // AnyExprLink) m)
               => Rooted SomeExpr -> m SomeExpr
importConstructor r = importRooted r >>= \f -> return $ f $ r ^. root
