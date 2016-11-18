
module Old.Luna.Syntax.Model.Layer (module Old.Luna.Syntax.Model.Layer, module X) where

import Prologue

import Old.Luna.Syntax.Term.Class (TermOf)
import Data.Shell_OLD as X
import Data.Construction
import Data.HMap.Lazy (HTMap)

type instance TermOf (ls :< a) = TermOf a




---------------------------
-- === Native layers === --
---------------------------

-- === Layout-specific === --

data Type        = Type        deriving (Show, Eq, Ord)
data Lambda      = Lambda      deriving (Show, Eq, Ord)
data TCData      = TCData      deriving (Show, Eq, Ord)

-- === Universal === --

instance Castable Bool Bool where cast = id
instance Castable (Maybe a) (Maybe a) where cast = id
instance Castable Char Char where cast = id
instance {-# OVERLAPPABLE #-} Castable a a' => Castable (Maybe a) (Maybe a') where cast = fmap cast
instance Castable HTMap HTMap where cast = id

-- Note layer
data Note = Note deriving (Show, Eq, Ord)
type instance LayerData Note t = String
instance Monad m => Creator m (Layer Note a) where create = return $ Layer ""

-- Name layer
data Name = Name deriving (Show, Eq, Ord)
type instance LayerData Name a = String
instance Monad m => Creator    m (Layer Name a) where create     = return $ Layer ""
instance Monad m => Destructor m (Layer Name a) where destruct _ = return ()

-- TODO: move it to a specific modules

-- Markable layer
data Markable = Markable deriving (Show, Eq, Ord)
type instance LayerData Markable t = Bool
instance Monad m => Creator    m (Layer Markable a) where create = return $ Layer False
instance Monad m => Destructor m (Layer Markable a) where destruct _ = return ()

-- Meta layer
data Meta = Meta deriving (Show, Eq, Ord)
type instance LayerData Meta t = HTMap
instance Monad m => Creator    m (Layer Meta a) where create     = return $ Layer def
instance Monad m => Destructor m (Layer Meta a) where destruct _ = return ()
