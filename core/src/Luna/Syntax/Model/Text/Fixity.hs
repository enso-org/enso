module Luna.Syntax.Model.Text.Fixity where

import Prelude.Luna hiding (Constraint)

import qualified Data.Relation.Order as Order
import           Luna.Data.Name



--------------------------
---- === Constraint === --
--------------------------

--data Constraint a = Constraint a [Order.Strict a] deriving (Show, Functor, Foldable, Traversable)


----------------------
---- === Fixity === --
----------------------

--newtype Fixity = Fixity [Constraint MultiName]
