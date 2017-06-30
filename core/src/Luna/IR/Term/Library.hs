module Luna.IR.Term.Library where


import Luna.Prelude hiding (FilePath)

import Data.Families (makeLensedTerm)
import Data.Map      (Map)
import Luna.IR.Term.Unit
import OCI.IR.Class
import OCI.IR.Term
import OCI.IR.Name.Qualified
import Data.Property

---------------------
-- === Library === --
---------------------

-- type FilePath = Text
--
-- data TermLibrary a = Library { _filePath :: FilePath
--                              , _units    :: Map QualName (Expr Unit)
--                              } deriving (Show, Functor, Foldable, Traversable)
--
-- makeLensedTerm ''TermLibrary
