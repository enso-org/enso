{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.AST.Module where

--import Data.Binary (Binary)

--import Flowbox.Prelude
--import Luna.Syntax.Decl      (LDecl)
--import Luna.Syntax.Label     (Label)
--import Luna.Syntax.Name.Path (QualPath)


--data Module a e = Module { _mpath :: QualPath
--                         , _body  :: [LDecl a e]
--                         } deriving (Show, Generic, Eq, Read)


--makeLenses ''Module

--type LModule a e = Label a (Module a e)

--instance (Binary a, Binary e) => Binary (Module a e)

--instance (Default a, Default e) => Default (Module a e) where
--    def = Module def def
