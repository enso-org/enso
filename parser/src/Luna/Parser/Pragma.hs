{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable        #-}


module Luna.Parser.Pragma where


--import           Data.Typeable

--import           Flowbox.Prelude

--import qualified Luna.Parser.Token         as Tok
--import qualified Luna.System.Pragma.Store  as Pragma
--import           Luna.System.Pragma        (pragma, SwitchPragma, IsPragma)
--import           Luna.System.Pragma.Store  (MonadPragmaStore, PragmaStoreT)
--import           Text.Trifecta.Combinators (DeltaParsing)
--import           Text.Parser.Token         (TokenParsing)
--import           Text.Parser.Char          (CharParsing)
--import           Text.Parser.Combinators   (Parsing)

--data ImplicitSelf = ImplicitSelf deriving (Show, Read, Typeable)
--instance IsPragma ImplicitSelf
--implicitSelf = pragma :: SwitchPragma ImplicitSelf


--data OrphanNames  = OrphanNames  deriving (Show, Read, Typeable)
--instance IsPragma OrphanNames
--orphanNames  = pragma :: SwitchPragma OrphanNames

--data IncludeStd   = IncludeStd   deriving  (Show, Read, Typeable)
--instance IsPragma IncludeStd
--includeStd   = pragma :: SwitchPragma IncludeStd


--init = do
--    Pragma.register implicitSelf
--    Pragma.register orphanNames
--    Pragma.register includeStd

--    Pragma.enable   implicitSelf
--    --Pragma.disable  orphanNames

--deriving instance (TokenParsing m, DeltaParsing m) => DeltaParsing (PragmaStoreT m)
--deriving instance (TokenParsing m, MonadPlus m)    => TokenParsing (PragmaStoreT m)
--deriving instance (CharParsing m, MonadPlus m)     => CharParsing  (PragmaStoreT m)
--deriving instance (Parsing m, MonadPlus m)         => Parsing      (PragmaStoreT m)
