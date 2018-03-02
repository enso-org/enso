module Luna.IR.Term.TH where

import Prelude
import Control.Monad (mapM)
import Data.List     (splitAt)

import qualified Language.Haskell.TH         as TH
import qualified Language.Haskell.TH.Syntax  as TH
import           Language.Haskell.TH         (Q, Name)
import           Language.Haskell.TH.Builder

import Luna.IR.Link      (HasLinks, readLinksIO)
import qualified Luna.IR.Link as Link
import OCI.IR.Conversion (generalize)
import qualified OCI.IR.Link as Link


deriveLinks :: Name -> Q [TH.Dec]
deriveLinks ty = do
    TypeInfo tyConName tyVars cs <- getTypeInfo ty
    dec <- genReadLinksIO cs
    return [classInstance ''HasLinks tyConName tyVars [dec]]

genReadLinksIO :: [TH.Con] -> Q TH.Dec
genReadLinksIO cs = do
    clause <- mapM genReadLinksClause cs
    return $ TH.FunD 'readLinksIO clause

genReadLinksClause :: TH.Con -> Q TH.Clause
genReadLinksClause con = do
    (pat, names, freeNames) <- conPat con
    let calls = map readLinkCall names
        binds = map (uncurry readLinkBind) $ zip freeNames names
        ret   = readLinkReturn freeNames
        body  = TH.DoE $ binds ++ [ret]
    return $ TH.Clause [pat] (TH.NormalB body) []

readLinkCall :: Name -> TH.Exp
readLinkCall n = app (var 'readLinksIO) (var n)

readLinkBind :: Name -> Name -> TH.Stmt
readLinkBind n1 n2 = TH.BindS (TH.VarP n1) $ app (var 'readLinksIO) (var n2)

readLinkReturn :: [Name] -> TH.Stmt
readLinkReturn ns = TH.NoBindS $ app (var 'return) $ app (var 'mconcat) vars
    where vars = TH.ListE $ map TH.VarE ns

conPat :: TH.Con -> Q (TH.Pat, [Name], [Name])
conPat con = do
    let (cName, cArity) = conInfo con
    names <- newNames (2 * cArity)
    let (pNames, freeNames) = splitAt cArity names
        patVars = map TH.VarP pNames
        pat     = TH.ConP cName patVars
    return (pat, pNames, freeNames)
