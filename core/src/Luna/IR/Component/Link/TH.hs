module Luna.IR.Component.Link.TH where

import Prologue

import qualified Language.Haskell.TH as TH

import Control.Monad               (mapM)
import Data.List                   (splitAt)
import Language.Haskell.TH         (Name, Q)
import Language.Haskell.TH.Builder
import Luna.IR.Component.Link      (HasLinks, readLinksIO)


discover :: Name -> Q [TH.Dec]
discover ty = do
    TypeInfo tyConName tyVars cs <- reifyTypeInfo ty
    decs <- genReadLinksIO cs
    pure [classInstance ''HasLinks tyConName tyVars decs]

genReadLinksIO :: [TH.Con] -> Q [TH.Dec]
genReadLinksIO cs = do
    clause <- mapM genReadLinksClause cs
    pure [TH.FunD 'readLinksIO clause, inlineF 'readLinksIO]

genReadLinksClause :: TH.Con -> Q TH.Clause
genReadLinksClause con = do
    (pat, names, freeNames) <- conPat con
    let binds = zipWith readLinkBind freeNames names
        ret   = readLinkReturn freeNames
        body  = TH.DoE $ binds <> [ret]
    pure $ TH.Clause [pat] (TH.NormalB body) mempty

readLinkCall :: Name -> TH.Exp
readLinkCall n = app (var 'readLinksIO) (var n)

readLinkBind :: Name -> Name -> TH.Stmt
readLinkBind n1 n2 = TH.BindS (TH.VarP n1) $ app (var 'readLinksIO) (var n2)

readLinkReturn :: [Name] -> TH.Stmt
readLinkReturn ns = TH.NoBindS . app (var 'pure) $ app (var 'mconcat) vars
    where vars = TH.ListE $ TH.VarE <$> ns

conPat :: TH.Con -> Q (TH.Pat, [Name], [Name])
conPat con = do
    let (cName, cArity) = conNameArity con
    names <- newNames (2 * cArity)
    let (pNames, freeNames) = splitAt cArity names
        patVars             = TH.VarP <$> pNames
        pat                 = TH.ConP cName patVars
    pure (pat, pNames, freeNames)
