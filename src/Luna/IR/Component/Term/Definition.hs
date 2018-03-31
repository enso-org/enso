{-# LANGUAGE OverloadedStrings #-}

module Luna.IR.Component.Term.Definition where

import Prologue

import qualified Control.Lens.TH                  as Lens
import qualified Data.Tag                         as Tag
import qualified Foreign.Storable.Deriving        as Storable
import qualified Foreign.Storable1.Deriving       as Storable1
import qualified Language.Haskell.TH              as TH
import qualified Language.Haskell.TH.Syntax       as TH
import qualified Luna.IR.Component.Link.TH        as Link
import qualified Luna.IR.Component.Term.Class     as Term
import qualified Luna.IR.Component.Term.Discovery as Discovery

import Data.List.Split             (splitOn)
import Language.Haskell.TH.Builder


-- | The 'Self' data is used to discover term self reference declaration.
data Self


-- | Term definition boilerplate
--
--       Term.define [d|
--           newtype Var a = Var
--               { name :: Char
--               } deriving (Show, Eq)
--        |]
--
--   Generates:
--
--       Tag.familyInstance "TermCons" "Var"
--       newtype ConsVar a = Var
--           { __name :: Int
--           } deriving (Show, Eq)
--       instance Discovery.IsTermTag Var
--       type instance Format.Of      Var     = Format.Phrase
--       type instance Term.TagToCons Var     = ConsVar
--       type instance Term.ConsToTag ConsVar = Var
--       makeLenses       ''ConsVar
--       Storable.derive  ''ConsVar
--       Storable1.derive ''ConsVar
--       Link.discover    ''ConsVar

define :: Q [Dec] -> Q [Dec]
define declsQ = do
    decls <- declsQ
    concat <$> mapM defineSingle decls


defineSingle :: Dec -> Q [Dec]
defineSingle termDecl = do
    con  <- case termDecl ^. consList of
        [c] -> return c
        _   -> fail "Term constructor should be a single constructor data type."
    nameStr <- case (maybeNameStr termDecl, maybeNameStr con) of
        (Just n, Just n') -> if n == n'
            then return n
            else fail "Term type should have the same name as its constructor."
        _ -> fail "Term definition should have a named constructor."
    let typeNameStr     = "Cons" <> nameStr
        name            = convert nameStr
        typeName        = convert typeNameStr
        mangleFields    = namedFields %~ fmap (mangleField nameStr)
        derivs          = cons' <$> [''Show, ''Eq]
        setTypeName     = maybeName    .~ Just typeName
        setDerivClauses = derivClauses .~ [TH.DerivClause Nothing derivs]
        tagDecls        = Tag.familyInstance' "TermCons" nameStr
        con'            = mangleFields con
        termDecl'       = (consList .~ [con'])
                        . setTypeName
                        . setDerivClauses
                        $ termDecl

        isTermTagInst   = TH.InstanceD Nothing []
                          (TH.AppT (cons' ''Discovery.IsTermTag) (cons' name))
                          []

        tagToConsInst   = typeInstance ''Term.TagToCons [cons' name] (cons' typeName)
        consToTagInst   = typeInstance ''Term.ConsToTag [cons' typeName] (cons' name)

    lensInst      <- Lens.declareLenses (pure [termDecl'])
    storableInst  <- Storable.derive'   termDecl'
    storable1Inst <- Storable1.derive'  termDecl'

    let decls = tagDecls
             <> lensInst
             <> [ isTermTagInst
                , tagToConsInst
                , consToTagInst
                ]
             <> storableInst
             <> storable1Inst
    return decls
    where maybeNameStr :: MayHaveName a => a -> (Maybe String)
          maybeNameStr = fmap TH.nameBase . view maybeName

mangleFieldName :: String -> Name -> Name
mangleFieldName sfx n = convert $ fixNameStr (convert n) <> "_" <> sfx

mangleField :: String -> TH.VarBangType -> TH.VarBangType
mangleField sfx = _1 %~ mangleFieldName sfx

-- GHC BUG: https://ghc.haskell.org/trac/ghc/ticket/14848
fixNameStr :: String -> String
fixNameStr s = case splitOn ":" s of
    []  -> error "impossible"
    [a] -> a
    as  -> unsafeLast $ unsafeInit as




