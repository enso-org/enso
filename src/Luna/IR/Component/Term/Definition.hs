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


-- TODO: convert single constructor data to newtype by default

-- | Term definition boilerplate
--
--       Term.define [d|
--           data Unify a = Unify
--               { left  :: LinkTo Terms Self a
--               , right :: LinkTo Terms Self a
--               }
--        |]
--
--   Generates:
--
--       Tag.familyInstance "TermCons" "Unify"
--       data ConsUnify a = Unify
--           { __left  :: {-# UNPACK #-} !(LinkTo Terms Unify a)
--           , __right :: {-# UNPACK #-} !(LinkTo Terms Unify a)
--           } deriving (Show, Eq)
--       instance Discovery.IsTermTag Unify
--       type instance Format.Of      Unify     = Format.Phrase
--       type instance Term.TagToCons Unify     = ConsUnify
--       type instance Term.ConsToTag ConsUnify = Unify
--       makeLenses       ''ConsUnify
--       Storable.derive  ''ConsUnify
--       Storable1.derive ''ConsUnify
--       Link.discover    ''ConsUnify

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
        unpackStrict    = TH.Bang TH.SourceUnpack TH.SourceStrict
        mangleFields    = namedFields %~ fmap (_1 %~ mangleFieldName nameStr)
        bangFields      = namedFields %~ fmap (_2 .~ unpackStrict)
        rebindFields    = namedFields %~ fmap (_3 %~ rebindSelf)
        derivs          = cons' <$> [''Show, ''Eq]
        setTypeName     = maybeName    .~ Just typeName
        setDerivClauses = derivClauses .~ [TH.DerivClause Nothing derivs]
        tagDecls        = Tag.familyInstance' "TermCons" nameStr
        con'            = mangleFields
                        . bangFields
                        . rebindFields
                        $ con
        termDecl'       = (consList .~ [con'])
                        . setTypeName
                        . setDerivClauses
                        $ termDecl

        isTermTagInst   = TH.InstanceD Nothing []
                          (TH.AppT (cons' ''Discovery.IsTermTag) (cons' name))
                          []
        rebindSelf      = runIdentity . rebindSelfM
        rebindSelfM     = traverseType $ \case
                              TH.ConT n -> pure $ if n == ''Self
                                  then TH.ConT name
                                  else TH.ConT n
                              a -> rebindSelfM a

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


----------


-- | IMPORTANT: Use 'makeUniTerm' in a place in code where all possible
--              terms are already declared.
--
--   The 'makeUniTerm' function discovers all already declared terms and
--   creates an unified datatype for processing them. Its purpose is also
--   to enumerate the terms, so we can use the ordering when serializing them.
--
--   The generated code looks like:
--
--       data UniTerm a
--           = UniTermTop     !(ConsTop     a)
--           | UniTermVar     !(ConsVar     a)
--           | UniTermMissing !(ConsMissing a)
--           ...
--           deriving (Show, Eq)
--       Storable.derive  ''UniTerm
--       Storable1.derive ''UniTerm
--

makeUniTerm :: Q [Dec]
makeUniTerm = do
    let unpackInst = \case
            TH.InstanceD _ _ (TH.AppT _ (TH.ConT n)) _ -> n
            _ -> error "impossible"
    termNames <- unpackInst <<$>> TH.reifyInstances ''Discovery.IsTermTag ["x"]
    let dataName     = "UniTerm"
        unpackStrict = TH.Bang TH.SourceUnpack TH.SourceStrict
        mkCons n     = TH.NormalC consName [(unpackStrict, TH.AppT (TH.ConT childName) "a")] where
            consName  = dataName <> n
            childName = "Cons"   <> n
        derivs       = [TH.DerivClause Nothing $ cons' <$> [''Show, ''Eq]]
        dataDecl     = TH.DataD [] dataName ["a"] Nothing (mkCons <$> termNames)
                       derivs
    storableInst  <- Storable.derive'   dataDecl
    storable1Inst <- Storable1.derive'  dataDecl
    pure $ [dataDecl]
        <> storableInst
        <> storable1Inst
