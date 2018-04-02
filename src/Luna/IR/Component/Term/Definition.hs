{-# LANGUAGE OverloadedStrings #-}

module Luna.IR.Component.Term.Definition where

import Prologue

import qualified Control.Lens.TH                     as Lens
import qualified Control.Monad.State.Layered         as State
import qualified Data.Char                           as Char
import qualified Data.Map.Strict                     as Map
import qualified Data.Tag                            as Tag
import qualified Foreign.Storable.Deriving           as Storable
import qualified Foreign.Storable1.Deriving          as Storable1
import qualified Language.Haskell.TH                 as TH
import qualified Language.Haskell.TH.Builder         as State
import qualified Language.Haskell.TH.Syntax          as TH
import qualified Luna.IR.Component.Link              as Link
import qualified Luna.IR.Component.Link.TH           as Link
import qualified Luna.IR.Component.Term.Class        as Term
import qualified Luna.IR.Component.Term.Construction as Term
import qualified Luna.IR.Component.Term.Discovery    as Discovery
import qualified Luna.IR.Term.Format                 as Format
import qualified OCI.IR.Layout                       as Layout
import qualified Type.Data.Map                       as TypeMap

import Data.Map.Strict              (Map)
import Luna.IR.Component.Link       (type (*-*), Link)
import Luna.IR.Component.Term.Layer (Model)

import Control.Monad.State.Layered  (StateT)
import Language.Haskell.TH          (Type (AppT, ConT))
import Language.Haskell.TH.Builder
import Luna.IR.Component.Term.Class (Term)
import OCI.IR.Layout                (Layout)



---------------------
-- === Helpers === --
---------------------

-- === Definition === --

-- | 'LinkTo' is a phantom helper type. It gets resolved to 'LinkTo__' during
--   code generation. It is used both as a short form for its expansion
--   (note that expansion refers to `self` which gets replaced by current term
--   type) as well as for smart constructor type inference.
data LinkTo   t a
type LinkTo__ t self a = Link (Layout.Get t a *-* Layout.Set Model self a)

-- | 'FieldCons' is a typeclass which unifies how fields of smart cons get
--   constructed. It's created only to make the generated code shorter and
--   more maintainable.
class Monad m => FieldCons t a m b where
    fieldCons :: Term t -> a -> m b


-- === Instances === --

instance Monad m => FieldCons t a m a where
    fieldCons _ = pure ; {-# INLINE fieldCons #-}

instance Link.Creator m => FieldCons t (Term a) m (Link b) where
    fieldCons self t = Layout.unsafeRelayout <$> Link.new t self ; {-# INLINE fieldCons #-}



---------------------------
-- === Term creation === --
---------------------------

-- | Term definition boilerplate
--
--   @
--       Term.define ''Format.Thunk [d|
--           data Unify a = Unify
--               { left  :: LinkTo Terms a
--               , right :: LinkTo Terms a
--               }
--        |]
--   @
--
--   Generates:
--
--   @
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
--       type instance Format.Of Unify = Format.Thunk
--
--       unify :: forall m t2 t1. Creator Unify m
--             => Term t1 -> Term t2
--             -> m (Term (Layout.Layout
--                      '[ Model := (Layout.MergeList '[Unify])
--                       , Terms := (Layout.MergeList '[t1, t2])]))
--       unify t1 t2 = Term.newM $ \self -> pure Unify <*> fieldCons self t1
--                                                     <*> fieldCons self t2
--       {-# INLINE unify #-}
--   @
--
--   Moreover:
--   1. Every single constructor data type is converted to newtype by default.

define :: Name -> Q [Dec] -> Q [Dec]
define = defineChoice True

defineNoSmartCons :: Name -> Q [Dec] -> Q [Dec]
defineNoSmartCons = defineChoice False

defineChoice :: Bool -> Name -> Q [Dec] -> Q [Dec]
defineChoice needsSmartCons format declsQ = do
    decls <- declsQ
    concat <$> mapM (defineSingle needsSmartCons format) decls

defineSingle :: Bool -> Name -> Dec -> Q [Dec]
defineSingle needsSmartCons format termDecl = do
    (conName, param, con) <- case termDecl of
        TH.DataD _ conName [TH.PlainTV param] _ [con] _
          -> pure (conName, param, con)
        _ -> fail . unlines
           $ [ "Term constructor should be a data type parametrized with"
             , "a single type variable and a single constructor definition."
             ]
    let conNameStr      = convertTo @String conName
    case maybeNameStr con of
        (Just n) -> when_ (n /= conNameStr)
            $ fail "Term type should have the same name as its constructor."
    let typeNameStr     = mkTypeName conNameStr
        tagName         = convert conNameStr
        typeName        = convert typeNameStr

        mangleFields    = namedFields %~ fmap (_1 %~ mangleFieldName conNameStr)
        bangFields      = namedFields %~ fmap (_2 .~ unpackStrictAnn)
        rebindFields    = namedFields %~ fmap (_3 %~ expandField tagName)
        con'            = mangleFields
                        . bangFields
                        . rebindFields
                        $ con

        setTypeName     = maybeName    .~ Just typeName
        setDerivClauses = derivClauses .~ [TH.DerivClause Nothing derivs]
        derivs          = cons' <$> [''Show, ''Eq]
        termDecl'       = (consList .~ [con'])
                        . setTypeName
                        . setDerivClauses
                        $ termDecl

        tagDecls        = Tag.familyInstance' "TermCons" conNameStr
        isTermTagInst   = TH.InstanceD Nothing []
                          (TH.AppT (cons' ''Discovery.IsTermTag) (cons' tagName))
                          []
        tagToConsInst   = typeInstance ''Term.TagToCons [cons' tagName]
                          (cons' typeName)
        consToTagInst   = typeInstance ''Term.ConsToTag [cons' typeName]
                          (cons' tagName)
        formatInst      = typeInstance ''Format.Of [cons' tagName] (cons' format)

        fieldTypes      = fmap (view _3) . view namedFields $ con

    lensInst      <- Lens.declareLenses (pure [termDecl'])
    storableInst  <- Storable.derive'   termDecl'
    storable1Inst <- Storable1.derive'  termDecl'
    smartCons     <- makeSmartCons tagName fieldTypes

    pure $ tagDecls
        <> lensInst
        <> [ isTermTagInst
           , tagToConsInst
           , consToTagInst
           , formatInst
           ]
        <> storableInst
        <> storable1Inst
        <> if needsSmartCons then smartCons else []

    where maybeNameStr :: MayHaveName a => a -> (Maybe String)
          maybeNameStr = fmap TH.nameBase . view maybeName

expandField :: Name -> TH.Type -> TH.Type
expandField self field = case field of
    AppT (AppT (ConT n) t) a -> if n == ''LinkTo
        then apps (cons' ''LinkTo__) [t, cons' self, a]
        else field
    _ -> field


-- === Helpers === --

unpackStrictAnn :: TH.Bang
unpackStrictAnn = TH.Bang TH.SourceUnpack TH.SourceStrict

mkTypeName :: (IsString a, Semigroup a) => a -> a
mkTypeName = ("Cons" <>)

mangleFieldName :: String -> Name -> Name
mangleFieldName sfx n = convert $ fixDuplicateRecordNamesGHCBug (convert n)
                               <> "_" <> sfx

makeSmartCons :: Name -> [TH.Type] -> Q [TH.Dec]
makeSmartCons tName fieldTypes = do
    smartConsSigType <- inferSmartConsSigType tName fieldTypes
    let smartConsName = lowerCase tName
        smartConsSig  = TH.SigD smartConsName smartConsSigType
    smartConsDef <- makeSmartConsBody tName smartConsName (length fieldTypes)
    pure $ smartConsSig : smartConsDef



--------------------------------
-- === Smart constructors === --
--------------------------------

type InputMap a = Map Name [a]

inferSmartConsSigType :: Name -> [TH.Type] -> Q (TH.Type)
inferSmartConsSigType name ts = do
    m            <- TH.newName "m"
    (ins, inMap) <- inferSmartConsTypeInputs ts
    let inMap'    = Map.insert ''Model [cons' name] (fmap var <$> inMap)
    let out       = app (var m) $ inferSmartConsTypeOutput inMap'
        arrow a b = AppT (AppT TH.ArrowT a) b
        (a:as)    = reverse $ ins <> [out]
        sig       = foldl (flip arrow) a as
        ctx       = [app2 (cons' ''Term.Creator) (cons' name) (var m)]
        tvs       = TH.PlainTV <$> (m : concat (Map.elems inMap))
    pure $ TH.ForallT tvs ctx sig

inferSmartConsTypeInputs :: [TH.Type] -> Q ([TH.Type], InputMap Name)
inferSmartConsTypeInputs ts = State.runT (mapM inferSmartConsTypeInput ts)
                              mempty

inferSmartConsTypeInput :: TH.Type -> StateT (InputMap Name) Q TH.Type
inferSmartConsTypeInput field = case field of
    AppT (AppT (ConT n) (ConT t)) a -> if n /= ''LinkTo
        then pure field
        else do
            tvName <- lift $ TH.newName "t"
            State.modify_ @(InputMap Name) $ Map.insertWith (<>) t [var tvName]
            pure $ app (cons' ''Term) (var tvName)
    t -> pure t

inferSmartConsTypeOutput :: InputMap TH.Type -> TH.Type
inferSmartConsTypeOutput map = app (cons' ''Term) layout where
    assocs = uncurry inferSmartConsTypeOutputField <$> Map.assocs map
    layout = app (cons' ''Layout) $ fromList assocs

inferSmartConsTypeOutputField :: Name -> [TH.Type] -> TH.Type
inferSmartConsTypeOutputField k vs = field where
    tp    = app  (cons' ''Layout.MergeList) $ fromList vs
    field = app2 (cons' ''TypeMap.AssocP) (cons' k) tp

makeSmartConsBody :: Name -> Name -> Int -> Q [TH.Dec]
makeSmartConsBody tname fname varNum = do
    ins  <- newNames varNum
    self <- newName "self"
    let body      = app (var 'Term.newM) lam
        seg t a = app2 (var '(<*>)) t
                        (app2 (var 'fieldCons) (var self) a)
        lam       = TH.LamE [var self]
                  $ foldl seg (app (var 'pure) (cons' tname)) (var <$> ins)
        fn        = TH.FunD fname
                  $ [TH.Clause (TH.VarP <$> ins) (TH.NormalB body) []]
        inline    = TH.PragmaD (TH.InlineP fname TH.Inline TH.FunLike TH.AllPhases)

    pure [fn,inline]



--------------------------------
-- === UniTerm generation === --
--------------------------------

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
--       instance Term.IsUni ConsTop     where toUni = UniTermTop
--       instance Term.IsUni ConsVar     where toUni = UniTermVar
--       instance Term.IsUni ConsMissing where toUni = UniTermMissing
--
makeUniTerm :: Q [Dec]
makeUniTerm = do
    let unpackInst = \case
            TH.InstanceD _ _ (TH.AppT _ (TH.ConT n)) _ -> n
            _ -> error "impossible"
    termNames <- unpackInst <<$>> TH.reifyInstances ''Discovery.IsTermTag ["x"]
    let dataName     = "UniTerm"
        mkCons n     = TH.NormalC consName [(unpackStrictAnn, TH.AppT (TH.ConT childName) "a")] where
            consName  = dataName <> n
            childName = mkTypeName n
        derivs       = [TH.DerivClause Nothing $ cons' <$> [''Show, ''Eq]]
        dataDecl     = TH.DataD [] dataName ["a"] Nothing (mkCons <$> termNames)
                       derivs
        isUniInst n  = TH.InstanceD Nothing []
                       (TH.AppT (cons' ''Term.IsUni) (cons' $ mkTypeName n))
                       [TH.ValD "toUni" (TH.NormalB . cons' $ mkUniTermName n) []]
        isUniInsts   = isUniInst <$> termNames
    storableInst  <- Storable.derive'   dataDecl
    storable1Inst <- Storable1.derive'  dataDecl
    pure $ [ dataDecl
           ]
        <> storableInst
        <> storable1Inst
        <> isUniInsts

mkUniTermName :: (IsString a, Semigroup a) => a -> a
mkUniTermName = ("UniTerm" <>)

