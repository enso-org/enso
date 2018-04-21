{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Component.Term.Definition where

import Prologue

import qualified Control.Lens.TH                     as Lens
import qualified Control.Monad.State.Layered         as State
import qualified Data.Char                           as Char
import qualified Data.Generics.Traversable.Deriving  as GTraversable
import qualified Data.Map.Strict                     as Map
import qualified Data.Tag                            as Tag
import qualified Foreign.Storable.Deriving           as Storable
import qualified Foreign.Storable1.Deriving          as Storable1
import qualified Language.Haskell.TH                 as TH
import qualified Language.Haskell.TH.Builder         as THBuilder
import qualified Language.Haskell.TH.Syntax          as TH
import qualified Luna.IR.Component.Link              as Link
import qualified Luna.IR.Component.Term.Class        as Term
import qualified Luna.IR.Component.Term.Construction as Term
import qualified Luna.IR.Term.Format                 as Format
import qualified OCI.Data.Name                       as IR
import qualified OCI.IR.Layout                       as Layout
import qualified Type.Data.Map                       as TypeMap

import Control.Monad.State.Layered  (StateT)
import Data.Map.Strict              (Map)
import Language.Haskell.TH          (Type (AppT, ConT))
import Language.Haskell.TH.Builder  hiding (Field)
import Luna.IR.Component.Link       (type (*-*), Link)
import Luna.IR.Component.Term.Class (Term)
import Luna.IR.Component.Term.Layer (Model)
import OCI.IR.Layout                (Layout)

import           Data.PtrList.Mutable (UnmanagedPtrList)
import qualified Data.PtrList.Mutable as PtrList

type List = UnmanagedPtrList


---------------------
-- === Helpers === --
---------------------

-- === Definition === --

-- | 'LinkTo' is a phantom helper type for link definition. It gets resolved to
--   much more complex form during 'ExpandField' resolution.
data LinkTo t

-- | 'Field' is a typeclass which unifies how fields of smart cons get
--   constructed. It's created only to make the generated code shorter and
--   more maintainable.
class Monad m => Field t a m b where
    consField :: Term t -> a -> m b

instance Monad m => Field t a m a where
    consField _ = pure ; {-# INLINE consField #-}

instance Link.Creator m => Field t (Term a) m (Link b) where
    consField self t = Layout.unsafeRelayout <$> Link.new t self ; {-# INLINE consField #-}

instance Link.Creator m => Field t [Term a] m (List (Link b)) where
    consField self = PtrList.fromList <=< mapM (consField self) ; {-# INLINE consField #-}

type family ExpandField self layout a where
    ExpandField self layout (LinkTo t) = Link ( Layout.Get t layout
                                            *-* Layout.Set Model self layout)
    ExpandField self layout (t a)      = t (ExpandField self layout a)
    ExpandField self layout a          = a

type family FieldCons var field where
    FieldCons var (LinkTo t) = Term var
    FieldCons var (List a)   = [FieldCons var a]
    FieldCons var a          = a

type family AddToOutput var field layout where
    AddToOutput var (LinkTo t) layout = Layout.SetMerge t var layout
    AddToOutput var (t a)      layout = AddToOutput var a layout
    AddToOutput var a          layout = layout



---------------------------
-- === Term creation === --
---------------------------

-- | Term definition boilerplate
--
--   @
--       Term.define [d|
--           data Thunk = Test
--               { foo :: Foo
--               , bar :: Bar
--               }
--        |]
--   @
--
--   Generates:
--
--   @
--       -- === Definition === --
--
--       Tag.familyInstance "TermCons" "Test"
--       data ConsTest a = Test
--           { __foo :: {-# UNPACK #-} !(ExpandField Test a Foo)
--           , __bar :: {-# UNPACK #-} !(ExpandField Test a Bar)
--           } deriving (Show, Eq)
--       instance Term.IsTermTag Test
--       type instance Format.Of      Test     = Format.Thunk
--       type instance Term.TagToCons Test     = ConsTest
--       type instance Term.ConsToTag ConsTest = Test
--       GTraversable.derive ''ConsTest
--       Storable1.derive    ''ConsTest
--       Storable.derive     ''ConsTest
--       makeLenses          ''ConsTest
--
--
--       -- === Smart constructors === --
--
--       test :: forall a t1 t2 m. Creator Test m
--             => FieldCons t1 Foo
--             -> FieldCons t2 Bar
--             -> m (Term (AddToOutput t1 Foo
--                        (AddToOutput t2 Bar
--                        (Layout.Singleton Model Test)))
--                  )
--       test t1 t2 = Term.newM $ \self
--                 -> pure Test <*> consField self t1 <*> consField self t2
--       {-# INLINE test #-}
--
--       test' :: forall m t2 t1 out.
--                 ( Creator Test m
--                 , Layout.Relayout <test output type> out
--                 ) -> Term t1 -> Term t2 -> m (Term out)
--       test' t1 t2 = Layout.relayout <$> test t1 t2
--       {-# INLINE test' #-}
--
--
--       -- === Instances === --
--
--       instance Link.Provider1 ConsTest where
--           linksIO1 = Link.glinks ; {-# INLINE linksIO1 #-}
--   @
--
--   Moreover:
--   1. Every single field data type is converted to newtype automatically.

define :: Q [Dec] -> Q [Dec]
define = defineChoice True

defineNoSmartCons :: Q [Dec] -> Q [Dec]
defineNoSmartCons = defineChoice False

defineChoice :: Bool -> Q [Dec] -> Q [Dec]
defineChoice needsSmartCons declsQ = do
    decls <- declsQ
    concat <$> mapM (defineSingle needsSmartCons) decls

defineSingle :: Bool -> Dec -> Q [Dec]
defineSingle needsSmartCons termDecl = case termDecl of
    TH.DataD ctx dataName [] kind cons derivs
      -> concat <$> mapM (defineSingleCons needsSmartCons dataName) cons
    _ -> fail "Term constructor should be a non-parametrized data type"

defineSingleCons :: Bool -> Name -> TH.Con -> Q [Dec]
defineSingleCons needsSmartCons dataName con = do
    conName1 <- maybe (fail "All constructors have to be named") pure
              $ convert $ con ^. maybeName
    param    <- newName "a"
    let (needSmartCons, conNameStr) = case last conName1 of
            Just '_' -> (False, unsafeInit conName1)
            _        -> (True,  conName1)
        isNewtype     = length (getBangTypes con) == 1
        termDecl      = if isNewtype
            then TH.NewtypeD [] conName [TH.PlainTV param] Nothing  con  []
            else TH.DataD    [] conName [TH.PlainTV param] Nothing [con] []
        conName       = TH.mkName conNameStr
        typeNameStr   = mkTypeName conNameStr
        tagName       = convert conNameStr
        typeName      = convert typeNameStr

        bangFields    = if isNewtype then id else
                        namedFields %~ fmap (_2 .~ unpackStrictAnn)
        mangleFields  = namedFields %~ fmap (_1 %~ mangleFieldName conNameStr)
        rebindFields  = namedFields %~ fmap (_3 %~ expandField tagName param)
        con'          = mangleFields
                      . bangFields
                      . rebindFields
                      . (maybeName .~ Just conName)
                      $ con

        setTypeName   = maybeName    .~ Just typeName
        setDerivs     = derivClauses .~ [TH.DerivClause Nothing derivs]
        derivs        = cons' <$> [''Show, ''Eq]
        termDecl'     = (consList .~ [con'])
                      . setTypeName
                      . setDerivs
                      $ termDecl

        tagDecls      = Tag.familyInstance' ''Term.TermCons conNameStr
        isTermTagInst = TH.InstanceD Nothing []
                        (TH.AppT (cons' ''Term.IsTermTag) (cons' tagName))
                        []
        linkProviderI = TH.InstanceD Nothing []
                        (TH.AppT (cons' ''Link.Provider1) (cons' typeName))
                        [ TH.ValD (var 'Link.linksIO1) (TH.NormalB $ var 'Link.glinks) []
                        ]
        tagToConsInst = typeInstance ''Term.TagToCons [cons' tagName]
                        (cons' typeName)
        consToTagInst = typeInstance ''Term.ConsToTag [cons' typeName]
                        (cons' tagName)
        format        = TH.mkName
                      $ "Format" <> "." <> convert dataName
        formatInst    = typeInstance ''Format.Of [cons' tagName] (cons' format)

        fieldTypes    = fmap (view _3) . view namedFields $ con

    instLens      <- Lens.declareLenses (pure [termDecl'])
    instStorable  <- Storable.derive'    termDecl'
    instStorable1 <- Storable1.derive'   termDecl'
    instGtraverse <- GTraversable.derive termDecl'
    smartCons     <- makeSmartCons tagName param fieldTypes

    pure
        -- === Definition ===
         $ tagDecls
        <> instLens
        <> [ isTermTagInst
           , tagToConsInst
           , consToTagInst
           , formatInst
           ]
        <> instStorable
        <> instStorable1
        <> instGtraverse

        -- === Smart constructors === --
        <> (if needSmartCons then smartCons else [])

        -- === Instances === --
        <> [ linkProviderI ]

    where maybeNameStr :: MayHaveName a => a -> (Maybe String)
          maybeNameStr = fmap TH.nameBase . view maybeName

expandField :: Name -> Name -> TH.Type -> TH.Type
expandField self param field = app3 (cons' ''ExpandField) (cons' self)
                                    (var param) field



-- === Helpers === --

unpackStrictAnn :: TH.Bang
unpackStrictAnn = TH.Bang TH.SourceUnpack TH.SourceStrict

mkTypeName :: (IsString a, Semigroup a) => a -> a
mkTypeName = ("Cons" <>)

mangleFieldName :: String -> Name -> Name
mangleFieldName sfx n = convert $ fixDuplicateRecordNamesGHCBug (convert n)
                               <> "_" <> sfx



--------------------------------
-- === Smart constructors === --
--------------------------------

makeSmartCons :: Name -> Name -> [TH.Type] -> Q [TH.Dec]
makeSmartCons tName param fieldTypes = do
    sigTypes <- inferSmartConsSigType tName param fieldTypes
    let sigType         = fst sigTypes
        genSigType      = snd sigTypes
        smartConsName   = lowerCase tName
        smartConsName'  = smartConsName <> "'"
        smartConsSig    = TH.SigD smartConsName  sigType
        smartConsGenSig = TH.SigD smartConsName' genSigType
        fieldNum        = length fieldTypes
    smartConsDef    <- makeSmartConsBody tName smartConsName fieldNum
    smartConsGenDef <- makeSmartConsGenBody smartConsName fieldNum
    pure $ smartConsSig : smartConsGenSig : (smartConsDef <> smartConsGenDef)

inferSmartConsSigType :: Name -> Name -> [TH.Type] -> Q (TH.Type, TH.Type)
inferSmartConsSigType name param fields = do
    tvs <- mapM (const $ TH.newName "t") fields
    mtv <- TH.newName "m"
    let ins   = zipWith (app2 (cons' ''FieldCons) . var :: Name -> TH.Type -> TH.Type) tvs fields
        outTp = inferSmartConsTypeOutput name $ zip tvs fields
        out   = app (var mtv) outTp
        inSig = arrows $ ins <> [out]
        ctx   = [app2 (cons' ''Term.Creator) (cons' name) (var mtv)]
        ptvs  = TH.PlainTV <$> (param : tvs <> [mtv])
        sig   = TH.ForallT ptvs ctx inSig

    genOutName <- newName "out"
    let genOut   = app (var mtv) (var genOutName)
        genInSig = arrows $ ins <> [genOut]
        genCtx   = app2 (cons' ''Layout.Relayout) outTp (var genOutName) : ctx
        genTvs   = TH.PlainTV genOutName : ptvs
        genSig   = TH.ForallT genTvs genCtx genInSig

    pure (sig, genSig)

arrows :: [TH.Type] -> TH.Type
arrows ts = foldl (flip arrow) a as where
    arrow a b = AppT (AppT TH.ArrowT a) b
    (a:as)    = reverse ts

inferSmartConsTypeOutput :: Name -> [(Name,TH.Type)] -> TH.Type
inferSmartConsTypeOutput tag ins = app (cons' ''Term) layout where
    layout = foldr ($) base (uncurry apply <$> ins)
    apply  = app3 (cons' ''AddToOutput) . var
    base   = app2 (cons' ''Layout.Singleton) (cons' ''Model) (cons' tag)

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
                        (app2 (var 'consField) (var self) a)
        lam       = TH.LamE [var self]
                  $ foldl seg (app (var 'pure) (cons' tname)) (var <$> ins)
        fn        = TH.FunD fname
                  $ [TH.Clause (TH.VarP <$> ins) (TH.NormalB body) []]
        inline    = TH.PragmaD (TH.InlineP fname TH.Inline TH.FunLike TH.AllPhases)

    pure [fn,inline]

makeSmartConsGenBody :: Name -> Int -> Q [TH.Dec]
makeSmartConsGenBody fname varNum = do
    ins <- newNames varNum
    let fname' = fname <> "'"
        body   = app2 (var 'fmap) (var 'Layout.relayout)
               $ apps (var fname) (var <$> ins)
        fn     = TH.FunD fname'
               $ [TH.Clause (TH.VarP <$> ins) (TH.NormalB body) []]
        inline = TH.PragmaD (TH.InlineP fname' TH.Inline TH.FunLike TH.AllPhases)
    pure [fn, inline]



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
--       Storable.derive     ''UniTerm
--       Storable1.derive    ''UniTerm
--       GTraversable.derive ''UniTerm
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
    termNames <- unpackInst <<$>> TH.reifyInstances ''Term.IsTermTag ["x"]
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
    instStorable  <- Storable.derive'    dataDecl
    instStorable1 <- Storable1.derive'   dataDecl
    instGtraverse <- GTraversable.derive dataDecl

    pure $ [ dataDecl ]
        <> instStorable
        <> instStorable1
        <> instGtraverse
        <> isUniInsts

mkUniTermName :: (IsString a, Semigroup a) => a -> a
mkUniTermName = ("UniTerm" <>)
