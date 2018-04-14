{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Prettyprint where

import qualified Prelude  as P
import           Prologue
-- import qualified Prelude as P
-- import Luna.Prelude hiding (List, Symbol, SomeSymbol, Prettyprinter, (<+>))
-- import Data.Layout hiding (spaced, Prettyprinter, Doc)
-- import Control.Monad.State.Dependent
-- import qualified OCI.IR as IR
-- import Luna.IR hiding (modify_, line, Definition, Atom)
-- import OCI.Pass hiding (modify_)
-- import qualified Luna.Syntax.Text.Scope as Scope
-- import Luna.Syntax.Text.Parser.Hardcoded (hardcode)
-- import Luna.Syntax.Text.Lexer.Grammar  (isOperator, markerBeginChar, markerEndChar, metadataHeader)
-- import Language.Symbol (HasLabel, LabelOf, label)
-- import System.Log
-- import qualified OCI.IR.Name.Multipart as Name
-- import           OCI.IR.Name.Multipart (MultipartName)
-- import OCI.IR.Name.Qualified
-- import qualified Luna.IR.Term.Literal as Literal
-- import qualified Luna.IR.Term.Unit as Import
-- import qualified Data.Text as Text

-- import Debug.Trace

-- -- TODO: Refactor:
-- type instance GetRefHandler (StateT Scope m) = GetRefHandler m
-- instance Prec.RelReader label m => Prec.RelReader label (SubPass p m)

-- import qualified Luna.Syntax.Text.Parser.Hardcoded as Builtin
import qualified Control.Monad.State.Layered    as State
import qualified Data.Layout                    as Layout
import qualified Data.Layout                    as Doc
import qualified Data.PtrList.Mutable           as List
import qualified Data.Vector.Storable.Foreign   as Vector
import qualified Language.Symbol                as Symbol
import qualified Language.Symbol.Operator.Assoc as Assoc
import qualified Language.Symbol.Operator.Prec  as Prec
import qualified Luna.IR                        as IR
import qualified Luna.IR.Component.Link.Class   as Link
import qualified Luna.IR.Layer                  as Layer
import qualified Luna.IR.Link                   as Link
import qualified Luna.IR.Term.Literal           as Literal
import qualified Luna.Pass                      as Pass
import qualified Luna.Syntax.Text.Lexer.Grammar as Grammar
import qualified Luna.Syntax.Text.Scope         as Scope
import qualified OCI.IR.Layout                  as Layout

import Control.Monad.State.Layered  (StateT)
import Data.Layout                  (quoted, space, (</>))
import Data.Vector.Storable.Foreign (Vector)
import Language.Symbol              (SomeSymbol, atom)
import Language.Symbol.Label        (Labeled (Labeled), label, labeled, unlabel)
import Luna.IR                      (Name)
import Luna.Pass                    (Pass)
import Luna.Syntax.Text.Scope       (Scope)

import Data.Layout     (parensed, (<+>))
import Language.Symbol (body)

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- FIXME -> take it from Builtin
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
minusName    = "-"
uminusName   = "#uminus#"
appName      = "#app#"
accName      = "."
lamName      = ":"
typedName    = "::"
wildcardName = "_"
unifyName    = "="
updateName   = "=" -- #update# ?
arrowName    = "->"


data Prettyprint
type instance Pass.Spec Prettyprint t = Pass.BasicPassSpec t
Pass.cache_phase1 ''Prettyprint
Pass.cache_phase2 ''Prettyprint


---------------------------------
-- === Pretty phantom type === --
---------------------------------

data Pretty = Pretty deriving (Show, Eq)



------------------------
-- === SpacedName === --
------------------------

-- === Definition === --

data SpacedName = SpacedName
    { _spaced  :: !Bool
    , _rawName :: !Name
    } deriving (Show, Eq)


-- === Utils === --

spaced, notSpaced :: Name -> SpacedName
spaced    = SpacedName True
notSpaced = SpacedName False


-- === Instances === --

instance Monad m => Prec.RelReader SpacedName (StateT Scope m) where
    readRelLabel (SpacedName sa a) (SpacedName sb b) = if
        | a == "."     -> pure GT
        | sa && not sb -> pure LT
        | sb && not sa -> pure GT
        | otherwise    -> Prec.readRel a b

instance Monad m => Assoc.Reader (Maybe SpacedName) (StateT Scope m) where
    readLabel = \case Just n -> Assoc.readLabel (_rawName n)
                      _      -> pure Assoc.Left



-- --------------------------
-- -- === PrettySymbol === --
-- --------------------------

-- -- === Definition === --

type PrettySymbol a = Labeled (Maybe SpacedName) (SomeSymbol Pretty a)
type Doc = Layout.Doc Text

type instance Symbol.Data Pretty Symbol.Atom   a = a
type instance Symbol.Data Pretty Symbol.Prefix a = a
type instance Symbol.Data Pretty Symbol.Suffix a = a
type instance Symbol.Data Pretty Symbol.Infix  a = a
type instance Symbol.Data Pretty Symbol.Mixfix a = (a, [Name])


-- === Utils === --

unnamed ::               a -> Labeled (Maybe SpacedName) a
named   :: SpacedName -> a -> Labeled (Maybe SpacedName) a
unnamed = labeled Nothing
named   = labeled . Just

getBody :: PrettySymbol a -> a
getBody (unlabel -> s) = case s of
    Symbol.Atom   t -> t ^. Symbol.body
    Symbol.Prefix t -> t ^. Symbol.body
    Symbol.Infix  t -> t ^. Symbol.body
    Symbol.Suffix t -> t ^. Symbol.body
    Symbol.Mixfix _ -> error "Impossible conversion"


-- -- === Instances === --

instance Convertible Name Doc           where convert = convertVia @String
instance Convertible (PrettySymbol a) a where convert = getBody
-- instance Convertible (Vector Char) Doc  where convert = convertVia @String


-- ---------------------------
-- -- === Prettyprinter === --
-- ---------------------------

-- type  Prettyprinter style m = Prettyprinter style style m
class Monad m => Prettyprinter style m where
    prettyprint :: IR.SomeTerm -> m (PrettySymbol Doc)

-- instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, Prettyprinter style m)
--     => Prettyprinter style (t m) where
--     prettyprint = lift . prettyprint @style ; {-# INLINE prettyprint #-}


showSymbol :: PrettySymbol Doc -> Text
showSymbol (unlabel -> sym) = Doc.renderLineBlock $ Doc.render source where
    source = case sym of
        Symbol.Atom   s -> s ^. Symbol.body
        Symbol.Prefix s -> s ^. Symbol.body
        Symbol.Infix  s -> s ^. Symbol.body
        Symbol.Suffix s -> s ^. Symbol.body
        Symbol.Mixfix _ -> "mixfix"

type Prettyshow style m = (Prettyprinter style (StateT Scope m), Monad m)
run :: ∀ style m a. Prettyshow style m => Scope -> IR.Term a -> m Text
run scope ir = flip State.evalT scope
             $ fmap showSymbol . prettyprint @style $ Layout.relayout ir

-- renderSimple :: Prettyprinter Simple m => IT.Term a -> m Text
-- renderSimple = prettyshow Simple



--------------------------------
-- === Symbol Application === --
--------------------------------

newtype ParensedBool = ParensedBool Bool deriving (Show)
makeLenses ''ParensedBool

instance Convertible Bool ParensedBool where convert = wrap
instance Convertible ParensedBool Bool where convert = unwrap
instance Mempty      ParensedBool      where mempty  = wrap False
instance Semigroup   ParensedBool      where (<>)    = wrap .: (||) `on` unwrap
instance P.Monoid    ParensedBool      where mempty  = mempty
                                             mappend = (<>)

type MonadSymApp m = (Assoc.Reader (Maybe SpacedName) m, Prec.RelReader SpacedName m)
checkAppParens :: MonadSymApp m => PrettySymbol Doc -> PrettySymbol Doc -> m (Bool, Bool)
appSymbols     :: MonadSymApp m => PrettySymbol Doc -> PrettySymbol Doc -> m (PrettySymbol Doc)
appSymbols'    :: MonadSymApp m => PrettySymbol Doc -> PrettySymbol Doc -> m ((ParensedBool, ParensedBool), PrettySymbol Doc)
checkAppParens = fmap (over both convert . fst) .: appSymbols'
appSymbols     = fmap snd .: appSymbols'
appSymbols' sf@(Labeled flab fsym) sa = do
    ascf     <- Assoc.read sf
    asca     <- Assoc.read sa
    argParen <- case (sf^.label, sa^.label) of
        (Just flab, Just alab) -> do
            prec <- case fsym of
                Symbol.Infix _ -> compare EQ <$> Prec.readRelLabel alab flab
                _              -> Prec.readRelLabel flab alab
            pure $ \asc -> case prec of
                LT -> False
                GT -> True
                EQ -> (asca /= ascf) || (asca /= asc)
        _ -> pure $ const False

    let argHandle a asc  = ((mempty, convert $ argParen asc), if argParen asc then parensed a else a)
        arg              = argHandle sa'
        sa'              = convert sa
        argAppAtom       = named (spaced appName) $ Symbol.infixx mempty
        sconcatIf        = switch (<>) (<+>)
        sconcatIfLab l n = sconcatIf $ (_rawName <$> l) == Just n

    out <- case fsym of
        Symbol.Atom {}   -> do
            (ps , sym ) <- appSymbols' argAppAtom sf
            (ps', sym') <- appSymbols' sym sa
            pure ((fst ps', snd ps), sym')
        Symbol.Infix  s -> pure $ (labeled flab . Symbol.prefix) .: sconcatIfLab flab appName    <$> arg Assoc.Left <*> pure (s^.body)
        Symbol.Prefix s -> pure $ (labeled flab . atom)   .: sconcatIfLab flab uminusName <$> pure (s^.body) <*> arg Assoc.Right
        Symbol.Mixfix s -> pure . (mempty,) $ case s^.body of
            (b, [])       -> labeled flab $ atom   (b <+> sa')
            (b, (n : ns)) -> labeled flab $ Symbol.mixfix (b <+> sa' <+> convert n, ns)
    pure out



-- -----------------------------------
-- -- === Simple pretty printer === --
-- -----------------------------------

simple = unnamed . atom

-- -- === Definition === --

data Simple = Simple deriving (Show)

--          , Prettyprinter t t m
--          , Req m '[ Reader // Layer // AnyExpr     // Model
--                   , Reader // Layer // AnyExprLink // Model
--                   ]
--          ) => Prettyprinter Simple t m where
instance ( MonadIO m -- DEBUG ONLY
         , Prec.RelReader SpacedName m
         , Assoc.Reader (Maybe SpacedName) m
         , State.Monad Scope m
         , Layer.Reader IR.Terms IR.Model m
         , Layer.Reader IR.Links Link.Source m
         ) => Prettyprinter Simple m where
    prettyprint = prettyprintSimple

prettyprintSimple ir = Layer.read @IR.Model ir >>= \case
    IR.UniTermAcc  (IR.Acc a name)
        -> named (spaced accName) . atom
         . (\an -> convert an <+> accName <+> convert name)
       <$> subgen a -- FIXME[WD]: check if left arg need to be parensed

    IR.UniTermAccSection (IR.AccSection path)
        -> named (notSpaced accName) . atom
         . ("." <>) . intercalate "." . fmap convert
       <$> Vector.toList path
    IR.UniTermApp  (IR.App f a)         -> join $ appSymbols <$> subgen f <*> subgen a
    IR.UniTermBlank IR.Blank            -> pure $ simple wildcardName
    IR.UniTermCons (IR.Cons name args)  -> do
        args' <- mapM subgen =<< List.toList args
        foldM appSymbols (simple $ convert name) args'
    IR.UniTermFunction (IR.Function n as body)
        -> unnamed . atom
       .:. (\n' as' body' -> "def" <+> n' <> arglist as' <> body')
       <$> subgenBody n <*> (mapM subgenBody =<< List.toList as) <*> smartBlock body
    IR.UniTermFunctionSig (IR.FunctionSig n tp)
        -> simple .: (\n' tp' -> "def" <+> n' <+> typedName <+> tp')
       <$> subgenBody n
       <*> subgenBody tp
    IR.UniTermGrouped (IR.Grouped expr) -> simple . parensed <$> subgenBody expr
    IR.UniTermNumber num                -> simple . convert <$> Literal.prettyshow num
    IR.UniTermImportHub (IR.ImportHub is)
        -> simple . foldl (</>) mempty <$> (mapM subgenBody =<< List.toList is)

    IR.UniTermLam (IR.Lam arg body)
        -> named (notSpaced lamName) . atom
        .: (<>) <$> subgenBody arg <*> smartBlock body
    IR.UniTermList (IR.List elems)
        -> simple . Doc.bracked . intercalate ", "
       <$> (mapM subgenBody =<< List.toList elems)
    IR.UniTermMissing IR.Missing -> pure $ simple mempty
    IR.UniTermSectionLeft  (IR.SectionLeft  op a)
        -> simple . parensed .: (<+>) <$> subgenBody op <*> subgenBody a
    IR.UniTermSectionRight (IR.SectionRight op a)
        -> simple . parensed .: flip (<+>) <$> subgenBody op <*> subgenBody a
    IR.UniTermString (IR.String s)
        -> simple . quoted . convert <$> Vector.toList s -- FIXME [WD]: add proper multi-line strings indentation
    IR.UniTermTuple (IR.Tuple elems)
        -> simple . parensed . (intercalate ", ")
       <$> (mapM subgenBody =<< List.toList elems)
    IR.UniTermTyped (IR.Typed expr tp)
        -> named (spaced typedName) . atom
        .: mappendWith (Doc.spaced typedName)
       <$> subgenBody expr
       <*> subgenBody tp
    IR.UniTermUnify (IR.Unify l r)
        -> named (spaced unifyName) . atom
        .: mappendWith (Doc.spaced unifyName)
       <$> subgenBody l
       <*> subgenBody r
    IR.UniTermUnit (IR.Unit im _ b) -> do
        cls <- Link.source b
        Layer.read @IR.Model cls >>= \case
            IR.UniTermRecord (IR.Record _ _ _ _ ds)
                -> unnamed . atom .: go <$> subgenBody im
                                        <*> (mapM subgenBody =<< List.toList ds)
                where go imps defs = let glue = ""
                        in  imps <> glue <> foldl (</>) mempty defs

    IR.UniTermVar (IR.Var name) -> Scope.lookupMultipartName name <&> \case
        Just (n:|ns) -> labeled Nothing $ Symbol.mixfix (convert n, ns)
        Nothing -> if | Grammar.isOperator name -> named (spaced    name) $ Symbol.infixx (convert name)
                      | name == appName    -> named (notSpaced name) $ Symbol.infixx (convert name)
                      | name == uminusName -> named (notSpaced name) $ Symbol.prefix minusName
                      | otherwise          -> unnamed                $ atom   (convert name)


    t -> error $ "NO PRETTY PRINT FOR: " <> show t
--     prettyprint style subStyle root = matchExpr root $ \case
--         String    str               -> pure . unnamed $ atom (convert $ quoted str) -- FIXME [WD]: add proper multi-line strings indentation
--         RecASG    name args         -> unnamed . atom . (convert name <>) . (\x -> if null x then mempty else space <> intercalate space x) <$> mapM subgenBody args
--         Var       name              -> lookupMultipartName name <&> \case
--                                            Just n -> labeled Nothing $ mixfix (convert $ n ^. Name.base, n ^. Name.segments)
--                                            Nothing -> if | isOperator name    -> named (spaced    name) $ infixx (convert name)
--                                                          | name == appName    -> named (notSpaced name) $ infixx (convert name)
--                                                          | name == uminusName -> named (notSpaced name) $ prefix minusName
--                                                          | otherwise          -> unnamed                $ atom   (convert name)
--         Tuple      elems            -> unnamed . atom . parensed . (intercalate ", ") <$> mapM subgenBody elems
--         Seq       a b               -> unnamed . atom .: (</>) <$> subgenBody a <*> subgenBody b
--         Marked       m a            -> unnamed . atom .: (<>) <$> subgenBody m   <*> subgenBody a
--         Marker         a            -> pure . unnamed . atom $ convert markerBeginChar <> convert (show a) <> convert markerEndChar
--         ASGRootedFunction  n _      -> unnamed . atom . (\n' -> "<function '" <> n' <> "'>") <$> subgenBody n
--         Match        a cs           -> unnamed . atom .: (\expr body -> "case" <+> expr <+> "of" </> indented (block $ foldl (</>) mempty body)) <$> subgenBody a <*> mapM subgenBody cs
--         ClsASG _ n as cs ds         -> unnamed . atom .:. go <$> mapM subgenBody as <*> mapM subgenBody cs <*> mapM subgenBody ds where
--                                            go args conss decls = "class" <+> convert n <> arglist args <> body where
--                                                body      = if_ (not . null $ cs <> ds) $ ":" </> bodyBlock
--                                                bodyBlock = indented (block $ foldl (</>) mempty $ conss <> decls)

--         FieldASG mn a               -> unnamed . atom . (\tp -> if null mn then tp else intercalate space (convert <$> mn) <> Doc.spaced typedName <> tp) <$> subgenBody a
--         Invalid t                   -> pure . named (spaced appName) . atom $ "Invalid" <+> convert (show t)
--         UnresolvedImport i t        -> unnamed . atom . (\src -> "import " <> src <> tgts) <$> subgenBody i where
--                                        tgts = case t of Import.Everything -> ""
--                                                         Import.Listed ns  -> ": " <> intercalate " " (convert <$> ns)
--         UnresolvedImportSrc i       -> pure . unnamed . atom $ case i of
--             Import.World            -> "#World#"
--             Import.Absolute ss      -> convertVia @P.String ss
--             Import.Relative ss      -> "." <> convertVia @P.String ss

--         ForeignImportList language imports -> unnamed . atom .
--             (\imps -> "foreign import" <+> convert language <>  lamName
--                 </> indented (block $ foldl (</>) mempty imps))
--             <$> mapM subgenBody imports
--         ForeignLocationImportList location imports -> unnamed . atom .:
--             (\loc imps -> loc <>  lamName
--                 </> indented (block $ foldl (</>) mempty imps))
--             <$> subgenBody location <*> mapM subgenBody imports
--         ForeignSymbolImport safety foreignName localName sig ->
--             unnamed . atom .:. (\safetyAn forName tSig -> safetyAn
--                 <>  forName <+> convert localName <+> typedName <+> tSig)
--             <$> subgenBody safety <*> subgenBody foreignName <*> subgenBody sig
--         ForeignImportSafety safety -> pure . unnamed. atom $ case safety of
--             Import.Safe    -> "safe " -- Space is required.
--             Import.Unsafe  -> "unsafe "
--             Import.Default -> ""


--         Metadata t -> pure . unnamed . atom
--             $ "###" <+> metadataHeader <+> convertVia @Text t
--         -- FIXME [Ara, WD] Conversion via Text is not efficient
--         Documented d a -> unnamed . atom .
--             (\a' -> convertVia @P.String docJoined </> a') <$> subgenBody a
--             where docLines    = Text.split (== '\n') $ convertVia @P.String d
--                   docComments = map ("#" <>) docLines
--                   docJoined   = Text.intercalate "\n" docComments
--         Disabled a -> unnamed . atom . ("##" <>) <$> subgenBody a
--         Update a ns v -> named (spaced updateName) . atom .:
--             (\a' v' -> convert a' <> "." <> intercalate "." (convert <$> ns)
--             <+> "=" <+> convert v') <$> subgen a <*> subgen v
--         Modify a ns n v -> named (spaced updateName) . atom .:
--             (\a' v' -> convert a' <> "." <> intercalate "." (convert <$> ns)
--                 <+> convert n <> "=" <+> convert v')
--             <$> subgen a <*> subgen v
--         x -> error $ msg <> " " <> show x <> " (" <> show root <> ")" where
--             msg = "Pretty printer: unexpected"

--         where subgen     = prettyprint subStyle subStyle <=< source
--               subgenBody = fmap getBody . subgen
--               smartBlock body = do
--                   multiline <- isMultilineBlock body
--                   body'     <- subgenBody body
--                   pure $ if multiline then lamName </> indented (block body')
--                                         else lamName <> space <> body'
    where subgen     = prettyprint @Simple . Layout.relayout <=< Link.source
          subgenBody = fmap getBody . subgen
          smartBlock body = do
            multiline <- isMultilineBlock body
            body'     <- subgenBody body
            pure $ if multiline then lamName </> Doc.indented (Doc.block body')
                                else lamName <> space <> body'
          arglist as = if not $ null as then space <> intercalate space as
                                        else mempty

-- === Utils === --

-- isMultilineBlock :: Req m '[ Reader // Layer // AnyExpr     // Model
--                            , Reader // Layer // AnyExprLink // Model
--                            ]
isMultilineBlock ::
     ( Layer.Reader IR.Terms IR.Model m
     , Layer.Reader Link.Links Link.Source m
     ) => (IR.Link a) -> m Bool
isMultilineBlock lnk = do
    ir <- Link.source (Layout.relayout lnk :: IR.SomeLink)
    Layer.read @IR.Model ir >>= pure . \case
        IR.UniTermSeq {} -> True
        _                -> False






-- ------------------------------------
-- -- === Compact pretty printer === --
-- ------------------------------------

-- -- === Phantoms === --

-- data CompactStyle = CompactStyle deriving (Show)


-- -- === Compactible === --

-- class Monad m => Compactible t style m where
--     shouldBeCompact :: style -> IR.SomeTerm -> m Bool

-- instance {-# OVERLAPPABLE #-} Monad m => Compactible t style m where
--     shouldBeCompact _ _ = pure False


-- -- === Definition === --

-- instance ( MonadIO m -- DEBUG ONLY
--          , Prettyprinter t t m
--          , Compactible Var CompactStyle m
--          , Prec.RelReader SpacedName m, Assoc.Reader (Maybe SpacedName) m, MonadState Scope m
--          , Req m '[ Reader // Layer // AnyExpr     // Model
--                   , Reader // Layer // AnyExprLink // Model
--                   ]
--          ) => Prettyprinter CompactStyle t m where
--     prettyprint style subStyle root = matchExpr root $ \case
--         String str    -> pure . unnamed . atom . convert . quoted $ if length str > succ maxLen then take maxLen str <> "…" else str where maxLen = 3
--         Var    name   -> shouldBeCompact @Var style root >>= switch (pure . unnamed $ atom "•") defGen
--         Lam{}         -> pure . unnamed $ atom "Ⓕ"
--         ASGFunction{} -> pure . unnamed $ atom "Ⓕ"
--         Marked m b    -> prettyprint style subStyle =<< source b
--         Grouped g     -> do
--             body <- prettyprint style subStyle =<< source g
--             pure $ case unlabel body of
--                 Atom{} -> body
--                 _      -> unnamed . atom . parensed . getBody $ body
--         _             -> defGen
--         where simpleGen = prettyprint Simple subStyle
--               defGen    = simpleGen root



-- ------------------
-- -- === Pass === --
-- ------------------

-- type instance Abstract Pretty = Pretty
-- type instance Inputs  Net   Pretty = '[AnyExpr]
-- type instance Outputs Net   Pretty = '[AnyExpr]
-- type instance Inputs  Layer Pretty = '[AnyExpr // Model, AnyExpr // UID, AnyExprLink // Model]
-- type instance Outputs Layer Pretty = '[]
-- type instance Inputs  Attr  Pretty = '[]
-- type instance Outputs Attr  Pretty = '[]
-- type instance Inputs  Event Pretty = '[] -- will never be used
-- type instance Outputs Event Pretty = '[]
-- type instance Preserves     Pretty = '[]


-- simplePass, compactPass :: MonadPassManager m => IR.SomeTerm -> Pass Pretty m
-- simplePass  = void . subpass Simple
-- compactPass = void . subpass CompactStyle

-- subpass :: ( Req m '[ Reader // Layer // AnyExpr     // Model
--                     , Reader // Layer // AnyExprLink // Model
--                     ]
--            , Prettyprinter style (StateT Scope m)
--            ) => style -> IR.SomeTerm -> m Text
-- subpass style expr = evalDefStateT @Scope $ do
--     hardcode
--     sym <- prettyShow style expr
--     pure $ showSymbol sym

