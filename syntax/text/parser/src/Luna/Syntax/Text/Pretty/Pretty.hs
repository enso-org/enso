{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Pretty.Pretty where

import qualified Prelude as P
import Luna.Prelude hiding (List, Symbol, UniSymbol, ChainedPrettyPrinter, (<+>))
import Data.Layout hiding (spaced, ChainedPrettyPrinter, Doc)
import qualified Data.Layout as Layout
import qualified Data.Layout as Doc
import Control.Monad.State.Dependent
import qualified OCI.IR as IR
import Luna.IR hiding (modify_, line, Definition, Atom)
import OCI.Pass hiding (modify_)
import qualified Language.Symbol.Operator.Prec  as Prec
import qualified Language.Symbol.Operator.Assoc as Assoc
import Luna.Syntax.Text.Scope (Scope, lookupMultipartName)
import qualified Luna.Syntax.Text.Scope as Scope
import Luna.Syntax.Text.Parser.Hardcoded (hardcode)
import Luna.Syntax.Text.Lexer.Grammar  (isOperator, markerBeginChar, markerEndChar, metadataHeader)
import Language.Symbol (HasLabel, LabelOf, label)
import System.Log
import qualified OCI.IR.Name.Multipart as Name
import           OCI.IR.Name.Multipart (MultipartName)
import OCI.IR.Name.Qualified
import qualified Luna.IR.Term.Literal as Literal
import Language.Symbol hiding (Expr)
import Luna.Syntax.Text.Parser.Hardcoded
import qualified Luna.IR.Term.Unit as Import
import qualified Data.Text as Text

import Debug.Trace

-- TODO: Refactor:
type instance GetRefHandler (StateT Scope m) = GetRefHandler m
instance Prec.RelReader label m => Prec.RelReader label (SubPass p m)



---------------------------------
-- === Pretty phantom type === --
---------------------------------

data Pretty = Pretty deriving (Show, Eq)



------------------------
-- === SpacedName === --
------------------------

-- === Definition === --

data SpacedName = SpacedName { _spaced  :: !Bool
                             , _rawName :: !Name
                             } deriving (Show, Eq)


-- === Utils === --

spaced, notSpaced :: Name -> SpacedName
spaced    = SpacedName True
notSpaced = SpacedName False


-- === Instances === --

instance Monad m => Prec.RelReader SpacedName (StateT Scope m) where
    readRelLabel (SpacedName sa a) (SpacedName sb b) = if
        | a == "."     -> return GT
        | sa && not sb -> return LT
        | sb && not sa -> return GT
        | otherwise    -> Prec.readRel a b

instance Monad m => Assoc.Reader (Maybe SpacedName) (StateT Scope m) where
    readLabel = \case Just n -> Assoc.readLabel (_rawName n)
                      _      -> return Assoc.Left



--------------------------
-- === PrettySymbol === --
--------------------------

-- === Definition === --

type PrettySymbol a = Labeled (Maybe SpacedName) (UniSymbol Pretty a)
type Doc = Layout.Doc Text

type instance Definition Pretty Atom   a = a
type instance Definition Pretty Prefix a = a
type instance Definition Pretty Suffix a = a
type instance Definition Pretty Infix  a = a
type instance Definition Pretty Mixfix a = (a, [Name])


-- === Utils === --

unnamed ::               a -> Labeled (Maybe SpacedName) a
named   :: SpacedName -> a -> Labeled (Maybe SpacedName) a
unnamed = labeled Nothing
named   = labeled . Just

getBody :: PrettySymbol a -> a
getBody (unlabel -> s) = case s of
    Atom   t -> t ^. body
    Prefix t -> t ^. body
    Infix  t -> t ^. body
    Suffix t -> t ^. body
    Mixfix _ -> error "Impossible conversion"


-- === Instances === --

instance Convertible Name Doc           where convert = convertVia @Text
instance Convertible (PrettySymbol a) a where convert = getBody



---------------------------
-- === PrettyPrinter === --
---------------------------

type  PrettyPrinter style m = ChainedPrettyPrinter style style m
class ChainedPrettyPrinter style subStyle m where
    chainedPrettyShow :: style -> subStyle -> SomeExpr -> m (PrettySymbol Doc)

prettyShow :: PrettyPrinter style m => style -> SomeExpr -> m (PrettySymbol Doc)
prettyShow s = chainedPrettyShow s s



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
                Infix _ -> compare EQ <$> Prec.readRelLabel alab flab
                _       -> Prec.readRelLabel flab alab
            return $ \asc -> case prec of
                LT -> False
                GT -> True
                EQ -> (asca /= ascf) || (asca /= asc)
        _ -> return $ const False

    let argHandle a asc  = ((mempty, convert $ argParen asc), if argParen asc then parensed a else a)
        arg              = argHandle sa'
        sa'              = convert sa
        argAppAtom       = named (spaced appName) $ infixx mempty
        sconcatIf        = switch (<>) (<+>)
        sconcatIfLab l n = sconcatIf $ (_rawName <$> l) == Just n

    out <- case fsym of
        Atom {}   -> do
            (ps , sym ) <- appSymbols' argAppAtom sf
            (ps', sym') <- appSymbols' sym sa
            return ((fst ps', snd ps), sym')
        Infix  s -> return $ (labeled flab . prefix) .: sconcatIfLab flab appName    <$> arg Assoc.Left <*> pure (s^.body)
        Prefix s -> return $ (labeled flab . atom)   .: sconcatIfLab flab uminusName <$> pure (s^.body) <*> arg Assoc.Right
        Mixfix s -> return . (mempty,) $ case s^.body of
            (b, [])       -> labeled flab $ atom   (b <+> sa')
            (b, (n : ns)) -> labeled flab $ mixfix (b <+> sa' <+> convert n, ns)
    return out



-----------------------------------
-- === Simple pretty printer === --
-----------------------------------

-- === Definition === --

data SimpleStyle  = SimpleStyle  deriving (Show)

instance ( MonadIO m -- DEBUG ONLY
         , ChainedPrettyPrinter t t m
         , Prec.RelReader SpacedName m, Assoc.Reader (Maybe SpacedName) m, MonadState Scope m
         , Req m '[ Reader // Layer // AnyExpr     // Model
                  , Reader // Layer // AnyExprLink // Model
                  ]
         ) => ChainedPrettyPrinter SimpleStyle t m where
    chainedPrettyShow style subStyle root = matchExpr root $ \case
        Blank                       -> return . unnamed $ atom wildcardName
        Missing                     -> return . unnamed $ atom mempty
        String    str               -> return . unnamed $ atom (convert $ quoted str) -- FIXME [WD]: add proper multi-line strings indentation
        Number    num               -> return . unnamed $ atom (convert $ show num)
        Acc       a name            -> named (spaced accName)   . atom . (\an -> convert an <+> accName <+> convert name) <$> subgen a -- FIXME[WD]: check if left arg need to be parensed
        Unify     l r               -> named (spaced unifyName) . atom .: mappendWith (Doc.spaced unifyName) <$> subgenBody l <*> subgenBody r
        App       f a               -> join $ appSymbols <$> subgen f <*> subgen a
        Cons      name args         -> foldM appSymbols (unnamed . atom $ convert name) =<< mapM subgen args
        RecASG    name args         -> unnamed . atom . (convert name <>) . (\x -> if null x then mempty else space <> intercalate space x) <$> mapM subgenBody args
        Var       name              -> lookupMultipartName name <&> \case
                                           Just n -> labeled Nothing $ mixfix (convert $ n ^. Name.base, n ^. Name.segments)
                                           Nothing -> if | isOperator name    -> named (spaced    name) $ infixx (convert name)
                                                         | name == appName    -> named (notSpaced name) $ infixx (convert name)
                                                         | name == uminusName -> named (notSpaced name) $ prefix minusName
                                                         | otherwise          -> unnamed                $ atom   (convert name)
        Grouped   expr              -> unnamed . atom . parensed <$> subgenBody expr
        Typed     expr tp           -> named (spaced typedName) . atom .: mappendWith (Doc.spaced typedName) <$> subgenBody expr <*> subgenBody tp
        List      elems             -> unnamed . atom . bracked  . (intercalate ", ") <$> mapM subgenBody elems
        Tuple      elems            -> unnamed . atom . parensed . (intercalate ", ") <$> mapM subgenBody elems
        Seq       a b               -> unnamed . atom .: (</>) <$> subgenBody a <*> subgenBody b
        Lam       arg body          -> named (notSpaced lamName) . atom .: (<>) <$> subgenBody arg <*> smartBlock body
        LeftSection  op a           -> unnamed . atom . parensed .:      (<+>) <$> subgenBody op  <*> subgenBody a
        RightSection op a           -> unnamed . atom . parensed .: flip (<+>) <$> subgenBody op  <*> subgenBody a
        Marked       m a            -> unnamed . atom .: (<>) <$> subgenBody m   <*> subgenBody a
        Marker         a            -> return . unnamed . atom $ convert markerBeginChar <> convert (show a) <> convert markerEndChar
        ASGRootedFunction  n _      -> unnamed . atom . (\n' -> "<function '" <> n' <> "'>") <$> subgenBody n
        ASGFunction  n as body      -> unnamed . atom .:. (\n' as' body' -> "def" <+> n' <> arglist as' <> body') <$> subgenBody n <*> mapM subgenBody as <*> smartBlock body
        FunctionSig  n tp           -> unnamed . atom .: (\n' tp' -> "def" <+> n' <+> typedName <+> tp') <$> subgenBody n <*> subgenBody tp
        Match        a cs           -> unnamed . atom .: (\expr body -> "case" <+> expr <+> "of" </> indented (block $ foldl (</>) mempty body)) <$> subgenBody a <*> mapM subgenBody cs
        ClsASG _ n as cs ds         -> unnamed . atom .:. go <$> mapM subgenBody as <*> mapM subgenBody cs <*> mapM subgenBody ds where
                                           go args conss decls = "class" <+> convert n <> arglist args <> body where
                                               body      = if_ (not . null $ cs <> ds) $ ":" </> bodyBlock
                                               bodyBlock = indented (block $ foldl (</>) mempty $ conss <> decls)

        FieldASG mn a               -> unnamed . atom . (\tp -> if null mn then tp else intercalate space (convert <$> mn) <> Doc.spaced typedName <> tp) <$> subgenBody a
        Invalid t                   -> return . named (spaced appName) . atom $ "Invalid" <+> convert (show t)
        UnresolvedImportHub is      -> unnamed . atom . foldl (</>) mempty <$> mapM subgenBody is
        UnresolvedImport i t        -> unnamed . atom . (\src -> "import " <> src <> tgts) <$> subgenBody i where
                                       tgts = case t of Import.Everything -> ""
                                                        Import.Listed ns  -> ": " <> intercalate " " (convert <$> ns)
        UnresolvedImportSrc i       -> return . unnamed . atom $ case i of
            Import.World            -> "#World#"
            Import.Absolute ss      -> convertVia @P.String ss
            Import.Relative ss      -> "." <> convertVia @P.String ss

        ForeignImportList language imports -> unnamed . atom .
            (\imps -> "foreign import" <+> convert language <>  lamName
                </> indented (block $ foldl (</>) mempty imps))
            <$> mapM subgenBody imports
        ForeignLocationImportList location imports -> unnamed . atom .:
            (\loc imps -> loc <>  lamName
                </> indented (block $ foldl (</>) mempty imps))
            <$> subgenBody location <*> mapM subgenBody imports
        ForeignSymbolImport safety foreignName localName sig ->
            unnamed . atom .:. (\safetyAn forName tSig -> safetyAn
                <>  forName <+> convert localName <+> typedName <+> tSig)
            <$> subgenBody safety <*> subgenBody foreignName <*> subgenBody sig
        ForeignImportSafety safety -> return . unnamed. atom $ case safety of
            Import.Safe    -> "safe " -- Space is required.
            Import.Unsafe  -> "unsafe "
            Import.Default -> ""

        Unit      im _ b            -> do
                                       cls <- source b
                                       matchExpr cls $ \case
                                           ClsASG _ _ _ _ ds -> unnamed . atom .: go <$> subgenBody im <*> mapM subgenBody ds
                                               where go imps defs = let glue = ""
                                                                    in  imps <> glue <> foldl (</>) mempty defs

        AccSection n -> return . named (notSpaced accName) . atom
            $ "." <> intercalate "." (convert <$> n)
        Metadata t -> return . unnamed . atom
            $ "###" <+> metadataHeader <+> convertVia @Text t
        -- FIXME [Ara, WD] Conversion via Text is not efficient
        Documented d a -> unnamed . atom .
            (\a' -> convertVia @P.String docJoined </> a') <$> subgenBody a
            where docLines    = Text.split (== '\n') $ convertVia @P.String d
                  docComments = map ("#" <>) docLines
                  docJoined   = Text.intercalate "\n" docComments
        Disabled a -> unnamed . atom . ("##" <>) <$> subgenBody a
        Update a ns v -> named (spaced updateName) . atom .:
            (\a' v' -> convert a' <> "." <> intercalate "." (convert <$> ns)
            <+> "=" <+> convert v') <$> subgen a <*> subgen v
        Modify a ns n v -> named (spaced updateName) . atom .:
            (\a' v' -> convert a' <> "." <> intercalate "." (convert <$> ns)
                <+> convert n <> "=" <+> convert v')
            <$> subgen a <*> subgen v
        x -> error $ msg <> " " <> show x <> " (" <> show root <> ")" where
            msg = "Pretty printer: unexpected"

        where subgen     = chainedPrettyShow subStyle subStyle <=< source
              subgenBody = fmap getBody . subgen
              arglist as = if_ (not $ null as) $ space <> intercalate space as
              smartBlock body = do
                  multiline <- isMultilineBlock body
                  body'     <- subgenBody body
                  return $ if multiline then lamName </> indented (block body')
                                        else lamName <> space <> body'

-- === Utils === --

isMultilineBlock :: Req m '[ Reader // Layer // AnyExpr     // Model
                           , Reader // Layer // AnyExprLink // Model
                           ]
                 => Link' SomeExpr -> m Bool
isMultilineBlock lnk = do
    expr <- source lnk
    matchExpr expr $ return . \case
        Seq {} -> True
        _      -> False






------------------------------------
-- === Compact pretty printer === --
------------------------------------

-- === Phantoms === --

data CompactStyle = CompactStyle deriving (Show)


-- === Compactible === --

class Monad m => Compactible t style m where
    shouldBeCompact :: style -> SomeExpr -> m Bool

instance {-# OVERLAPPABLE #-} Monad m => Compactible t style m where
    shouldBeCompact _ _ = return False


-- === Definition === --

instance ( MonadIO m -- DEBUG ONLY
         , ChainedPrettyPrinter t t m
         , Compactible Var CompactStyle m
         , Prec.RelReader SpacedName m, Assoc.Reader (Maybe SpacedName) m, MonadState Scope m
         , Req m '[ Reader // Layer // AnyExpr     // Model
                  , Reader // Layer // AnyExprLink // Model
                  ]
         ) => ChainedPrettyPrinter CompactStyle t m where
    chainedPrettyShow style subStyle root = matchExpr root $ \case
        String str    -> return . unnamed . atom . convert . quoted $ if length str > succ maxLen then take maxLen str <> "…" else str where maxLen = 3
        Var    name   -> shouldBeCompact @Var style root >>= switch (return . unnamed $ atom "•") defGen
        Lam{}         -> return . unnamed $ atom "Ⓕ"
        ASGFunction{} -> return . unnamed $ atom "Ⓕ"
        Marked m b    -> chainedPrettyShow style subStyle =<< source b
        Grouped g     -> do
            body <- chainedPrettyShow style subStyle =<< source g
            return $ case unlabel body of
                Atom{} -> body
                _      -> unnamed . atom . parensed . getBody $ body
        _             -> defGen
        where simpleGen = chainedPrettyShow SimpleStyle subStyle
              defGen    = simpleGen root



------------------
-- === Pass === --
------------------

type instance Abstract Pretty = Pretty
type instance Inputs  Net   Pretty = '[AnyExpr]
type instance Outputs Net   Pretty = '[AnyExpr]
type instance Inputs  Layer Pretty = '[AnyExpr // Model, AnyExpr // UID, AnyExprLink // Model]
type instance Outputs Layer Pretty = '[]
type instance Inputs  Attr  Pretty = '[]
type instance Outputs Attr  Pretty = '[]
type instance Inputs  Event Pretty = '[] -- will never be used
type instance Outputs Event Pretty = '[]
type instance Preserves     Pretty = '[]


simplePass, compactPass :: MonadPassManager m => SomeExpr -> Pass Pretty m
simplePass  = void . subpass SimpleStyle
compactPass = void . subpass CompactStyle

subpass :: ( Req m '[ Reader // Layer // AnyExpr     // Model
                    , Reader // Layer // AnyExprLink // Model
                    ]
           , PrettyPrinter style (StateT Scope m)
           ) => style -> SomeExpr -> m Text
subpass style expr = evalDefStateT @Scope $ do
    hardcode
    sym <- prettyShow style expr
    return $ renderSymbol sym

renderSymbol :: PrettySymbol Doc -> Text
renderSymbol (unlabel -> sym) = renderLineBlock $ render source where
    source = case sym of
        Atom   s -> s ^. body
        Prefix s -> s ^. body
        Infix  s -> s ^. body
        Suffix s -> s ^. body
        Mixfix _ -> "mixfix"
