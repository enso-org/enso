{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr (module Luna.IR.Expr, module X) where


import qualified Luna.Prelude as Prelude
import Luna.Prelude hiding (List, String, Integer, Rational, cons, seq)

import Luna.IR.Term.Core
import OCI.IR.Class as IR hiding (Import)
import qualified Luna.IR.Term.Core as Term
import qualified Luna.IR.Term.Function as Term
import qualified Luna.IR.Term.Unit     as Term
import qualified Luna.IR.Term.World    as Term
import qualified Luna.IR.Term.Literal  as Literal
import qualified Luna.IR.Term.Cls      as Cls
import OCI.IR.Name
import OCI.IR.Name.Qualified

import Luna.IR.Term.Core     as X
import Luna.IR.Term.Function as X
import Luna.IR.Term.World    as X
import Luna.IR.Term.Unit as X
    ( UnitProxy
    , UnresolvedImport
    , UnresolvedImportSrc
    , UnresolvedImportTgt
    , UnresolvedImportHub
    , ImportSource
    , ForeignSymbolImport
    , ForeignLocationImportList
    , ForeignImportList
    , ForeignImportSafety
    , ForeignImportType
    , Unit)
import qualified Luna.IR.Term.Unit as Import
import OCI.IR.Term  as X

import Type.Inference
import OCI.IR.Layout
import OCI.IR.Layout.Typed hiding (Cons)
import Data.Event (Emitter, type (//))
import Luna.IR.Term ()
import Luna.IR.Format2 ()
import Luna.IR.Format (Draft)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text32 (Text32)


type ExprCons' m a = (MonadRef m, Writer Net AnyExpr m, NewElemEvent m (Expr a))
type ExprCons  m a = (ExprCons' m a, Writer Net AnyExprLink m, NewElemEvent m SomeExprLink)




-- === Star === --

star :: ExprCons' m Star => m (Expr Star)
star = expr Term.uncheckedStar

reserveStar :: (MonadRef m, Writer Net AnyExpr m) => m (Expr Star)
reserveStar = reserveExpr

registerStar :: NewElemEvent m (Expr Star) => Expr Star -> m ()
registerStar = dispatchNewExpr Term.uncheckedStar


-- === Literals === --

number    :: ExprCons' m Number    => Literal.Number                 -> m (Expr Number)
string    :: ExprCons' m String    => Literal.String                 -> m (Expr String)
fmtString :: ExprCons' m FmtString => Literal.FmtString SomeExprLink -> m SomeExpr
number    = expr . Term.uncheckedNumber
string    = expr . Term.uncheckedString
fmtString = expr . Term.uncheckedFmtString

number' :: ExprCons' m Number => Literal.Number -> m SomeExpr
string' :: ExprCons' m String => Literal.String -> m SomeExpr
number' = fmap generalize . number
string' = fmap generalize . string

list' :: ExprCons m List => [Expr t] -> m SomeExpr
list  :: ExprCons m List => [Expr t] -> m (Expr $ List >> t)
list' = fmap generalize . list
list fs = mdo
    t  <- expr $ Term.uncheckedList fn
    fn <- mapM (flip link t . unsafeRelayout) fs
    return t

tuple' :: ExprCons m Tuple => [Expr t] -> m SomeExpr
tuple  :: ExprCons m Tuple => [Expr t] -> m (Expr $ Tuple >> t)
tuple' = fmap generalize . tuple
tuple fs = mdo
    t  <- expr $ Term.uncheckedTuple fn
    fn <- mapM (flip link t . unsafeRelayout) fs
    return t

missing' :: ExprCons' m Missing => m SomeExpr
missing  :: ExprCons' m Missing => m (Expr Missing)
missing' = fmap generalize missing
missing  = expr Term.uncheckedMissing


-- === Prims === --

cons' :: ExprCons m Cons => Name -> [Expr t] -> m SomeExpr
cons  :: ExprCons m Cons => Name -> [Expr t] -> m (Expr $ Cons >> t)
cons' = fmap generalize .: cons
cons n fs = mdo
    t  <- expr $ Term.uncheckedCons n fn
    fn <- mapM (flip link t . unsafeRelayout) fs
    return t

-- FIXME[WD]: make it pure
cons_  :: forall t m. ExprCons m Cons => Name -> m (Expr $ Cons >> t)
cons'_ :: forall   m. ExprCons m Cons => Name -> m SomeExpr
cons_  = flip cons []
cons'_ = fmap generalize . cons_

match' :: ExprCons m Match => Expr a -> [Expr b] -> m SomeExpr
match  :: ExprCons m Match => Expr a -> [Expr b] -> m (Expr $ Match >> (a <+> b))
match' = fmap generalize .: match
match a cs = mdo
    t   <- expr $ Term.uncheckedMatch an cn
    an  <- link (unsafeRelayout a) t
    cn  <- mapM (flip link t . unsafeRelayout) cs
    return t

blank' :: ExprCons' m Blank => m SomeExpr
blank  :: ExprCons' m Blank => m (Expr Blank)
blank' = generalize <$> blank
blank  = expr Term.uncheckedBlank

var' :: ExprCons m Var => Name -> m SomeExpr
var  :: ExprCons m Var => Name -> m (Expr Var)
var' = fmap generalize . var
var  = expr . Term.uncheckedVar

fieldLens' :: ExprCons m FieldLens => QualName -> m SomeExpr
fieldLens  :: ExprCons m FieldLens => QualName -> m (Expr FieldLens)
fieldLens' = fmap generalize . fieldLens
fieldLens  = expr . Term.uncheckedFieldLens

singleFieldLens' :: ExprCons m FieldLens => Name -> m SomeExpr
singleFieldLens  :: ExprCons m FieldLens => Name -> m (Expr FieldLens)
singleFieldLens' = fieldLens' . convert
singleFieldLens  = fieldLens  . convert

acc' :: ExprCons m App => Expr t -> Name -> m SomeExpr
acc  :: ExprCons m Acc => Expr t -> Name -> m (Expr $ Acc >> t)
acc' = fmap generalize .: acc
acc b n = mdo
    t  <- expr $ Term.uncheckedAcc lb n
    lb <- link (unsafeRelayout b) t
    return t

unify' :: ExprCons m Unify => Expr l -> Expr l' -> m SomeExpr
unify  :: ExprCons m Unify => Expr l -> Expr l' -> m (Expr $ Unify >> (l <+> l'))
unify' = fmap generalize .: unify
unify a b = mdo
    t  <- expr $ Term.uncheckedUnify la lb
    la <- link (unsafeRelayout a) t
    lb <- link (unsafeRelayout b) t
    return t

monadic' :: ExprCons m Monadic => Expr l -> Expr l' -> m SomeExpr
monadic  :: ExprCons m Monadic => Expr l -> Expr l' -> m (Expr $ Monadic >> (l <+> l'))
monadic' = fmap generalize .: monadic
monadic a b = mdo
    t  <- expr $ Term.uncheckedMonadic la lb
    la <- link (unsafeRelayout a) t
    lb <- link (unsafeRelayout b) t
    return t

seq' :: ExprCons m Seq => Expr l -> Expr l' -> m SomeExpr
seq  :: ExprCons m Seq => Expr l -> Expr l' -> m (Expr $ Seq >> (l <+> l'))
seq' = fmap generalize .: seq
seq a b = mdo
    t  <- expr $ Term.uncheckedSeq la lb
    la <- link (unsafeRelayout a) t
    lb <- link (unsafeRelayout b) t
    return t

app' :: ExprCons m App => Expr l -> Expr l' -> m SomeExpr
app  :: ExprCons m App => Expr l -> Expr l' -> m (Expr $ App >> (l <+> l'))
app' = fmap generalize .: app
app f a = mdo
    t  <- expr $ Term.uncheckedApp lf la
    lf <- link (unsafeRelayout f) t
    la <- link (unsafeRelayout a) t
    return t


lam' :: ExprCons m Lam => Expr l -> Expr l' -> m SomeExpr
lam  :: ExprCons m Lam => Expr l -> Expr l' -> m (Expr $ Lam >> (l <+> l'))
lam' = fmap generalize .: lam
lam i o = mdo
    t  <- expr $ Term.uncheckedLam li lo
    li <- link (unsafeRelayout i) t
    lo <- link (unsafeRelayout o) t
    return t

grouped' :: ExprCons m Grouped => Expr l -> m SomeExpr
grouped  :: ExprCons m Grouped => Expr l -> m (Expr $ Grouped >> l)
grouped' = fmap generalize . grouped
grouped e = mdo
    t  <- expr $ Term.uncheckedGrouped le
    le <- link (unsafeRelayout e) t
    return t

documented' :: ExprCons m Documented => Text32 -> Expr l -> m SomeExpr
documented  :: ExprCons m Documented => Text32 -> Expr l -> m (Expr $ Documented >> l)
documented' = fmap generalize .: documented
documented doc a = mdo
    t  <- expr $ Term.uncheckedDocumented doc la
    la <- link (unsafeRelayout a) t
    return t


-- === Sugar === --

update' :: ExprCons m Update => Expr l -> [Name] -> Expr l' -> m SomeExpr
update  :: ExprCons m Update => Expr l -> [Name] -> Expr l' -> m (Expr $ Update >> (l <+> l'))
update' = fmap generalize .:. update
update a ns b = mdo
    t  <- expr $ Term.uncheckedUpdate la ns lb
    la <- link (unsafeRelayout a) t
    lb <- link (unsafeRelayout b) t
    return t

modify' :: ExprCons m Modify => Expr l -> [Name] -> Name -> Expr l' -> m SomeExpr
modify  :: ExprCons m Modify => Expr l -> [Name] -> Name -> Expr l' -> m (Expr $ Modify >> (l <+> l'))
modify' = fmap generalize .:: modify
modify a ns n b = mdo
    t  <- expr $ Term.uncheckedModify la ns n lb
    la <- link (unsafeRelayout a) t
    lb <- link (unsafeRelayout b) t
    return t



-- === Definitions === --

rootedFunction :: ExprCons m RootedFunction => IR.Rooted SomeExpr -> m SomeExpr
rootedFunction body = expr $ Term.uncheckedRootedFunction body


asgRootedFunction' :: ExprCons m ASGRootedFunction => Expr a -> IR.Rooted SomeExpr -> m SomeExpr
asgRootedFunction  :: ExprCons m ASGRootedFunction => Expr a -> IR.Rooted SomeExpr -> m (Expr $ ASGRootedFunction >> a)
asgRootedFunction' = fmap generalize .: asgRootedFunction
asgRootedFunction name body = mdo
    t     <- expr $ Term.uncheckedASGRootedFunction lname body
    lname <- link (unsafeRelayout name) t
    return t

asgFunction' :: ExprCons m ASGFunction => Expr a -> [Expr b] -> Expr c -> m SomeExpr
asgFunction  :: ExprCons m ASGFunction => Expr a -> [Expr b] -> Expr c -> m (Expr $ ASGFunction >> (a <+> b <+> c))
asgFunction' = fmap generalize .:. asgFunction
asgFunction name args body = mdo
    t     <- expr $ Term.uncheckedASGFunction lname largs lbody
    lname <- link (unsafeRelayout name) t
    largs <- mapM (flip link t . unsafeRelayout) args
    lbody <- link (unsafeRelayout body) t
    return t

functionSig' :: ExprCons m FunctionSig => Expr a -> Expr b -> m SomeExpr
functionSig  :: ExprCons m FunctionSig => Expr a -> Expr b -> m (Expr $ FunctionSig >> (a <+> b))
functionSig' = fmap generalize .: functionSig
functionSig name sig = mdo
    t     <- expr $ Term.uncheckedFunctionSig lname lsig
    lname <- link (unsafeRelayout name) t
    lsig  <- link (unsafeRelayout sig)  t
    return t

clsASG' :: ExprCons m ClsASG => Bool -> Name -> [Expr a] -> [Expr b] -> [Expr c] -> m SomeExpr
clsASG  :: ExprCons m ClsASG => Bool -> Name -> [Expr a] -> [Expr b] -> [Expr c] -> m (Expr $ ClsASG >> (a <+> b <+> c))
clsASG' = fmap generalize .::. clsASG
clsASG native name args conss decls = mdo
    t  <- expr $ Term.uncheckedClsASG native name an cn dn
    an <- mapM (flip link t . unsafeRelayout) args
    cn <- mapM (flip link t . unsafeRelayout) conss
    dn <- mapM (flip link t . unsafeRelayout) decls
    return t

recASG' :: ExprCons m RecASG => Name -> [Expr a] -> m SomeExpr
recASG  :: ExprCons m RecASG => Name -> [Expr a] -> m (Expr $ RecASG >> a)
recASG' = fmap generalize .: recASG
recASG name args = mdo
    t  <- expr $ Term.uncheckedRecASG name an
    an <- mapM (flip link t . unsafeRelayout) args
    return t

fieldASG' :: ExprCons m FieldASG => [Name] -> Expr a -> m SomeExpr
fieldASG  :: ExprCons m FieldASG => [Name] -> Expr a -> m (Expr $ FieldASG >> a)
fieldASG' = fmap generalize .: fieldASG
fieldASG name a = mdo
    t  <- expr $ Term.uncheckedFieldASG name la
    la <- link (unsafeRelayout a) t
    return t


typed' :: ExprCons m Typed => Expr a -> Expr b -> m SomeExpr
typed  :: ExprCons m Typed => Expr a -> Expr b -> m (Expr $ Typed >> (a <+> b))
typed' = fmap generalize .: typed
typed a b = mdo
    t  <- expr $ Term.uncheckedTyped la lb
    la <- link (unsafeRelayout a) t
    lb <- link (unsafeRelayout b) t
    return t


unit'  :: ExprCons m Unit => Expr a -> [Expr a] -> Expr a -> m SomeExpr
unit'' :: ExprCons m Unit => Expr a -> [Expr a] -> Expr a -> m (Expr Unit)
unit   :: ExprCons m Unit => Expr a -> [Expr a] -> Expr a -> m (Expr $ Unit >> a)
unit'  = fmap generalize .:. unit
unit'' = fmap unsafeGeneralize .:. unit
unit imps units body = mdo
    t      <- expr $ Term.uncheckedUnit limps lunits lbody
    limps  <- link (unsafeRelayout imps)  t
    lunits <- mapM (flip link t . unsafeRelayout) units
    lbody  <- link (unsafeRelayout body)  t
    return t

phantomUnit' :: (ExprCons m Unit, UnitRegistration m) => QualName -> [Expr a] -> m SomeExpr
phantomUnit' name units = do
    hub <- unresolvedImpHub' [] -- FIXME[WD]: Make it resolved
    cls <- Cls.cls' mempty mempty mempty mempty
    t   <- unit' hub (generalize units) cls
    registerUnit name t
    return t


unitProxy' :: (ExprCons m UnitProxy, UnitRegistration m) => QualName -> [Expr a] -> m SomeExpr
unitProxy  :: (ExprCons m UnitProxy, UnitRegistration m) => QualName -> [Expr a] -> m (Expr UnitProxy)
unitProxy' = generalize .:. unitProxy
unitProxy name units = mdo
    t      <- expr $ Term.uncheckedUnitProxy name lunits
    lunits <- mapM (flip link t . unsafeRelayout) units
    registerUnit name t
    return t

simpleUnitProxy' :: (ExprCons m UnitProxy, UnitRegistration m) => QualName -> m SomeExpr
simpleUnitProxy' = flip unitProxy' mempty

imp' :: ExprCons m UnresolvedImport => Expr a -> Expr a -> m SomeExpr
imp  :: ExprCons m UnresolvedImport => Expr a -> Expr a -> m (Expr $ UnresolvedImport >> a)
imp' = generalize .:. imp
imp a tgt = mdo
    t    <- expr $ Term.uncheckedImport la ltgt
    la   <- link (unsafeRelayout a)   t
    ltgt <- link (unsafeRelayout tgt) t
    return t

unresolvedImp' :: ExprCons m UnresolvedImport => Expr a -> UnresolvedImportTgt -> m SomeExpr
unresolvedImp  :: ExprCons m UnresolvedImport => Expr a -> UnresolvedImportTgt -> m (Expr $ UnresolvedImport >> a)
unresolvedImp' = generalize .:. unresolvedImp
unresolvedImp a tgts = mdo
    t  <- expr $ Term.uncheckedUnresolvedImport la tgts
    la <- link (unsafeRelayout a) t
    return t

unresolvedImpSrc' :: ExprCons' m UnresolvedImportSrc => ImportSource -> m SomeExpr
unresolvedImpSrc  :: ExprCons' m UnresolvedImportSrc => ImportSource -> m (Expr UnresolvedImportSrc)
unresolvedImpSrc'   = generalize .: unresolvedImpSrc
unresolvedImpSrc  a = expr $ Term.uncheckedUnresolvedImportSrc a

unresolvedImpHub' :: ExprCons m UnresolvedImportHub => [Expr a] -> m SomeExpr
unresolvedImpHub  :: ExprCons m UnresolvedImportHub => [Expr a] -> m (Expr UnresolvedImportHub)
unresolvedImpHub' = generalize .: unresolvedImpHub
unresolvedImpHub imps = mdo
    t     <- expr $ Term.uncheckedUnresolvedImportHub limps
    limps <- mapM (flip link t . unsafeRelayout) imps
    return t

impHub' :: ExprCons m UnresolvedImportHub => (Map Name (Expr a)) -> m SomeExpr
impHub  :: ExprCons m UnresolvedImportHub => (Map Name (Expr a)) -> m (Expr UnresolvedImportHub)
impHub' = generalize .: impHub
impHub imps = mdo
    t     <- expr $ Term.uncheckedImportHub limps
    limps <- mapM (flip link t . unsafeRelayout) imps
    return t

foreignImpSafety' :: (ExprCons m ForeignImportSafety)
                  => ForeignImportType -> m SomeExpr
foreignImpSafety  :: (ExprCons m ForeignImportSafety)
                  => ForeignImportType -> m (Expr ForeignImportSafety)
foreignImpSafety' = generalize .: foreignImpSafety
foreignImpSafety impType = expr $ Term.uncheckedForeignImportSafety impType

foreignSymbolImp' :: (ExprCons m ForeignSymbolImport)
                  => Expr a -> Expr a -> Name -> Expr a -> m SomeExpr
foreignSymbolImp  :: (ExprCons m ForeignSymbolImport)
                  => Expr a -> Expr a -> Name -> Expr a
                  -> m (Expr ForeignSymbolImport)
foreignSymbolImp' = generalize .::. foreignSymbolImp
foreignSymbolImp safety foreignName localName importType = mdo
    newTerm     <- expr $
        Term.uncheckedForeignSymbolImport safetyExpr forNameExpr localName typeExpr
    safetyExpr  <- link (unsafeRelayout safety) newTerm
    forNameExpr <- link (unsafeRelayout foreignName) newTerm
    typeExpr    <- link (unsafeRelayout importType) newTerm
    return newTerm

foreignLocationImpList' :: (ExprCons m ForeignLocationImportList)
                        => Expr a -> [Expr a] -> m SomeExpr
foreignLocationImpList  :: (ExprCons m ForeignLocationImportList)
                        => Expr a -> [Expr a]
                        -> m (Expr ForeignLocationImportList)
foreignLocationImpList' = generalize .:. foreignLocationImpList
foreignLocationImpList location imports = mdo
    newTerm <- expr $ Term.uncheckedForeignLocationImportList locExpr impExpr
    locExpr <- link (unsafeRelayout location) newTerm
    impExpr <- mapM (flip link newTerm . unsafeRelayout) imports
    return newTerm

foreignImpList' :: (ExprCons m ForeignImportList)
                => Name -> [Expr a] -> m SomeExpr
foreignImpList  :: (ExprCons m ForeignImportList)
                => Name -> [Expr a] -> m (Expr ForeignImportList)
foreignImpList' = generalize .:. foreignImpList
foreignImpList lang imports = mdo
    newTerm   <- expr $ Term.uncheckedForeignImportList lang impLayout
    impLayout <- mapM (flip link newTerm . unsafeRelayout) imports
    return newTerm

invalid' :: ExprCons' m Invalid => Text32 -> m SomeExpr
invalid  :: ExprCons' m Invalid => Text32 -> m (Expr Invalid)
invalid' = generalize .: invalid
invalid  = expr . Term.uncheckedInvalid


accSection  :: ExprCons' m AccSection => [Name] -> m SomeExpr
accSection name = expr $ Term.uncheckedAccSection name

leftSection' :: ExprCons m LeftSection => Expr a -> Expr b -> m SomeExpr
leftSection  :: ExprCons m LeftSection => Expr a -> Expr b -> m (Expr $ LeftSection >> (a <+> b))
leftSection' = generalize .:. leftSection
leftSection op a = mdo
    t   <- expr $ Term.uncheckedLeftSection lop la
    lop <- link (unsafeRelayout op) t
    la  <- link (unsafeRelayout a)  t
    return t

rightSection' :: ExprCons m RightSection => Expr a -> Expr b -> m SomeExpr
rightSection  :: ExprCons m RightSection => Expr a -> Expr b -> m (Expr $ RightSection >> (a <+> b))
rightSection' = generalize .:. rightSection
rightSection op a = mdo
    t   <- expr $ Term.uncheckedRightSection lop la
    la  <- link (unsafeRelayout a)  t
    lop <- link (unsafeRelayout op) t
    return t



disabled' :: ExprCons m Disabled => Expr a -> m SomeExpr
disabled  :: ExprCons m Disabled => Expr a -> m (Expr $ Disabled >> a)
disabled' = fmap generalize . disabled
disabled a = mdo
    t    <- expr $ Term.uncheckedDisabled la
    la   <- link (unsafeRelayout a) t
    return t

marker' :: ExprCons' m Marker => Word64 -> m SomeExpr
marker  :: ExprCons' m Marker => Word64 -> m (Expr Marker)
marker' = generalize .: marker
marker  = expr . Term.uncheckedMarker

marked' :: ExprCons m Marked => Expr l -> Expr r -> m SomeExpr
marked  :: ExprCons m Marked => Expr l -> Expr r -> m (Expr $ Marked >> (l <+> r))
marked' = fmap generalize .: marked
marked l r = mdo
    t  <- expr $ Term.uncheckedMarked ll lr
    ll <- link (unsafeRelayout l) t
    lr <- link (unsafeRelayout r) t
    return t

metadata' :: ExprCons' m Metadata => Text32 -> m SomeExpr
metadata  :: ExprCons' m Metadata => Text32 -> m (Expr Metadata)
metadata' = fmap generalize . metadata
metadata  = expr . Term.uncheckedMetadata
