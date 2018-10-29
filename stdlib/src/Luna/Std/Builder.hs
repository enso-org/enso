{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Std.Builder where

import Prologue

import qualified Data.Graph.Data.Graph.Class           as Graph
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Graph.Store                      as Store
import qualified Data.Set                              as Set
import qualified Data.Map                              as Map
import qualified Luna.IR                               as IR
import qualified Luna.Runtime                          as Luna
import qualified Luna.Pass.Sourcing.Data.Def           as Def
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Attr                        as Attr
import qualified Luna.Pass.Basic                       as Pass
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified Luna.Std.Instances                    as Base
import qualified Luna.Syntax.Prettyprint               as Syntax

import           Data.Set                                     (Set)
import           Data.Map                                     (Map)
import           Data.Graph.Store                             (Rooted (..))

data LTp = LVar IR.Name
         | LCons IR.Qualified IR.Name [LTp]
         | LLam LTp LTp
         deriving (Show)

instance IsString LTp where
    fromString = LVar . convert

varNamesFromType :: LTp -> Set IR.Name
varNamesFromType (LVar n)    = Set.singleton n
varNamesFromType (LCons mod n s) = varNamesFromTypes s
varNamesFromType (LLam t1 t2)    = Set.union (varNamesFromType t1) (varNamesFromType t2)

varNamesFromTypes :: [LTp] -> Set IR.Name
varNamesFromTypes = Set.unions . fmap varNamesFromType

data Signature = Signature
    { _args :: [LTp]
    , _out  :: LTp
    }
type instance Attr.Type Signature = Attr.Atomic
instance Default Signature where
    def = Signature [] $ LVar "a"
makeLenses ''Signature

newtype CompiledSignature = CompiledSignature (Rooted (IR.Term IR.DefHeader))
type instance Attr.Type CompiledSignature = Attr.Atomic
instance Default CompiledSignature where
    def = CompiledSignature $ Rooted mempty
makeLenses ''CompiledSignature

data PrimTypeBuilder
type instance Pass.Spec PrimTypeBuilder t = PrimTypeBuilderSpec t
type family PrimTypeBuilderSpec t where
    PrimTypeBuilderSpec (Pass.In  Pass.Attrs) = '[Signature]
    PrimTypeBuilderSpec (Pass.Out Pass.Attrs) = '[CompiledSignature]
    PrimTypeBuilderSpec t = Pass.BasicPassSpec t

instance ( Pass.Interface PrimTypeBuilder (Pass.Pass stage PrimTypeBuilder)
         , Store.Serializer IR.Terms (Pass.Pass stage PrimTypeBuilder)
         , IR.DeleteSubtree (Pass.Pass stage PrimTypeBuilder)
         ) => Pass.Definition stage PrimTypeBuilder where
    definition = do
        Signature args out <- Attr.get
        let varNames = Set.toList $ varNamesFromTypes $ out : args
        vars <- for varNames $ \v -> (v,) <$> IR.var' v
        let varMap = Map.fromList vars
        argTypes  <- traverse (mkType varMap) args
        outType   <- mkType varMap out
        funType   <- foldM (flip IR.lam') outType $ reverse argTypes
        funHeader <- IR.defHeader funType ([] :: [IR.SomeTerm])
                                          ([] :: [IR.SomeTerm])
                                          ([] :: [IR.SomeTerm])
        rooted <- Store.serialize (Layout.unsafeRelayout funHeader)
        IR.deleteSubtree funHeader
        Attr.put $ CompiledSignature rooted

mkType :: (Pass.Interface PrimTypeBuilder m
          ) => Map IR.Name IR.SomeTerm -> LTp -> m IR.SomeTerm
mkType vars = \case
    LVar n -> return $ fromJust (error "impossible") $ Map.lookup n vars
    LCons mod n fs -> IR.resolvedCons' mod n "" =<< traverse (mkType vars) fs
    LLam tp1 tp2 -> do
        t1 <- mkType vars tp1
        t2 <- mkType vars tp2
        IR.lam' t1 t2

type StdBuilder graph m =
    ( MonadIO m
    , Graph.Encoder graph m
    , Scheduler.MonadScheduler m
    , Scheduler.PassRegister graph PrimTypeBuilder m
    , Pass.Definition graph PrimTypeBuilder
    , Pass.Interface PrimTypeBuilder (Pass.Pass graph PrimTypeBuilder)
    , Store.Serializer IR.Terms (Pass.Pass graph PrimTypeBuilder)
    , IR.DeleteSubtree (Pass.Pass graph PrimTypeBuilder)
    )

makeType :: forall graph m. StdBuilder graph m
         => [LTp] -> LTp -> m (Rooted (IR.Term IR.DefHeader))
makeType args out = Graph.encodeAndEval @graph $ Scheduler.evalT $ do
    Scheduler.registerAttr @Signature
    Scheduler.registerAttr @CompiledSignature

    Scheduler.setAttr $ Signature args out
    Scheduler.enableAttrByType @CompiledSignature

    Scheduler.registerPass @graph @PrimTypeBuilder
    Scheduler.runPassByType @PrimTypeBuilder

    unwrap <$> Scheduler.getAttr @CompiledSignature

makeFunction :: forall graph m. StdBuilder graph m
             => (Luna.Units -> Luna.Value) -> [LTp] -> LTp -> m Def.Def
makeFunction val args out = do
    rooted <- makeType @graph args out
    return $ Def.Precompiled $ Def.PrecompiledDef val rooted

makeFunctionPure :: forall graph m. StdBuilder graph m
                 => (Luna.Units -> Luna.Value) -> [LTp] -> LTp -> m Def.Def
makeFunctionPure val args out = makeFunction @graph val args out

makeFunctionIO :: forall graph m. StdBuilder graph m
               => (Luna.Units -> Luna.Value) -> [LTp] -> LTp -> m Def.Def
makeFunctionIO val args out = makeFunction @graph val args out


makeUnaryMinusType :: forall graph m.
    ( StdBuilder graph m
    ) => m (Rooted (IR.Term IR.DefHeader))
makeUnaryMinusType = Graph.encodeAndEval @graph $ Scheduler.evalT $ do
    Scheduler.registerAttr     @CompiledSignature
    Scheduler.enableAttrByType @CompiledSignature
    Scheduler.registerAttr     @Signature
    Scheduler.enableAttrByType @Signature

    Scheduler.registerPassFromFunction__ @graph @PrimTypeBuilder $ do
        typeVar <- IR.var "#a"
        lam     <- IR.lam typeVar typeVar
        minVar  <- IR.var uminusMethodName
        acc     <- IR.acc typeVar minVar
        uni     <- IR.unify typeVar acc
        hdr     <- IR.defHeader lam [uni] [acc] ([] :: [IR.SomeTerm])
        rooted  <- Store.serialize (Layout.unsafeRelayout hdr)
        IR.deleteSubtree hdr
        Attr.put $ CompiledSignature rooted
    Scheduler.runPassByType @PrimTypeBuilder

    unwrap <$> Scheduler.getAttr @CompiledSignature

uminusMethodName :: IR.Name
uminusMethodName = "negate"

uminusFunName :: IR.Name
uminusFunName = Syntax.uminusName

int :: Integer -> Int
int = fromIntegral

integer :: Int -> Integer
integer = fromIntegral

real :: Real a => a -> Double
real = realToFrac

jsonLT :: LTp
jsonLT = LCons Base.baseModule "JSON" []

listLT :: LTp -> LTp
listLT t  = LCons Base.baseModule "List"  [t]

vectorLT :: LTp -> LTp
vectorLT t  = LCons Base.baseModule "Vector"  [t]

maybeLT :: LTp -> LTp
maybeLT t = LCons Base.baseModule "Maybe" [t]

tuple2LT :: LTp -> LTp -> LTp
tuple2LT t1 t2 = LCons Base.baseModule "Tuple2" [t1, t2]

tuple3LT :: LTp -> LTp -> LTp -> LTp
tuple3LT t1 t2 t3 = LCons Base.baseModule "Tuple3" [t1, t2, t3]

tuple4LT :: LTp -> LTp -> LTp -> LTp -> LTp
tuple4LT t1 t2 t3 t4 = LCons Base.baseModule "Tuple4" [t1, t2, t3, t4]

eitherLT :: LTp -> LTp -> LTp
eitherLT l r = LCons Base.baseModule "Either" [l, r]

mvarLT :: LTp -> LTp
mvarLT = LCons Base.baseModule "MVar" . pure

futureLT :: LTp -> LTp
futureLT = LCons Base.baseModule "Future" . pure

textLT :: LTp
textLT = LCons Base.baseModule "Text" []

intLT :: LTp
intLT = LCons Base.baseModule "Int" []

realLT :: LTp
realLT = LCons Base.baseModule "Real" []

boolLT :: LTp
boolLT = LCons Base.baseModule "Bool" []

scientificLT :: LTp
scientificLT = LCons Base.baseModule "Scientific" []

binaryLT :: LTp
binaryLT = LCons Base.baseModule "Binary" []

noneLT :: LTp
noneLT = LCons Base.baseModule "None" []

funLT :: LTp -> LTp -> LTp
funLT = LLam

