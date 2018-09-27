{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Sourcing.ClassProcessor where

import Prologue

import qualified Data.Graph.Data.Component.Vector      as ComponentVector
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Graph.Store                      as Store
import qualified Data.Map                              as Map
import qualified Data.Mutable.Storable.SmallAutoVector as SmallVector
import qualified Data.Vector.Storable.Foreign          as Vector
import qualified Luna.IR                               as IR
import qualified Luna.IR.Aliases                       as Uni
import qualified Luna.IR.Layer                         as Layer
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Attr                        as Attr
import qualified Luna.Pass.Basic                       as Pass
import qualified Luna.Pass.Data.Error                  as Error
import qualified Luna.Pass.Data.Stage                  as TC
import qualified Luna.Pass.Sourcing.Data.Class         as Class
import qualified Luna.Pass.Sourcing.Data.Def           as Def
import qualified Luna.Pass.Sourcing.Data.Unit          as Unit
import qualified Luna.Pass.Sourcing.Utils              as Sourcing
import qualified Luna.Syntax.Text.Lexer.Symbol         as Syntax

import Control.Lens                  ((^..), _Just)
import Data.Map                      (Map)
import Luna.Pass.Data.Root
import Luna.Pass.Sourcing.Data.Class hiding (root)
import Luna.Pass.Sourcing.Data.Def   hiding (documented)

data ClassProcessor

type instance Pass.Spec ClassProcessor t = ClassProcessorSpec t
type family ClassProcessorSpec t where
    ClassProcessorSpec (Pass.In  Pass.Attrs) = '[Root, Unit.Name]
    ClassProcessorSpec (Pass.Out Pass.Attrs) = '[Root, Class]
    ClassProcessorSpec t = Pass.BasicPassSpec t


instance Pass.Definition TC.Stage ClassProcessor where
    definition = do
        Root r            <- Attr.get
        Unit.Name modName <- Attr.get
        (r, cls) <- prepareClass modName $ Layout.unsafeRelayout r
        Attr.put $ Root r
        Attr.put cls

prepareClass :: IR.Qualified -> IR.Term IR.Record -> TC.Pass ClassProcessor (IR.SomeTerm, Class)
prepareClass = \modName klass -> do
    IR.Record native name params conses defs <- IR.modelView klass
    parameters      <- traverse IR.source =<< ComponentVector.toList params
    paramNames      <- getParamNames parameters
    constructors    <- traverse IR.source =<< ComponentVector.toList conses
    (newConses, constructorsMap) <- processConses native name constructors
    accs            <- generateFieldAccessors constructorsMap
    methods         <- traverse IR.source =<< ComponentVector.toList defs
    hasToJSONImpl   <- hasMethod methods "toJSON"
    toJSON          <- if hasToJSONImpl || native then return [] else do
        defaultToJSON <- generateToJSON constructorsMap
        return [defaultToJSON]
    let allMethods = accs <> methods <> toJSON
    newKlass <- IR.record' native
                           name
                           parameters
                           newConses
                           allMethods
    IR.replace newKlass klass
    defsMap <- foldM (registerMethod modName name paramNames) def allMethods
    return (newKlass, Class (Constructor . length <$> constructorsMap)
                            defsMap
                            (Layout.unsafeRelayout newKlass))

type Fields = [([IR.Name], IR.SomeTerm)]
type ConsMap = Map IR.Name [Maybe IR.Name]

hasMethod :: [IR.SomeTerm] -> IR.Name -> TC.Pass ClassProcessor Bool
hasMethod methods methodName = fmap or $ for methods $ \a -> do
    (doc, root) <- Sourcing.cutDoc a
    Layer.read @IR.Model root >>= \case
        Uni.Function n _ _ -> do
            name <- IR.source n
            Layer.read @IR.Model name >>= \case
                Uni.Var v -> return $ v == methodName
                _         -> return False
        _                  -> return False

fieldNames :: Fields -> [Maybe IR.Name]
fieldNames fs = concatMap flattenField fs where
    flattenField ([], _) = [Nothing]
    flattenField (ns, _) = Just <$> ns

getParamNames :: [IR.SomeTerm] -> TC.Pass ClassProcessor [IR.Name]
getParamNames ts = fmap catMaybes $ for ts $ Layer.read @IR.Model >=> \case
    Uni.Var n -> return $ Just n
    _         -> return Nothing

generateFieldAccessors :: ConsMap ->  TC.Pass ClassProcessor [IR.SomeTerm]
generateFieldAccessors = \consMap -> case Map.toList consMap of
    [(consName, fields)] -> do
        let fieldsCount = length fields
        accs <- for (zip [0..] fields) $ \(ix, mayFname) ->
            for mayFname $ \fname -> do
                get <- generateGetter consName fieldsCount ix fname
                set <- generateSetter consName fieldsCount ix fname
                pure (get, set)
        pure $ accs ^.. traverse . _Just . both
    _ -> return []

-- | generateGetter Cons n i field generates a method of the following shape:
--     def field:
--         case self of
--             Cons _1 ... a_i ... _n: a_i
generateGetter :: IR.Name -> Int -> Int -> IR.Name
               -> TC.Pass ClassProcessor IR.SomeTerm
generateGetter = \consName fieldsCount fieldPos fieldName -> do
    retVar   <- IR.var  fieldName
    matchVar <- IR.var' fieldName
    selfVar  <- IR.var  $ convert Syntax.selfName
    funName  <- IR.var  fieldName
    blanks   <- sequence $ take (fieldsCount - 1) $ repeat IR.blank'
    let argsBeforeMatched = take fieldPos blanks
        argsAfterMatched  = drop fieldPos blanks
        matchers :: [IR.SomeTerm] =  argsBeforeMatched
                                  <> [matchVar]
                                  <> argsAfterMatched
    clauseCons <- IR.cons consName matchers
    clause     <- IR.lam clauseCons retVar
    match      <- IR.match selfVar [clause]
    getter     <- IR.function funName [] match
    doc        <- SmallVector.fromList "Field getter"
    documented <- IR.documented doc getter
    return $ Layout.relayout documented

-- | generateSetter Cons n i field generates a method of the following shape:
--     def field= v:
--         case self of
--             Cons a_1 ... a_i ... a_n:
--                 Cons a_1 ... v ... a_n
generateSetter :: IR.Name -> Int -> Int -> IR.Name
               -> TC.Pass ClassProcessor IR.SomeTerm
generateSetter consName fieldsCount fieldPos fieldName = do
    -- since we are defining function body, it's safe to hardcode
    -- local variable names here
    self      <- IR.var $ convert Syntax.selfName
    argVarIn  <- IR.var  "v"
    argVarOut <- IR.var' "v"
    funName   <- IR.var $ convert $ Syntax.fieldSetterName $ convert fieldName
    let manyNames = ("a" <>) . convert . show <$> [1..]
    matchVars <- sequence $ take fieldsCount $ IR.var  <$> manyNames
    retVars   <- sequence $ take fieldsCount $ IR.var' <$> manyNames
    let argsBeforeMatched = take fieldPos       retVars
    let argsAfterMatched  = drop (fieldPos + 1) retVars
    let outVars :: [IR.SomeTerm] =  argsBeforeMatched
                                 <> [argVarOut]
                                 <> argsAfterMatched
    outCons :: IR.SomeTerm <- IR.cons' consName ([] :: [IR.SomeTerm])
    outConsApplied <- foldM IR.app' outCons outVars
    inCons <- IR.cons consName matchVars
    clause <- IR.lam inCons outConsApplied
    match  <- IR.match self [clause]
    setter <- IR.function funName [argVarIn] match
    doc    <- SmallVector.fromList "Field setter"
    IR.documented' doc setter

-- | generateToJSON ConsMap generates a method of the following shape:
--     def toJSON:
--         case self of
--             Cons1 a_1 ... a_i ... a_n:
--                 JSON . empty . insert "a_1" a_1.toJSON
--                              . ...
--                              . insert "a_i" a_i.toJSON
--                              . ...
--                              . insert "a_n" a_n.toJSON
--                              . insert "tag" "Cons1"
--             Cons2 a_1 ... a_i ... a_n:
--                 JSON . empty . insert "a_1" a_1.toJSON
--                              . ...
--                              . insert "a_i" a_i.toJSON
--                              . ...
--                              . insert "a_n" a_n.toJSON
--                              . insert "tag" "Cons2"
--
--   Field names will be consistent with names of constructor fields
--   when possible, "a_i" will be used if name couldn't be determined.
--
--   When class has one constructor, field "tag" will be omitted.
generateToJSON :: ConsMap -> TC.Pass ClassProcessor IR.SomeTerm
generateToJSON = \consMap -> do
    let conses = Map.toList consMap
    consClauses <- for conses $ \(consName, fieldsNames) -> do
        let fallbackNames = Just . ("a" <>) . convert . show <$> [1..]
            fields        = catMaybes $ zipWith (<|>) fieldsNames fallbackNames
        fieldsVars     <- mapM IR.var fields
        consPattern    <- IR.cons consName fieldsVars
        jsonCons       <- IR.cons "JSON" []
        empty          <- IR.var "empty"
        jsonEmpty :: IR.SomeTerm <- IR.acc' jsonCons empty
        insertVar      <- IR.var "insert"
        let insertField expr (fieldName, fieldVar) = do
                insert       <- IR.acc expr insertVar
                fieldNameVec <- SmallVector.fromList $ convert fieldName
                nameApplied  <- IR.app insert =<< IR.rawString fieldNameVec
                fieldJSON    <- IR.acc fieldVar =<< IR.var "toJSON"
                valueApplied <- IR.app' nameApplied fieldJSON
                return valueApplied
        jsonImpl       <- foldM insertField jsonEmpty $ zip fields fieldsVars
        let includeTag  = length conses > 1
        consToJSONImpl <- if includeTag then do
                insert          <- IR.acc jsonImpl insertVar
                tagVec          <- SmallVector.fromList "tag"
                tagApplied      <- IR.app insert =<< IR.rawString tagVec
                consNameVec     <- SmallVector.fromList $ convert consName
                consNameApplied <- IR.app' tagApplied =<< IR.rawString consNameVec
                return consNameApplied
            else
                return jsonImpl
        IR.lam consPattern consToJSONImpl
    selfVar    <- IR.var $ convert Syntax.selfName
    match      <- IR.match selfVar consClauses
    toJSONVar  <- IR.var "toJSON"
    toJSONDef  <- IR.function toJSONVar [] match
    doc        <- SmallVector.fromList "Default toJSON implementation"
    IR.documented' doc toJSONDef


registerCons :: ([IR.SomeTerm], ConsMap) -> IR.SomeTerm -> TC.Pass ClassProcessor ([IR.SomeTerm], ConsMap)
registerCons = \(cs, map) root -> Layer.read @IR.Model root >>= \case
    Uni.RecordCons n fs -> do
        fields <- traverse IR.source =<< ComponentVector.toList fs
        fs <- foldM registerFields def fields
        return $ (root : cs, Map.insert n (fieldNames fs) map)
    _ -> return (cs, map)

flattenApps :: IR.SomeTerm -> TC.Pass ClassProcessor (IR.SomeTerm, [IR.SomeTerm])
flattenApps r = Layer.read @IR.Model r >>= \case
    Uni.Grouped g -> flattenApps =<< IR.source g
    Uni.App f a -> do
        (fun, args) <- flattenApps =<< IR.source f
        arg         <- IR.source a
        return (fun, arg : args)
    _ -> return (r, [])

mkFieldTp :: IR.SomeTerm -> TC.Pass ClassProcessor IR.SomeTerm
mkFieldTp root = Layer.read @IR.Model root >>= \case
    Uni.Var{} -> return root
    _ -> do
        (r, reversedArgs) <- flattenApps root
        let args = reverse reversedArgs
        Layer.read @IR.Model r >>= \case
            Uni.Cons n _ -> do
                processed <- traverse mkFieldTp args
                IR.cons' n processed
            Uni.Var "->" -> do
                case args of
                    [a1, a2] -> IR.lam' a1 a2
                    _        -> return root
            _ -> return root

processFieldTp :: IR.SomeTerm -> TC.Pass ClassProcessor IR.SomeTerm
processFieldTp term = do
    flattened <- mkFieldTp term
    IR.replace flattened term
    return flattened

registerFields :: Fields -> IR.SomeTerm -> TC.Pass ClassProcessor Fields
registerFields = \fs root -> Layer.read @IR.Model root >>= \case
    Uni.RecordFields ns tp' -> do
        tp    <- processFieldTp =<< IR.source tp'
        names <- SmallVector.toList ns
        return $ fs <> [(names, tp)]
    _ -> return fs

processConses ::  Bool -> IR.Name -> [IR.SomeTerm]
              -> TC.Pass ClassProcessor ([IR.SomeTerm], ConsMap)
processConses isNative className conses = do
    topFields <- foldM registerFields def conses
    let topFieldNames = fieldNames topFields
    (realConses, consMap) <- foldM registerCons   def conses
    let hasToplevelCons =
            not isNative && (not (null topFieldNames) || null realConses)
    if hasToplevelCons then do
        topFs <- for topFields $ \(names, tp) -> do
            namesVec <- SmallVector.fromList names
            IR.recordFields namesVec tp
        cons  <- IR.recordCons' className topFs
        return (cons : realConses, Map.insert className (fieldNames topFields) consMap)
    else return (realConses, consMap)

registerMethod :: IR.Qualified -> IR.Name -> [IR.Name] -> DefsMap -> IR.SomeTerm -> TC.Pass ClassProcessor DefsMap
registerMethod = \modName clsName paramNames map t -> do
    (doc, root) <- Sourcing.cutDoc t
    Layer.read @IR.Model root >>= \case
        Uni.Function n _ _ -> do
            IR.source n >>= Layer.read @IR.Model >>= \case
                Uni.Var name -> do
                    newRoot <- addSelf modName clsName paramNames
                                   $ Layout.unsafeRelayout root
                    IR.replace newRoot root
                    let documented = Documented doc (Def.Body newRoot)
                    when_ (isJust $ map ^. wrapped . at name) $ do
                        let error = Error.duplicateMethodDefinition
                                modName clsName name
                        Error.setError (Just error) newRoot
                    return $ map & wrapped . at name .~ Just documented
                _ -> return map
        _ -> return map

addSelf :: IR.Qualified -> IR.Name -> [IR.Name]
        -> IR.Term IR.Function -> TC.Pass ClassProcessor (IR.Term IR.Function)
addSelf = \modName clsName paramNames fun -> do
    IR.Function n as b <- IR.modelView fun
    tvars <- traverse IR.var paramNames
    tp    <- IR.resolvedCons modName clsName clsName tvars
    name  <- IR.source n
    args  <- traverse IR.source =<< ComponentVector.toList as
    body  <- IR.source b
    self  <- IR.var' "self"
    oldTp <- IR.source =<< Layer.read @IR.Type self
    IR.replace tp oldTp
    Layout.unsafeRelayout <$> IR.function name (self : args) body

