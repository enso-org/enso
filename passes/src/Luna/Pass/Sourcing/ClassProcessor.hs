{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Sourcing.ClassProcessor where

import Prologue

import qualified Data.Graph.Data.Component.Vector      as ComponentVector
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Map                              as Map
import qualified Data.Mutable.Storable.SmallAutoVector as SmallVector
import qualified Data.Vector.Storable.Foreign          as Vector
import qualified Luna.IR                               as IR
import qualified Luna.IR.Aliases                       as Uni
import qualified Luna.IR.Layer                         as Layer
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Attr                        as Attr
import qualified Luna.Pass.Basic                       as Pass
import qualified Luna.Pass.Sourcing.Data.Def           as Def
import qualified Luna.Pass.Sourcing.Utils              as Sourcing

import Data.Map (Map)
import Luna.Pass.Data.Root
import Luna.Pass.Sourcing.Data.Class
import Luna.Pass.Sourcing.Data.Def

data ClassProcessor

type instance Pass.Spec ClassProcessor t = ClassProcessorSpec t
type family ClassProcessorSpec t where
    ClassProcessorSpec (Pass.In  Pass.Attrs) = '[Root]
    ClassProcessorSpec (Pass.Out Pass.Attrs) = '[Root, Class]
    ClassProcessorSpec t = Pass.BasicPassSpec t


instance ( Pass.Interface ClassProcessor (Pass.Pass stage ClassProcessor)
         , IR.DeleteSubtree (Pass.Pass stage ClassProcessor)
         ) => Pass.Definition stage ClassProcessor where
    definition = do
        Root r <- Attr.get
        (r, cls) <- prepareClass $ Layout.unsafeRelayout r
        Attr.put $ Root r
        Attr.put cls

prepareClass ::
    ( Pass.Interface ClassProcessor m
    , IR.DeleteSubtree m
    ) => IR.Term IR.Record -> m (IR.SomeTerm, Class)
prepareClass klass = do
    IR.Record native name params conses defs <- IR.model klass
    parameters      <- traverse IR.source =<< ComponentVector.toList params
    constructors    <- traverse IR.source =<< ComponentVector.toList conses
    constructorsMap <- processConses native name constructors
    getters         <- generateGetters constructorsMap
    methods  <- traverse IR.source =<< ComponentVector.toList defs
    newKlass <- IR.record' native
                           name
                           parameters
                           constructors
                           (getters <> methods)
    IR.replace newKlass klass
    defsMap <- foldM registerMethod def (getters <> methods)
    return (newKlass, Class (Constructor . length <$> constructorsMap) defsMap)

type Fields = [Maybe IR.Name]
type ConsMap = Map IR.Name Fields

generateGetters :: Pass.Interface ClassProcessor m
                => ConsMap -> m [IR.SomeTerm]
generateGetters consMap = do
    case Map.toList consMap of
        [(consName, fields)] -> do
            let fieldsCount = length fields
            fmap catMaybes $ for (zip [0..] fields) $ \(ix, mayFname) ->
                for mayFname $ \fname -> do
                    generateGetter consName fieldsCount ix fname
        _ -> return []

generateGetter :: Pass.Interface ClassProcessor m
               => IR.Name -> Int -> Int -> IR.Name -> m IR.SomeTerm
generateGetter consName fieldsCount fieldPos fieldName = do
    retVar   <- IR.var  fieldName
    matchVar <- IR.var' fieldName
    selfVar  <- IR.var  $ convert "self"
    funName  <- IR.var  fieldName
    blanks   <- sequence $ take (fieldsCount - 1) $ repeat IR.blank'
    let matchers :: [IR.SomeTerm] =  take fieldPos blanks
                                  <> [matchVar]
                                  <> drop fieldPos blanks
    clauseCons <- IR.cons consName matchers
    clause     <- IR.lam clauseCons retVar
    match      <- IR.match selfVar [clause]
    getter     <- IR.function funName [] match
    doc        <- SmallVector.fromList "Field getter"
    documented <- IR.documented doc getter
    return $ Layout.relayout documented


registerCons :: Pass.Interface ClassProcessor m
             => ConsMap -> IR.SomeTerm -> m ConsMap
registerCons map root = Layer.read @IR.Model root >>= \case
    Uni.RecordCons n fs -> do
        fields <- traverse IR.source =<< ComponentVector.toList fs
        fs <- foldM registerFields def fields
        return $ Map.insert n fs map
    _ -> return map

registerFields :: Pass.Interface ClassProcessor m
               => Fields -> IR.SomeTerm -> m Fields
registerFields fs root = Layer.read @IR.Model root >>= \case
    Uni.RecordFields ns _ -> do
        names <- SmallVector.toList ns
        let fields = if null names then [Nothing] else Just <$> names
        return $ fs <> fields
    _ -> return fs

processConses :: Pass.Interface ClassProcessor m
            => Bool -> IR.Name -> [IR.SomeTerm] -> m ConsMap
processConses isNative className conses = do
    topFields  <- foldM registerFields def conses
    realConses <- foldM registerCons   def conses
    let hasToplevelCons =
            not isNative && (not (null topFields) || Map.null realConses)
        consMap = if hasToplevelCons
                      then Map.insert className topFields realConses
                      else realConses
    return consMap

registerMethod ::
    ( Pass.Interface ClassProcessor m
    , IR.DeleteSubtree m
    ) => DefsMap -> IR.SomeTerm -> m DefsMap
registerMethod map t = do
    (doc, root) <- Sourcing.cutDoc t
    Layer.read @IR.Model root >>= \case
        Uni.Function n _ _ -> do
            IR.source n >>= Layer.read @IR.Model >>= \case
                Uni.Var name -> do
                    newRoot <- addSelf $ Layout.unsafeRelayout root
                    IR.replace newRoot root
                    let documented = Documented doc (Def.Body newRoot)
                    return $ map & wrapped . at name .~ Just documented
                _ -> return map
        _ -> return map

addSelf :: Pass.Interface ClassProcessor m
        => IR.Term IR.Function -> m (IR.Term IR.Function)
addSelf fun = do
    IR.Function n as b <- IR.model fun
    name <- IR.source n
    args <- traverse IR.source =<< ComponentVector.toList as
    body <- IR.source b
    self <- IR.var' $ convert "self"
    Layout.unsafeRelayout <$> IR.function name (self : args) body

