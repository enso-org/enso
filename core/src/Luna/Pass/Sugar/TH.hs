{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}
-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE NoRecursiveDo #-}

module Luna.Pass.Sugar.TH where

import Prologue hiding (pprint, Type)

import Luna.IR.Internal.IR


import Data.Path
import Type.List (type(<>))
import Data.Families
import qualified Data.Char as Char
import qualified Language.Haskell.TH as TH




upperHead :: String -> String
upperHead (c:cs) = Char.toUpper c : cs


makePass :: Name -> Q [TH.Dec]
makePass name = build $ do
    let passName = typeName . upperHead $ nameBase name
    VarI _ (ForallT _ ctx (AppT (AppT (AppT (AppT (ConT _) _) _) (AppT _ (VarT elemt))) (VarT m))) Nothing <- lift $ reify name
    let f a = case a of
            VarT v -> if v /= elemt then (ConT $ mkName "AnyType") else ctxfold (singleFold1 f) a
            _      -> ctxfold (singleFold1 f) a
        ctx' = tuples $ ctxfold (singleFold1 f) <$> ctx
        passHeader = toType $ apps (ConT $ mkName "ElemScope") [th passName, VarT elemt]
        defReqOf r t f = define $ typeInstance r
                       [ toType $ typeName t
                       , passHeader
                       ] f
        defReq r t = defReqOf r t $ apps (th . typeName $ "Get" <> r) [th $ typeName t, ctx']
        defInputs  = defReq "Inputs"
        defOutputs = defReq "Outputs"
        defIOs lst = defInputs lst >> defOutputs lst
    -- define $ phantom passName                                                            -- data InitModel
    define $ typeInstance' "Abstract" passName passName                                  -- type instance Abstract InitModel = InitModel
    mapM_ defIOs ["Net", "Layer", "Attr"]                                                -- type instance Inputs  Net   (ElemScope InitModel t) = GetInputs  Net   (InitModelCtx t AnyType)
                                                                                         -- type instance Outputs Net   (ElemScope InitModel t) = GetOutputs Net   (InitModelCtx t AnyType)
                                                                                         -- ...
    defReqOf "Outputs" "Event" $ app (th $ typeName "GetEmitters") ctx'                  -- type instance Outputs Event (ElemScope InitModel t) = GetEmitters      (InitModelCtx t AnyType)
    define $ typeInstance "Inputs" [th (typeName "Event"), passHeader] ([] :: [Type])    -- type instance Inputs  Event (ElemScope InitModel t) = '[]
    define $ typeInstance' "Preserves" passHeader ([] :: [Type])                         -- type instance Preserves     (ElemScope InitModel t) = '[]
    define $ classInstance' "KnownElemPass" [passName] [function' "elemPassDescription"  -- instance KnownElemPass InitModel where
             (Clause [] (NormalB (VarE $ mkName "genericDescriptionP")) [])]             --     elemPassDescription = genericDescriptionP
    return []

makeLayerGen :: Name -> Name -> Q [TH.Dec]
makeLayerGen p name = build $ do
    -- let passName = typeName . upperHead $ nameBase name
    let passName = app (typeName "Init") (toTypeName p)
    VarI _ (ForallT _ ctx _) Nothing <- lift $ reify name
    let f a = case a of
            VarT v -> if (nameBase v) /= "t" then (ConT $ mkName "AnyType") else (VarT $ mkName "t")-- ctxfold (singleFold1 f) a
            _      -> ctxfold (singleFold1 f) a
        ctx' = tuples $ ctxfold (singleFold1 f) <$> ctx
        passHeader = toType $ apps (ConT $ mkName "ElemScope") [th passName, VarT $ mkName "t"]
        defReqOf r t f = define $ typeInstance r
                       [ toType $ typeName t
                       , passHeader
                       ] f
        defReq r t = defReqOf r t $ apps (th . typeName $ "Get" <> r) [th $ typeName t, ctx']
        defInputs  = defReq "Inputs"
        defOutputs = defReq "Outputs"
        defIOs lst = defInputs lst >> defOutputs lst
    -- define $ phantom passName                                                            -- data InitModel
    define $ typeInstance' "Abstract" passName passName                                  -- type instance Abstract InitModel = InitModel
    mapM_ defIOs ["Net", "Layer", "Attr"]                                                -- type instance Inputs  Net   (ElemScope InitModel t) = GetInputs  Net   (InitModelCtx t AnyType)
                                                                                         -- type instance Outputs Net   (ElemScope InitModel t) = GetOutputs Net   (InitModelCtx t AnyType)
                                                                                         -- ...
    defReqOf "Outputs" "Event" $ app (th $ typeName "GetEmitters") ctx'                  -- type instance Outputs Event (ElemScope InitModel t) = GetEmitters      (InitModelCtx t AnyType)
    define $ typeInstance "Inputs" [th (typeName "Event"), passHeader] ([] :: [Type])    -- type instance Inputs  Event (ElemScope InitModel t) = '[]
    define $ typeInstance' "Preserves" passHeader ([] :: [Type])                         -- type instance Preserves     (ElemScope InitModel t) = '[]
    define $ classInstance' "KnownElemPass" [passName] [function' "elemPassDescription"  -- instance KnownElemPass InitModel where
             (Clause [] (NormalB (VarE $ mkName "genericDescriptionP")) [])]             --     elemPassDescription = genericDescriptionP
    return []



makePass1 :: Name -> Q [TH.Dec]
makePass1 name = build $ do
    let passName = typeName . upperHead $ nameBase name
    VarI _ (ForallT _ ctx   (AppT (AppT ArrowT _)  (AppT (AppT (AppT (AppT (ConT _) _) _) (AppT _ (VarT elemt))) (VarT m)) )   ) Nothing <- lift $ reify name
    let f a = case a of
            VarT v -> if v /= elemt then (ConT $ mkName "AnyType") else ctxfold (singleFold1 f) a
            _      -> ctxfold (singleFold1 f) a
        ctx' = tuples $ ctxfold (singleFold1 f) <$> ctx
        passHeader = toType $ apps (ConT $ mkName "ElemScope") [th passName, VarT elemt]
        defReqOf r t f = define $ typeInstance r
                       [ toType $ typeName t
                       , passHeader
                       ] f
        defReq r t = defReqOf r t $ apps (th . typeName $ "Get" <> r) [th $ typeName t, ctx']
        defInputs  = defReq "Inputs"
        defOutputs = defReq "Outputs"
        defIOs lst = defInputs lst >> defOutputs lst
    -- define $ phantom passName                                                            -- data InitModel
    define $ typeInstance' "Abstract" passName passName                                  -- type instance Abstract InitModel = InitModel
    mapM_ defIOs ["Net", "Layer", "Attr"]                                                -- type instance Inputs  Net   (ElemScope InitModel t) = GetInputs  Net   (InitModelCtx t AnyType)
                                                                                         -- type instance Outputs Net   (ElemScope InitModel t) = GetOutputs Net   (InitModelCtx t AnyType)
                                                                                         -- ...
    defReqOf "Outputs" "Event" $ app (th $ typeName "GetEmitters") ctx'                  -- type instance Outputs Event (ElemScope InitModel t) = GetEmitters      (InitModelCtx t AnyType)
    define $ typeInstance "Inputs" [th (typeName "Event"), passHeader] ([] :: [Type])    -- type instance Inputs  Event (ElemScope InitModel t) = '[]
    define $ typeInstance' "Preserves" passHeader ([] :: [Type])                         -- type instance Preserves     (ElemScope InitModel t) = '[]
    define $ classInstance' "KnownElemPass" [passName] [function' "elemPassDescription"  -- instance KnownElemPass InitModel where
             (Clause [] (NormalB (VarE $ mkName "genericDescriptionP")) [])]             --     elemPassDescription = genericDescriptionP
    return []



tuples :: [Type] -> Type
tuples = foldl (\a b -> AppT (AppT (TupleT 2) a) b) (TupleT 0)
-- foldl :: (b -> a -> b) -> b -> t a -> b

newtype IsoCtxFold ctx = IsoCtxFold (forall a. ctx a => a -> a)

subFold :: ctx a => IsoCtxFold ctx -> a -> a
subFold (IsoCtxFold f) a = f a ; {-# INLINE subFold #-}

-- subReFold :: ctx a => IsoCtxFold ctx -> a -> a
-- subReFold (IsoCtxFold f) a =

-- | folding over 1-st Type level (no Kinds and higher level types)
ctxfold :: (ctx Type, ctx TyVarBndr, ctx Cxt, ctx Name, ctx TyLit)
           => IsoCtxFold ctx -> Type -> Type
ctxfold f t = case t of
    ForallT   ts ctx tp -> ForallT   (subFold f <$> ts) (subFold f ctx) (subFold f tp)
    AppT      a b	    -> AppT      (subFold f a) (subFold f b)
    SigT      t k       -> SigT      (subFold f t) (subFold f k)
    VarT      n         -> VarT      (subFold f n)
    ConT      n         -> ConT      (subFold f n)
    PromotedT n         -> PromotedT (subFold f n)
    InfixT    t n t'    -> InfixT    (subFold f t) (subFold f n) (subFold f t')
    UInfixT   t n t'    -> UInfixT   (subFold f t) (subFold f n) (subFold f t')
    ParensT   t         -> ParensT   (subFold f t)
    LitT      l         -> LitT      (subFold f l)
    _                   -> t

class                         SingleFold1 a b where runSingleFold1 :: (a -> a) -> (b -> b)
instance {-# OVERLAPPABLE #-} SingleFold1 a b where runSingleFold1 = const id ; {-# INLINE runSingleFold1 #-}
instance                      SingleFold1 a a where runSingleFold1 = id       ; {-# INLINE runSingleFold1 #-}

simpleFold :: forall ctx. (forall a. ctx a => a -> a) -> IsoCtxFold ctx
simpleFold = IsoCtxFold ; {-# INLINE simpleFold #-}

singleFold1 :: (a -> a) -> IsoCtxFold (SingleFold1 a)
singleFold1 f = simpleFold $ runSingleFold1 f ; {-# INLINE singleFold1 #-}

-- data Type Source #




--
--
--
-- VarI Luna.IR.ToRefactor.initModel
--     (ForallT [KindedTV t StarT,KindedTV m (AppT (AppT ArrowT StarT) StarT)]
--              [AppT (AppT (ConT InitModelCtx) (VarT t)) (VarT m)]
--              (AppT (AppT (ConT NewElemPassDef) (VarT t)) (VarT m))
--     ) Nothing





----------------------------------
