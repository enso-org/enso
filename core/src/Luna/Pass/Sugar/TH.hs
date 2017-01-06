{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}

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


getFuncResultAndLvl :: Type -> (Int,Type)
getFuncResultAndLvl = getFuncResultAndLvl' 0 ; {-# INLINE getFuncResultAndLvl #-}

getFuncResultAndLvl' :: Int -> Type -> (Int, Type)
getFuncResultAndLvl' depth = \case
    AppT (AppT ArrowT _) t -> getFuncResultAndLvl' (succ depth) t
    t                      -> (depth, t)
{-# INLINE getFuncResultAndLvl' #-}

makePass :: Name -> Q [TH.Dec]
makePass name = build $ do
    let passName = typeName . upperHead $ nameBase name
    VarI _ (ForallT _ ctx header) Nothing <- lift $ reify name
    let (depth, tp) = getFuncResultAndLvl header
    base <- case tp of
        AppT (AppT (AppT (ConT n) _) base) _ -> if nameBase n == "Listener"
                                                then return base else fail $ "Function `" <> nameBase name <> "` result is not a Listener"
        _                                    -> fail $ "Function `" <> nameBase name <> "` result is not a Listener"

    t <- case base of
        AppT (ConT c) (VarT t) -> if nameBase c == "Elem" then return t else lift $ newName "t"
        _                      -> lift $ newName "t"
    let aliasCheck = (== t)
        f a = case a of
            VarT v -> if aliasCheck v then VarT t else ConT (mkName "AnyType")
            _      -> ctxfold (singleFold1 f) a
        ctx' = tuples $ ctxfold (singleFold1 f) <$> ctx
        passHeader = toType $ apps (ConT $ mkName "ElemScope") [th passName, VarT t]
        defReqOf r t f = define $ typeInstance r
                       [ toType $ typeName t
                       , passHeader
                       ] f
        defReq r t = defReqOf r t $ apps (th . typeName $ "Get" <> r) [th $ typeName t, ctx']
        defInputs  = defReq "Inputs"
        defOutputs = defReq "Outputs"
        defIOs lst = defInputs lst >> defOutputs lst
    -- lift $ runIO $ print $ "!!! " <> show base
    define $ phantom passName                                                            -- data InitX
    define $ typeInstance' "Abstract" passName passName                                  -- type instance Abstract InitX = InitX
    mapM_ defIOs ["Net", "Layer", "Attr"]                                                -- type instance Inputs  Net   (ElemScope InitX t) = GetInputs  Net   (InitXCtx t AnyType)
                                                                                         -- type instance Outputs Net   (ElemScope InitX t) = GetOutputs Net   (InitXCtx t AnyType)
                                                                                         -- ...
    defReqOf "Outputs" "Event" $ app (th $ typeName "GetEmitters") ctx'                  -- type instance Outputs Event (ElemScope InitX t) = GetEmitters      (InitXCtx t AnyType)
    define $ typeInstance "Inputs" [th (typeName "Event"), passHeader] ([] :: [Type])    -- type instance Inputs  Event (ElemScope InitX t) = '[]
    define $ typeInstance' "Preserves" passHeader ([] :: [Type])                         -- type instance Preserves     (ElemScope InitX t) = '[]
    define $ classInstance' "KnownElemPass" [passName] [function' "elemPassDescription"  -- instance KnownElemPass InitX where
             (Clause [] (NormalB (VarE $ mkName "genericDescriptionP")) [])]             --     elemPassDescription = genericDescriptionP
    define $ function' (nameBase name <> "Pass") $ Clause []                             -- initXPass = fmap (fmap (... [depth] tpElemPass (Proxy :: Proxy InitX))) initX
             (NormalB (th $ app (fmapsE depth $ th $ app (th $ varName "tpElemPass") (proxyE $ th passName)) (VarE name) ))
             []
    return []


fmapsE :: Int -> Exp -> Exp
fmapsE 0 e = e
fmapsE n e = AppE (VarE $ mkName "fmap") (fmapsE (pred n) e)

proxyE t = SigE (ConE $ mkName "Proxy") $ AppT (ConT $ mkName "Proxy") (ConT t)




tuples :: [Type] -> Type
tuples = foldl (\a b -> AppT (AppT (TupleT 2) a) b) (TupleT 0)
-- foldl :: (b -> a -> b) -> b -> t a -> b

newtype IsoCtxFold ctx = IsoCtxFold (forall a. ctx a => a -> a)

subFold :: ctx a => IsoCtxFold ctx -> a -> a
subFold (IsoCtxFold f) a = f a ; {-# INLINE subFold #-}

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


----------------------------------
