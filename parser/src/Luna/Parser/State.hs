{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Parser.State where

--import           Flowbox.Prelude
--import qualified Luna.Data.ASTInfo    as ASTInfo
--import           Luna.Data.ASTInfo    (ASTInfo)
--import           Luna.Data.SourceMap  (SourceMap)
--import qualified Luna.Data.SourceMap  as SourceMap
--import           Luna.Parser.Operator (OperatorMap)
--import qualified Luna.Data.Namespace  as Namespace
--import           Luna.Data.Namespace  (Namespace, NamespaceMonad)
--import qualified Data.List            as List
--import qualified Data.Maps            as Map
----import           Luna.DEP.AST.Comment     (Comment(..))
--import           Flowbox.Control.Monad.State (mapStateVal, get, put, StateT)
--import qualified Flowbox.Control.Monad.State as State
--import qualified Luna.Data.StructInfo        as StructInfo
--import           Luna.Syntax.Name.Path       (QualPath)

--data ParserState= ParserState { _info          :: ASTInfo
--                              , _opFixity      :: OperatorMap
--                              , _sourceMap     :: SourceMap
--                              , _namespace     :: Namespace
--                              , _adhocReserved :: [Text]
--                              , _modPath       :: QualPath
--                              } deriving (Show)

--makeLenses ''ParserState


--------------------------------------------------------------------------
---- Utils
--------------------------------------------------------------------------

--mk :: ASTInfo -> ParserState
--mk i = def & info .~ i

--addReserved words = adhocReserved %~ (++words)
--delReserved words = adhocReserved %~ (flip (foldl (flip List.delete)) words)

--lastID            = view (info . ASTInfo.lastID)
----addComment cmt s  = s & comments %~ Map.insertWith (++) (lastID s) [cmt]

----registerComment = mapStateVal . addComment . Comment



--regParent id pid = mapStateVal $ namespace %~ Namespace.regParent id pid

--pushNewScope id = mapStateVal $ namespace %~ Namespace.pushNewScope id
--pushScope    id = mapStateVal $ namespace %~ Namespace.pushScope id
--popScope        = mapStateVal $ namespace %~ Namespace.popScope

--regVarName id name = do
--    pid <- getPid
--    withStructInfo $ StructInfo.regVarName pid id name

--regTypeName id name = do
--    pid <- getPid
--    withStructInfo $ StructInfo.regTypeName pid id name

--withStructInfo f = mapStateVal (namespace . Namespace.info %~ f)

--getStructInfo = view (namespace . Namespace.info) <$> get

--withReserved words p = do
--    s <- get
--    let reserved = view adhocReserved s
--    put $ (addReserved words s)
--    ret <- p
--    s   <- get
--    put (s & adhocReserved .~ reserved)
--    return ret

--withCurrying p = p


--withNewScope id p = do
--    pushNewScope id
--    ret <- p
--    popScope
--    return ret


--withScope id p = do
--    pushScope id
--    ret <- p
--    popScope
--    return ret

--getModPath = view modPath <$> get

--setModPath mp = do
--    s <- get
--    put $ set modPath mp s

--getPid = do
--    mpid <- Namespace.head . view namespace <$> get
--    case mpid of
--        Nothing  -> fail "Internal parser error. Cannot optain pid."
--        Just pid -> return pid

--getScope  = view (namespace . Namespace.info . StructInfo.scope) <$> get
----getASTMap = view (namespace . Namespace.info . StructInfo.ast) <$> get



--registerID id = do
--    pid <- getPid
--    regParent id pid

--------------------------------------------------------------------------
---- Instances
--------------------------------------------------------------------------

---- FIXME[wd]: "Unnamed" string is an ugly hack for now
--instance conf~() => Default ParserState where
--        def = ParserState def def def def def "Unnamed"


--instance (Functor m, Monad m) => NamespaceMonad (StateT ParserState m) where
--    get = view namespace <$> State.get
--    put ns = do
--        s <- get
--        State.put $ set namespace ns s


