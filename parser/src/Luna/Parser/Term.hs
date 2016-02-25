{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Term where

--import Flowbox.Prelude hiding (maybe)

--import           Text.Parser.Combinators
--import qualified Luna.Parser.Token       as Tok
--import qualified Luna.Syntax.Expr as Expr
--import           Luna.Syntax.Expr (LExpr, Expr)
--import Luna.Parser.Builder (label, labeled, withLabeled, nextID)
--import Luna.Parser.Combinators (just, many1, (<??>), applyAll, maybe)
--import Luna.Parser.Struct (blockBegin)
--import Luna.Parser.Pattern (pattern)
--import           Luna.Syntax.Name             (vname)
--import qualified Luna.Syntax.Name.Path        as NamePath
--import           Data.Function                (on)
--import qualified Data.List                    as List
--import           Text.EditDistance            (defaultEditCosts, levenshteinDistance, EditCosts(..), Costs(..))
--import qualified Flowbox.Data.MapForest       as MapForest
--import qualified Data.Maps                    as Map
--import           Data.List                    (sortBy)
--import qualified Luna.Data.StructInfo         as StructInfo
--import qualified Luna.Data.Namespace.State    as Namespace
--import           Luna.Data.Namespace          (NamespaceMonad)
--import qualified Luna.Parser.State            as ParserState
--import           Luna.Parser.State            (ParserState)
--import           Luna.Data.StructInfo         (OriginInfo(OriginInfo))
--import Control.Monad.State (MonadState, get)
--import           Data.Maybe                   (isJust, fromJust)
--import qualified Luna.Syntax.Name.Pattern     as NamePat
--import           Luna.Syntax.Label  (Label(Label))
--import           Luna.Syntax.Name.Pattern     (NamePat(NamePat))

--import           Text.Parser.Expression (Assoc(AssocLeft), Operator(Infix, Prefix, Postfix), buildExpressionParser)
--import qualified Luna.Parser.Pattern as Pat
--import           Luna.Parser.Type    (typic, metaBase)
--import           Luna.Parser.Literal (literal)
--import qualified Luna.Syntax.Name    as Name
--import qualified Data.ByteString.UTF8         as UTF8
--import           Data.Char                    (isSpace)
--import qualified Luna.System.Session as Session
--import qualified Luna.System.Pragma.Store  as Pragma
--import           Luna.System.Pragma.Store  (MonadPragmaStore)
--import qualified Luna.System.Pragma        as Pragma (isEnabled)
--import qualified Luna.Parser.Pragma        as Pragma

--import qualified Luna.Parser.Decl          as Decl
--import qualified Luna.Syntax.Label         as Label

--import Text.Trifecta.Rendering (Caret(Caret))
--import Text.Trifecta.Combinators (DeltaParsing, careting)
--import Text.Trifecta.Delta (column)
--import Luna.Syntax.Enum (IDTag)

--import Luna.Parser.Indent (MonadIndent)

--type ParserMonad m = (MonadIndent m, MonadState ParserState m, DeltaParsing m, NamespaceMonad m, MonadPragmaStore m)

--prevParsedChar = do
--    Caret delta bs <- careting
--    let idx = max 0 . fromIntegral $ column delta - 1
--        txt = UTF8.toString bs
--    return $ if idx > length txt then '_' else txt !! idx

--lastLexemeEmpty = do
--    prevChar <- prevParsedChar
--    when (isSpace prevChar) $ fail "not empty"



--binary   name fun assoc = Infix   (Tok.reservedOp name *> return fun) assoc
--binaryM  name fun assoc = Infix   (Tok.reservedOp name *>        fun) assoc
--prefix   name fun       = Prefix  (Tok.reservedOp name *> return fun)
--prefixM  name fun       = Prefix  (Tok.reservedOp name *>        fun)
--prefixfM      fun       = Prefix  (fun)
--postfix  name fun       = Postfix (Tok.reservedOp name *> return fun)
--postfixM name fun       = Postfix (Tok.reservedOp name *>        fun)



--expr       = tlExpr entBaseE

----FIXME[wd]: exprSimple is broken - it includes func calls. Using pEntBaseSimpleE for now
----exprSimple = tlExpr pEntBaseSimpleE


---- === Top Level pattern, variable, record updates chains === --

--tlRecUpd     = assignSeg $ (\vop accs expr -> Expr.RecUpd vop [Expr.FieldUpd accs expr]) <$> Tok.varOp <*> many1 recAcc
--tlExprPat    = assignSeg $ Expr.Assignment <$> pattern
--tlExprPatVar = assignSeg $ Expr.Assignment <$> Pat.var

--assignSeg p = p <* Tok.assignment

--tlExprExtHead =   try tlExprPat
--              <|> try tlRecUpd

--tlExprBasicHead =  try tlExprPatVar
--               <|> try tlRecUpd

--tlExprParser head base =   try (labeled $ head <*> tlExprBasic base)
--                       <|> opTupleTE base

--tlExpr      = tlExprParser tlExprExtHead
--tlExprBasic = tlExprParser tlExprBasicHead

---- === / === --


--opE       = opTE entBaseE
--opTupleTE base = tupleE $ opTE base

--opTE base = buildExpressionParser optableE (appE base)

--tupleE p = p <??> ((\id xs x -> label id $ Expr.Tuple (x:xs)) <$> nextID <* Tok.separator <*> sepBy1 p Tok.separator)

----appE base = p <??> (appID (\i a s -> Expr.App i s a) <*> many1 (argE p)) where
--appE base = p <??> ((\i a s -> label i $ callBuilder2 s a) <$> nextID <*> many1 (appArg p)) where
--    p = termE base



--termE base = base <??> (flip applyAll <$> many1 (termBaseE base))  ------  many1 (try $ recUpdE))


--termBaseE p = choice [ accE
--                     , callTermE p
--                     ]

--recAcc  = (Tok.accessor *> Tok.varOp)

--accBaseE  = (Tok.accessor *> nameBase)

--nameBase =   (Name.VarName  <$> Tok.varOp)
--         <|> (Name.TypeName <$> Tok.conIdent)


--accE      = try( (\id a b -> label id $ Expr.Accessor a b) <$> nextID <*> accBaseE) -- needed by the syntax [1..10]




--parensE p = Tok.parens $ choice [ Expr.Meta    <$  Tok.meta <*> labeled metaBase
--                                , Expr.Grouped <$> p
--                                , Expr.Grouped <$> labeled (Expr.Tuple <$> pure [])
--                                ]

--callList     p = Tok.parens (sepBy p Tok.separator)
--callTermE p = (\id a b-> label id (Expr.app b a)) <$ lastLexemeEmpty <*> nextID <*> callList (appArg p)

--entBaseE :: ParserMonad m => m (LExpr IDTag ())
--entBaseE        = entConsE entComplexE

--pEntBaseSimpleE :: ParserMonad m => m (LExpr IDTag ())
--pEntBaseSimpleE = entConsE entSimpleE


--entConsE base = choice [ try $ labeled (parensE (tlExpr base))
--                       , base
--                       ]

--arg = Decl.arg pEntBaseSimpleE

--lambda = Expr.Lambda <$> lambdaSingleArg <*> pure Nothing <*> exprBlock
--lambdaSingleArg = (:[]) <$> labeled arg

--entComplexE = choice[ --labeled (Expr.Decl <$> labeled decl) -- FIXME: zrobic subparsowanie!
--                    entSimpleE
--                    ]
--             <?> "expression term"

--entSimpleE = choice[ caseE -- CHECK [wd]: removed try
--                   --, condE
--                   , labeled $ parensE expr
--                   , labeled $ try lambda
--                   , identE
--                   --, try (labeled Expr.RefType <$  Tok.ref <*> Tok.conIdent) <* Tok.accessor <*> Tok.varOp
--                   --, labeled $ Expr.Curry <$ Tok.curry <*> ParserState.withCurrying entSimpleE
--                   , labeled $ Expr.Curry <$ Tok.curry <*> labeled (Expr.Var <$> (Expr.Variable <$> Tok.varIdent <*> pure ()))
--                   , labeled $ Expr.Lit <$> literal
--                   , labeled $ listE
--                   --, labeled $ Expr.Native  <$> nativeE
--                   ]
--           <?> "expression term"

--optableE = [
--             [ operator4 "^"                                  AssocLeft ]
--           , [ operator4 "*"                                  AssocLeft ]
--           , [ operator4 "/"                                  AssocLeft ]
--           , [ operator4 "+"                                  AssocLeft ]
--           , [ operator4 "-"                                  AssocLeft ]
--           , [ operator4 "<"                                  AssocLeft ]
--           , [ operator4 ">"                                  AssocLeft ]
--           , [ operator4 "=="                                 AssocLeft ]
--           , [ operator4 "in"                                 AssocLeft ]
--           , [ binaryM   "$"  (callBuilder <$> nextID)        AssocLeft ]
--           , [ postfixM  "::" ((\id a b -> label id (Expr.Typed a b)) <$> nextID <*> typic) ]
--           ]
--           where
--              --operator op = binaryM op (binaryMatchE <$> (appID Expr.Infix <*> pure op))
--              --operator op = binaryM op (binaryMatchE <$> (appID Expr.Infix <*> pure op))
--              --operator4 op = binaryM op ( (\id1 id2 l r -> label id1 $ Expr.appInfix (label id2 $ Expr.Var $ NamePath.single $ op) l [r]
--              --                            ) <$> nextID <*> nextID)

--              --FIXME[wd]: remove fromText call after moving Tokenizer to Text
--              operator4 op = binaryM (fromText op) ( (\id1 id2 l r -> label id1 $ Expr.appInfix (label id2 $ Expr.Var $ Expr.Variable (vname $ NamePath.single op) ()) (Expr.unnamed l) [Expr.unnamed r]
--                                                   ) <$> nextID <*> nextID)
--              --operator op = binaryM op (binaryMatchE <$> (appID Expr.Infix <*> pure ('~':op)))
--              --operator2 op = binaryM op (binaryMatchE <$>  ( appID Expr.App <*> (appID Expr.Accessor <*> pure "add" <*> ... ) )  )
--              --operator2 op = binaryM op ( (\id1 id2 x y -> Expr.App id1 (Expr.Accessor id2 op x) [y]) <$> genID <*> genID)
--              --operator3 op = binaryM op ( (\id1 id2 x y -> Expr.App id1 (Expr.Accessor id2 "contains" y) [x]) <$> genID <*> genID)

----callBuilder id src@(Label lab expr) arg = label id $ case expr of
----    Expr.App src' (Expr.Seq args) -> Expr.App src' (Expr.Seq $ args ++ [Expr.Unnamed arg])
----    _                             -> Expr.App src (Expr.Seq $ [Expr.Unnamed arg])

----callBuilder2 src@(Label lab expr) argsx = case expr of
----    Expr.App src' (Expr.Seq args) -> Expr.App src' (Expr.Seq $ args ++ argsx)
----    _                             -> Expr.App src  (Expr.Seq argsx)

--callBuilder id src@(Label lab expr) arg = label id $ case expr of
--    Expr.App app -> Expr.App $ NamePat.appendLastSegmentArgs args app
--    _             -> Expr.app src args
--    where args = [Expr.unnamed arg]

--callBuilder2 src@(Label lab expr) args = case expr of
--    Expr.App app -> Expr.App $ NamePat.appendLastSegmentArgs args app
--    _             -> Expr.app src args


---- parse all patterns starting with the longest match and going down
---- if everything fails, try parsing again the longest one to show nice error message
--mkFuncParsers (a:as) x =   try (foldl (\a b -> try a <|> b) (mkFuncParser x a) (fmap (mkFuncParser x) as))
--                       <|> mkFuncParser x a -- nice error messages


--appArg p = try (Expr.AppArg <$> just Tok.varIdent <* Tok.assignment <*> p) <|> (Expr.AppArg Nothing <$> p)

----mkFuncParser baseVar (id, mpatt) = case mpatt of
----    Nothing                                       -> baseVar
----    Just patt@(NamePat.NamePatDesc pfx base segs) -> ParserState.withReserved (tail segNames)
----                                                   $ labeled $ Expr.App <$> pattParser
----        where NamePat.SegmentDesc baseName baseDefs = base
----              baseParser                                = NamePat.Segment <$> baseMultiVar <*> defBaseParser baseDefs
----              segParser (NamePat.SegmentDesc name defs) = NamePat.Segment <$> Tok.symbol name <*> defSegParser defs

----              defSegParser defs  = fmap takeJustArgs $ parseSegArgs defs
----              defBaseParser defs = fmap takeJustArgs $ parseBaseArgs defs
----              takeJustArgs       = fmap fromJust . filter isJust
----              argSimpleExpr      = appArg pEntBaseSimpleE
----              argCmplexExpr      = appArg (opTE pEntBaseSimpleE)
----              segNames           = NamePat.segmentNames patt
----              pattParser         = NamePat Nothing <$> baseParser   <*> mapM segParser segs
----              baseMultiVar       = labeled . pure $ Expr.Var $ Expr.Variable (vname $ NamePat.toNamePath patt) ()
----              condParser p req   = if req then just  p
----                                          else maybe p
----              parseBaseArgs args  = mapM (condParser argCmplexExpr) args
----              parseSegArgs []     = pure []
----              parseSegArgs (a:[]) = (:[]) <$> condParser argSimpleExpr a
----              parseSegArgs (a:as) = (:) <$> condParser argCmplexExpr a <*> parseSegArgs as



--mkFuncParser baseVar (id, mpatt) = case mpatt of
--    Nothing                                       -> baseVar
--    Just patt@(NamePat.NamePatDesc pfx base segs) -> ParserState.withReserved (tail segNames)
--                                                   $ labeled $ Expr.App <$> pattParser
--        where NamePat.SegmentDesc baseName baseDefs = base
--              baseParser                                = NamePat.Segment <$> baseMultiVar <*> defBaseParser baseDefs
--              segParser (NamePat.SegmentDesc name defs) = NamePat.Segment <$> Tok.symbol name <*> defSegParser defs

--              defSegParser defs  = fmap takeJustArgs $ parseSegArgs defs
--              defBaseParser defs = fmap takeJustArgs $ parseBaseArgs defs
--              takeJustArgs       = fmap fromJust . filter isJust
--              argSimpleExpr      = appArg pEntBaseSimpleE
--              argCmplexExpr      = appArg pEntBaseSimpleE
--              segNames           = NamePat.segmentNames patt
--              pattParser         = NamePat Nothing <$> baseParser   <*> mapM segParser segs
--              baseMultiVar       = labeled . pure $ Expr.Var $ Expr.Variable (vname $ NamePat.toNamePath patt) ()
--              condParser p req   = if req then just  p
--                                          else maybe p
--              parseBaseArgs args  = mapM (condParser argCmplexExpr) args
--              parseSegArgs []     = pure []
--              parseSegArgs (a:[]) = (:[]) <$> condParser argSimpleExpr a
--              parseSegArgs (a:as) = (:) <$> condParser argCmplexExpr a <*> parseSegArgs as


--notReserved p = do
--    rsv  <- view ParserState.adhocReserved <$> get
--    name <- p
--    if name `elem` rsv then fail $ fromText $ "'" <> name <> "' is a reserved word"
--                       else return name


-----
--varE   = do
--    name <- try $ notReserved Tok.varIdent
--    ast  <- lookupAST name

--    case ast of
--        --Just possibleDescs -> fail $ show possibleDescs
--        Just possibleDescs -> mkFuncParsers possibleDescs (labeled . pure $ Expr.Var $ Expr.Variable (vname $ NamePath.single name) ())
--        Nothing            -> withLabeled $ \id -> do
--                                  let np = NamePath.single name
--                                  path <- ParserState.getModPath
--                                  Namespace.regVarName (OriginInfo path id) np
--                                  return $ Expr.Var $ Expr.Variable (vname np) ()



--lookupAST name = do
--    scope      <- ParserState.getScope
--    structInfo <- ParserState.getStructInfo
--    pid        <- ParserState.getPid
--    let argPatts = view StructInfo.argPats structInfo

--    case Map.lookup pid scope of
--            Nothing                    -> fail "Internal parser error [1]"
--            Just (StructInfo.Scope varnames typenames) -> do
--                let possibleElems = reverse $ sortBy (compare `on` (length . fst))
--                                  $ MapForest.subElems name varnames
--                    possibleIDs   = fmap (view StructInfo.target . snd) possibleElems
--                    possiblePatts = fmap (flip Map.lookup argPatts) possibleIDs
--                    possibleDescs = zip possibleIDs possiblePatts

--                case possibleDescs of
--                    [] -> if (name == "self")
--                          then return Nothing
--                          else do
--                              allowOrphans <- Pragma.lookup Pragma.orphanNames
--                              case fmap Pragma.isEnabled allowOrphans of
--                                  Right True -> return Nothing
--                                  _          -> fail . fromText $ "name '" <> name <> "' is not defined" <> msgTip
--                                  where scopedNames = "self" : ((fmap $ mjoin " ") $ MapForest.keys varnames)
--                                        simWords    = findSimWords name scopedNames
--                                        msgTip      = if length simWords > 0 then ", perhaps you ment one of {" <> mjoin ", " (fmap (fromString . show) simWords) <> "}"
--                                                                          else ""
--                    x  -> return $ Just x




--editCosts = EditCosts { deletionCosts      = ConstantCost 10
--                      , insertionCosts     = ConstantCost 10
--                      , substitutionCosts  = ConstantCost 10
--                      , transpositionCosts = ConstantCost 10
--                      }

--editCosts2 = EditCosts { deletionCosts     = ConstantCost 10
--                      , insertionCosts     = ConstantCost 1
--                      , substitutionCosts  = ConstantCost 10
--                      , transpositionCosts = ConstantCost 3
--                      }

--findSimWords word words = fmap snd simPairs
--    --where dist a b = levenshteinDistance editCosts (phonix a) (phonix b)
--    where dist a b = levenshteinDistance editCosts2 (toString a) (toString b)
--          simWords = fmap (dist word) words
--          simPairs = filter ((<20).fst)
--                   $ List.sortBy (compare `on` fst)
--                   $ zip simWords words



----varE   = appID $ Expr.var <*> Tok.varIdent
--varOpE = labeled $ (Expr.Var . (flip Expr.Variable ()) . vname . NamePath.single)  <$> try (Tok.parens Tok.varOp)
--conE   = labeled $ Expr.Cons <$> Tok.conIdent

--identE = choice [ varE
--                , varOpE
--                , conE
--                ]

-----



--listE = Expr.List <$> Tok.brackets listTypes

--listTypes = choice [ try $ Expr.RangeList <$> rangeList opE
--                   ,       Expr.SeqList   <$> sepBy opE Tok.separator
--                   ]

--rangeList p =   (Expr.Geometric <$> p <* Tok.range <*> p <*> endLimit)
--            <|> (Expr.Linear    <$> p <*> endLimit)
--            where endLimit = try (Tok.range *> just p) <|> pure Nothing

--caseE     = labeled (Expr.Case <$ Tok.kwCase <*> pEntBaseSimpleE <*> (blockBegin caseBodyE <|> return []))
--caseBodyE = labeled (Expr.Match <$> pattern <*> exprBlock)


----condE     = appID Expr.Cond <* Tok.kwIf <*> exprSimple <*> exprBlock <*> maybe (indBlockSpacesIE *> Tok.kwElse *> exprBlock)


--            --nativeE     = Tok.betweenNative (many nativeElemE)
--            --nativeElemE = choice [ nativeVarE
--            --                     , nativeCodeE
--            --                     ]
--            --nativeCodeE = appID Expr.NativeCode <*> ((:) <$> (noneOf "`#") <*> nativeCodeBodyE)
--            --nativeVarE  = appID Expr.NativeVar  <*  symbol "#{" <*> many (noneOf "}") <* symbolic '}'

--            --nativeCodeBodyE = (try(lookAhead $ string "#{")  *> pure [])
--            --              <|> (try(lookAhead $ string "```") *> pure [])
--            --              <|> ((++) <$> ((:) <$> anyChar <*> many (noneOf "`#")) <*> nativeCodeBodyE)


--exprBlock  = blockBegin expr
