{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE DeriveDataTypeable        #-}

{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs            #-}


module Luna.Parser.Parser where

import Luna.Prelude hiding (init)

import           Text.Parser.Combinators
import qualified Data.ByteString.UTF8         as UTF8
import Control.Monad.State (get, evalStateT)

import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.ByteString as ByteStr
import           Text.PrettyPrint.ANSI.Leijen (displayIO, linebreak, renderPretty)
import qualified Text.PrettyPrint.ANSI.Leijen as Leijen

import           Text.Trifecta.Delta (Delta(Directed))
import           Text.Trifecta.Result (Result(Failure, Success), _errDoc)
import qualified Text.Trifecta.Parser as Trifecta



import qualified Luna.Parser.State  as ParserState
import           Luna.Parser.State  (ParserStateT)
import           Luna.Parser.Indent (IndentT)
import qualified Luna.Parser.Indent as Indent

import qualified Data.List as List

import qualified Luna.Parser.Literal  as Literal
import qualified Luna.Parser.Layout   as Layout
--import qualified Luna.Parser.Expr     as Expr
import qualified Luna.Parser.Module   as Module

import qualified Luna.Parser.Token  as Tok
import qualified Luna.Parser.Token.Layout  as Tok


import Text.Trifecta.Combinators (DeltaParsing(..))
import Text.Parser.Token         (TokenParsing)

import Luna.Parser.Class (Parser, runParser)


-------------------------------------------------------------
---- Utils
-------------------------------------------------------------

parserName = "Luna Compiler"

run :: Monad m => IndentT (ParserStateT m) a -> ParserState.State -> m a
run p st = ParserState.evalT (Indent.evalT' p) st
----run p st = fmap fst $ Pragma.runT (evalStateT (Indent.parser p) st) mempty

handleResult r = case r of
    Failure e -> Left (_errDoc e)
    Success a -> Right a

bundleResult p = (,) <$> p <*> ParserState.get

end = (Tok.spaces <?> "") <* (eof <?> "")

upToEnd p = Tok.spaces *> p <* end

--renderErr e = renderPretty 0.8 80 $ e Leijen.<> linebreak

-------------------------------------------------------------
---- Pragmas
-------------------------------------------------------------

--appConf = id

---- FIXME[wd]: logika powina byc przeniesiona na system pluginow
--defConfig = appConf def
---- FIXME[wd]: debugowo ustawione wartosci typow
emptyState = def :: ParserState.State
defState  = emptyState


----st = def {State._conf = conf}




-------------------------------------------------------------
---- Section parsing
-------------------------------------------------------------
---- Usage example: parseExpr (fileFeed "test.txt")
parseGen :: (Monad p, p ~ Parser) => IndentT (ParserStateT p) a -> ParserState.State -> p (a, ParserState.State)
parseGen p st = run (bundleResult p) st -- TU BYL `(Module.unit p)` zamiast `p`
--parseGen2 p st = run (bundleResult p) st

--exprTestParser = parseGen (upToEnd expr)


----moduleParser modPath = parseGen (upToEnd $ func)
--moduleParser qPath = parseGen (upToEnd $ Module.pUnit $ Module.pModule qPath)
----exprParser           = parseGen (upToEnd expr)
--exprBlockParser      = parseGen (upToEnd $ Struct.indBlock Term.expr)
--exprBlockParser2     = parseGen2 (Tok.spaces *> ((Struct.indBlock Term.expr <* end) <|> (end *> pure [])))
--exprParser           = parseGen  (upToEnd Term.expr)
--exprParser2          = parseGen2 (upToEnd Term.expr)
----patternParser        = parseGen (upToEnd pattern)
----typeParser           = parseGen (upToEnd typeT)

-------------------------------------------------------------
---- Input utils
-------------------------------------------------------------

parserDelta name = Directed (UTF8.fromString name) 0 0 0 0

parseFromByteString = Trifecta.parseByteString

--parseFromText p delta txt = Trifecta.parseByteString p delta (convert $ Text.encodeUtf8 txt)

parseFromString p delta input = parseFromByteString p delta (UTF8.fromString input)

--parseFromFile p delta path = do
--  s <- liftIO $ ByteStr.readFile path
--  return $ parseFromByteString p delta s

--newtype MyParser a = MyParser { fromMyParser :: Trifecta.Parser a } deriving (MonadPlus, DeltaParsing, TokenParsing, Tok.CharParsing, Parsing, Alternative, Applicative, Functor, Monad)
--newtype MyParser2 m a = MyParser2 { fromMyParser :: Trifecta.Parser (m a) }

--instance MonadTrans       MyParser2
--instance Functor         (MyParser2 m)
--instance Applicative     (MyParser2 m)
--instance Alternative     (MyParser2 m)
--instance Monad m => Monad           (MyParser2 m) where
--    return = MyParser2 ∘ return ∘ return
--    (MyParser2 tma) >>= f = do
--        ma <- tma
--        do
--            a <- ma
--            let tma' = fromMyParser $ f a


--instance Monad m => MonadPlus       (MyParser2 m)
--instance TokenParsing    (MyParser2 m)
--instance Tok.CharParsing (MyParser2 m) where
--    satisfy = undefined
--    char = undefined
--    notChar = undefined
--    anyChar = undefined
--    string = undefined
--    text = undefined

--instance Parsing         (MyParser2 m) where
--    try = undefined
--    (<?>) = undefined
--    skipMany = undefined
--    skipSome = undefined
--    unexpected = undefined
--    eof = undefined
--    notFollowedBy = undefined

--instance Monad m => DeltaParsing (MyParser2 m) where
--    line       = undefined
--    position   = undefined
--    rend       = undefined
--    restOfLine = undefined
--    slicedWith f (fromMyParser -> tma) = MyParser2 $ slicedWith f' tma where
--        f' ma bs = do
--            a <- ma
--            return $ f a bs

--slicedWith :: (a -> ByteString -> r) -> m a -> m r

--foo :: _ => MyParser a -> _
--foo :: _ => _
--foo = Trifecta.parseByteString

----parseFile       path  p = handleResult <$> parseFromFile       p (parserDelta parserName) path
parseString :: String -> Parser a -> Either Leijen.Doc a
parseString     input p = handleResult  $  parseFromString     (runParser p) (parserDelta parserName) input


--parseString2 :: String -> MyParser a -> Either Leijen.Doc a
--parseString2 input p = handleResult  $  parseFromString (fromMyParser p) (parserDelta parserName) input
----parseByteString input p = handleResult  $  parseFromByteString p (parserDelta parserName) input


--parseFile   path  = handleParsingM (\p delta -> parseFromFile   p delta path)
--parseString input = handleParsing  (\p delta -> parseFromString p delta input)
--parseText   input = handleParsing  (\p delta -> parseFromText   p delta input)

---- handleParsing is used to put information into the Trifecta
---- because Trifecta does not provide monad transformer!
--handleParsingM f p = do
--    pragmas <- Pragma.get
--    rx <- handleResult <$> f (Pragma.runT p pragmas) (parserDelta parserName)
--    case rx of
--        Left e                -> return $ Left e
--        Right (res, pragmas') -> Right res <$ Pragma.put pragmas'

--handleParsing f p = do
--    pragmas <- Pragma.get
--    case handleResult $ f (Pragma.runT p pragmas) (parserDelta parserName) of
--        Left e                -> return $ Left e
--        Right (res, pragmas') -> Right res <$ Pragma.put pragmas'


----parseByteString2 p input = handleResult  $  parseFromByteString p (parserDelta parserName) input
----parseText2 p input = handleResult  $  parseFromText p (parserDelta parserName) input
--                --data AliasAnalysis = AliasAnalysis

--                --traverseM        = AST.traverseM        AliasAnalysis
--                --defaultTraverseM = AST.defaultTraverseM AliasAnalysis

--testme ast st = ast -- runState (traverseM ast) st


-------------------------------------------------------------
---- Initialization
-------------------------------------------------------------

--init = Pragma.init
