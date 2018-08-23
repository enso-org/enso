{-# LANGUAGE NoStrict #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Attoparsec.Text32 where

import Prelude hiding (getChar, succ, take, takeWhile)

import qualified Data.Attoparsec.Internal       as Parsec
import qualified Data.Attoparsec.Internal.Types as Parsec
import qualified Data.Text32                    as Text32

import Control.Applicative ((<$>))
import Control.Monad       (when)
import Data.Coerce         (coerce)
import Data.List           (intercalate)
import Data.Text32         (Text32)

import Data.Convert


-- === Config === --

type Tokens = Text32
type Token  = Char


-- === Definitions === --

type Parser      = Parsec.Parser  Tokens
type Result      = Parsec.IResult Tokens
type Failure   r = Parsec.Failure Tokens Tokens   r
type Success a r = Parsec.Success Tokens Tokens a r

type instance Parsec.State Tokens = Tokens

instance Parsec.Chunk Text32 where
    type ChunkElem Tokens = Token
    nullChunk             = Text32.null
    pappendChunk          = (<>)
    atBufferEnd     (~_)  = toPos . Text32.length
    bufferElemAt    _ i b = (,1) <$> Text32.index b (fromPos i)
    chunkElemToChar _     = id
    {-# INLINE nullChunk       #-}
    {-# INLINE pappendChunk    #-}
    {-# INLINE atBufferEnd     #-}
    {-# INLINE bufferElemAt    #-}
    {-# INLINE chunkElemToChar #-}


-- === Parsec.Pos coertions === --

fromPos :: Parsec.Pos -> Int
fromPos = coerce
{-# INLINE fromPos #-}

toPos :: Int -> Parsec.Pos
toPos = coerce
{-# INLINE toPos   #-}


-- === Lookahead === --

tryPeekToken :: Parser (Maybe Token)
tryPeekToken = Parsec.Parser $ \t pos more _lose succ -> if
     | pos < lengthOf t -> let !c = Text32.unsafeIndex t (fromPos pos)
                            in succ t pos more (Just c)
     | more == Parsec.Complete -> succ t pos more Nothing
     | otherwise        ->
       let succ' t' pos' more' =
             let !c = Text32.unsafeIndex t' (fromPos pos')
             in succ t' pos' more' (Just c)
           lose' t' pos' more' = succ t' pos' more' Nothing
       in Parsec.prompt t pos more lose' succ'
{-# INLINE tryPeekToken #-}

peekToken :: Parser Token
peekToken = do
    (!_, !t) <- ensure 1
    pure $! Text32.unsafeHead t
{-# INLINE peekToken #-}


-- === Primitive parsers === --

satisfy :: (Token -> Bool) -> Parser Token
satisfy = \p -> do
  h <- peekToken
  if p h then h <$ advance 1
         else fail "satisfy"
{-# INLINE satisfy #-}

skip :: (Token -> Bool) -> Parser ()
skip = \p -> do
  h <- peekToken
  if p h then advance 1
         else fail "skip"
{-# INLINE skip #-}

satisfyWith :: (Token -> a) -> (a -> Bool) -> Parser a
satisfyWith = \f p -> do
  h <- peekToken
  let c = f h
  if p c then c <$ advance 1
         else fail "satisfyWith"
{-# INLINE satisfyWith #-}

takeWith :: Int -> (Tokens -> Bool) -> Parser Tokens
takeWith = \n p -> do
  (k, s) <- ensure n
  if p s then advance k >> return s
         else fail "takeWith"
{-# INLINE takeWith #-}

-- | Consume exactly @n@ tokens of input.
take :: Int -> Parser Tokens
take = \n -> takeWith (max n 0) (const True)
{-# INLINE take #-}

-- -- | @string s@ parses a sequence of tokens that identically match
-- -- @s@. Returns the parsed string (i.e. @s@).  This parser consumes no
-- -- input if it fails (even if a partial match).
tokens :: Tokens -> Parser Tokens
tokens = \s -> tokens_ (tokensSuspended id) id s
{-# INLINE tokens #-}

tokens_ ::
    (forall r. Tokens -> Tokens -> Tokens -> Parsec.Pos -> Parsec.More
                      -> Failure r -> Success Tokens r -> Result r)
    -> (Tokens -> Tokens)
    -> Tokens -> Parser Tokens
tokens_ suspended f s0 = Parsec.Parser $ \t pos more lose succ ->
  let s  = f s0
      ft = f (Text32.unsafeDrop (fromPos pos) t)
  in case Text32.commonPrefixes s ft of
       Nothing
         | Text32.null s  -> succ t pos more mempty
         | Text32.null ft -> suspended s s t pos more lose succ
         | otherwise      -> lose t pos more [] "string"
       Just (pfx,ssfx,tsfx)
         | Text32.null ssfx   -> let l = toPos (Text32.length pfx)
                                 in succ t (pos + l) more (substring pos l t)
         | not (Text32.null tsfx) -> lose t pos more [] "string"
         | otherwise              -> suspended s ssfx t pos more lose succ
{-# INLINE tokens_ #-}

tokensSuspended ::
    (Tokens -> Tokens)
    -> Tokens -> Tokens -> Tokens -> Parsec.Pos -> Parsec.More
    -> Failure r
    -> Success Tokens r
    -> Result r
tokensSuspended f s000 s0 t0 pos0 more0 lose0 succ0
    = Parsec.runParser (Parsec.demandInput_ >>= go) t0 pos0 more0 lose0 succ0 where
        go s' = Parsec.Parser $ \t pos more lose succ ->
            let s = f s'
            in case Text32.commonPrefixes s0 s of
                Nothing              -> lose t pos more [] "string"
                Just (_pfx,ssfx,tsfx)
                  | Text32.null ssfx -> let l = toPos (Text32.length s000)
                                        in succ t (pos + l) more
                                           (substring pos l t)
                  | Text32.null tsfx -> tokensSuspended f s000 ssfx t pos more
                                                        lose succ
                  | otherwise        -> lose t pos more [] "string"
{-# INLINE tokensSuspended #-}

skipWhile :: (Token -> Bool) -> Parser ()
skipWhile = \p -> let
    go = do
        t <- Text32.takeWhile p <$> get
        continue <- inputSpansChunks (size t)
        when continue go
    in go
{-# INLINE skipWhile #-}

takeTill :: (Token -> Bool) -> Parser Tokens
takeTill = \p -> takeWhile (not . p)
{-# INLINE takeTill #-}

takeWhile :: (Token -> Bool) -> Parser Tokens
takeWhile = \p -> do
    h        <- Text32.takeWhile p <$> get
    continue <- inputSpansChunks (size h)
    if continue then takeWhileAcc p [h]
                else return h
{-# INLINE takeWhile #-}

takeWhileAcc :: (Token -> Bool) -> [Tokens] -> Parser Tokens
takeWhileAcc = \p -> let
    go acc = do
        h        <- Text32.takeWhile p <$> get
        continue <- inputSpansChunks (size h)
        if continue then go (h:acc)
                    else return $ Parsec.concatReverse (h:acc)
    in go
{-# INLINE takeWhileAcc #-}

takeRest :: Parser [Tokens]
takeRest = go [] where
    go acc = Parsec.wantInput >>= \case
        False -> return (reverse acc)
        True  -> do
            s <- get
            advance (size s)
            go (s:acc)
{-# INLINE takeRest #-}

takeText :: Parser Tokens
takeText = Text32.concat <$> takeRest
{-# INLINE takeText #-}

takeWhile1 :: (Token -> Bool) -> Parser Tokens
takeWhile1 = \p -> do
    (`when` Parsec.demandInput) =<< endOfChunk
    h <- Text32.takeWhile p <$> get
    let size' = size h
    when (size' == 0) $ fail "takeWhile1"
    advance size'
    endOfChunk >>= \case
        True  -> takeWhileAcc p [h]
        False -> return h
{-# INLINE takeWhile1 #-}

anyToken :: Parser Token
anyToken = satisfy $ const True
{-# INLINE anyToken #-}

token :: Token -> Parser Token
token = satisfy . (==)
{-# INLINE token #-}

notToken :: Token -> Parser Token
notToken = satisfy . (/=)
{-# INLINE notToken #-}

failK    :: Failure a
successK :: Success a a
failK    = \t p _more -> Parsec.Fail (Text32.unsafeDrop (fromPos p) t)
successK = \t p _more -> Parsec.Done (Text32.unsafeDrop (fromPos p) t)
{-# INLINE failK    #-}
{-# INLINE successK #-}

parse     :: Parser a -> Tokens -> Result a
parseOnly :: Parser a -> Tokens -> Either String a
parse     m s =      Parsec.runParser m s 0 Parsec.Incomplete failK successK
parseOnly m s = case Parsec.runParser m s 0 Parsec.Complete   failK successK of
    Parsec.Fail _ [] err   -> Left err
    Parsec.Fail _ ctxs err -> Left (intercalate " > " ctxs ++ ": " ++ err)
    Parsec.Done _ a        -> Right a
    _                      -> error "parseOnly: impossible error!"
{-# INLINE parse #-}
{-# INLINE parseOnly #-}

get :: Parser Tokens
get = Parsec.Parser $ \t pos more _lose succ
    -> succ t pos more (Text32.unsafeDrop (fromPos pos) t)
{-# INLINE get #-}

endOfChunk :: Parser Bool
endOfChunk = Parsec.Parser $ \t pos more _lose succ
    -> succ t pos more (pos == lengthOf t)
{-# INLINE endOfChunk #-}

inputSpansChunks :: Parsec.Pos -> Parser Bool
inputSpansChunks i = Parsec.Parser $ \t pos_ more _lose succ ->
  let pos = pos_ + i
  in if pos < lengthOf t || more == Parsec.Complete
     then succ t pos more False
     else let lose' t' pos' more' = succ t' pos' more' False
              succ' t' pos' more' = succ t' pos' more' True
          in Parsec.prompt t pos more lose' succ'
{-# INLINE inputSpansChunks #-}

advance :: Parsec.Pos -> Parser ()
advance n = Parsec.Parser $ \t pos more _ succ -> succ t (pos+n) more ()
{-# INLINE advance #-}


-- === Ensure === -

ensure :: Int -> Parser (Parsec.Pos, Tokens)
ensure n = Parsec.Parser $ \t pos more lose succ ->
    case lengthAtLeast pos n t of
        Just n' -> succ t pos more (n', substring pos n' t)
        Nothing -> ensureSuspended n t pos more lose succ
{-# INLINE ensure #-}

ensureSuspended :: Int -> Tokens -> Parsec.Pos -> Parsec.More
                -> Failure r -> Success (Parsec.Pos, Tokens) r -> Result r
ensureSuspended n t pos more lose succ
    = Parsec.runParser (Parsec.demandInput >> go) t pos more lose succ
  where go = Parsec.Parser $ \t' pos' more' lose' succ' ->
            case lengthAtLeast pos' n t' of
                Just n' -> succ' t' pos' more' (n', substring pos n' t')
                Nothing -> Parsec.runParser (Parsec.demandInput >> go)
                                            t' pos' more' lose' succ'
{-# INLINE ensureSuspended #-}

lengthAtLeast :: Parsec.Pos -> Int -> Tokens -> Maybe Parsec.Pos
lengthAtLeast = \pos n t -> let
    !p' = fromPos pos + n
    in if p' <= Text32.length t
        then Just (toPos p')
        else Nothing where
{-# INLINE lengthAtLeast #-}

substring :: Parsec.Pos -> Parsec.Pos -> Tokens -> Tokens
substring = \p n b -> Text32.unsafeSlice (fromPos p) (fromPos n) b
{-# INLINE substring #-}

lengthOf :: Tokens -> Parsec.Pos
lengthOf = \t -> toPos $ Text32.length t
{-# INLINE lengthOf #-}

size :: Tokens -> Parsec.Pos
size = \t -> toPos $ Text32.length t
{-# INLINE size #-}
