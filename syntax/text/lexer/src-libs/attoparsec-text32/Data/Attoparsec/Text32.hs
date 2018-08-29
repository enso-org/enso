{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Attoparsec.Text32 where

import Prelude                        hiding (getChar, succ, take, takeWhile)
import Control.Applicative            ((<$>))
import Control.Monad                  (when)
import Data.Attoparsec.Internal
import Data.Attoparsec.Internal.Types hiding (Parser, Failure, Success, fromPos)
import Data.List                      (intercalate)

import           Data.Text32 (Text32)
import qualified Data.Text32 as Text32
import           Data.Coerce

import qualified Data.Attoparsec.Internal.Types as T





-- === Definitions === --

type Parser      = T.Parser Text32
type Result      = IResult Text32
type Failure   r = T.Failure Text32 Text32   r
type Success a r = T.Success Text32 Text32 a r

type instance State Text32 = Text32

instance Chunk Text32 where
    type ChunkElem Text32 = Char
    nullChunk             = Text32.null                         ; {-# INLINE nullChunk       #-}
    pappendChunk          = (<>)                                ; {-# INLINE pappendChunk    #-}
    atBufferEnd     _     = toPos . Text32.length               ; {-# INLINE atBufferEnd     #-}
    bufferElemAt    _ i b = (,1) <$> Text32.index b (fromPos i) ; {-# INLINE bufferElemAt    #-}
    chunkElemToChar _     = id                                  ; {-# INLINE chunkElemToChar #-}


-- === Pos coertions === --

fromPos :: Pos -> Int
toPos   :: Int -> Pos
fromPos = coerce ; {-# INLINE fromPos #-}
toPos   = coerce ; {-# INLINE toPos   #-}


-- === Primitive parsers === --

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- ensure 1
  let !h = Text32.unsafeHead c
  if p h then advance 1 >> return h
         else fail "satisfy"
{-# INLINE satisfy #-}

skip :: (Char -> Bool) -> Parser ()
skip p = do
  c <- ensure 1
  let !h = Text32.unsafeHead c
  if p h then advance 1
         else fail "skip"
{-# INLINE skip #-}

satisfyWith :: (Char -> a) -> (a -> Bool) -> Parser a
satisfyWith f p = do
  s <- ensure 1
  let c = f $! Text32.unsafeHead s
  if p c then advance 1 >> return c
         else fail "satisfyWith"
{-# INLINE satisfyWith #-}

takeWith :: Int -> (Text32 -> Bool) -> Parser Text32
takeWith n p = do
  s <- ensure n
  if p s then advance 1 >> return s
         else fail "takeWith"
{-# INLINE takeWith #-}

-- | Consume exactly @n@ characters of input.
take :: Int -> Parser Text32
take n = takeWith (max n 0) (const True) ; {-# INLINE take #-}

-- -- | @string s@ parses a sequence of characters that identically match
-- -- @s@. Returns the parsed string (i.e. @s@).  This parser consumes no
-- -- input if it fails (even if a partial match).
string :: Text32 -> Parser Text32
string s = string_ (stringSuspended id) id s ; {-# INLINE string #-}

string_ :: (forall r. Text32 -> Text32 -> Text32 -> Pos -> More
            -> Failure r -> Success Text32 r -> Result r)
        -> (Text32 -> Text32) -> Text32 -> Parser Text32
string_ suspended f s0 = T.Parser $ \t pos more lose succ ->
  let s  = f s0
      ft = f (Text32.unsafeDrop (fromPos pos) t)
  in case Text32.commonPrefixes s ft of
       Nothing
         | Text32.null s  -> succ t pos more mempty
         | Text32.null ft -> suspended s s t pos more lose succ
         | otherwise      -> lose t pos more [] "string"
       Just (pfx,ssfx,tsfx)
         | Text32.null ssfx       -> let l = toPos (Text32.length pfx)
                                     in succ t (pos + l) more (substring pos l t)
         | not (Text32.null tsfx) -> lose t pos more [] "string"
         | otherwise              -> suspended s ssfx t pos more lose succ
{-# INLINE string_ #-}

stringSuspended :: (Text32 -> Text32)
                -> Text32 -> Text32 -> Text32 -> Pos -> More
                -> Failure r
                -> Success Text32 r
                -> Result r
stringSuspended f s000 s0 t0 pos0 more0 lose0 succ0 = runParser (demandInput_ >>= go) t0 pos0 more0 lose0 succ0 where
    go s' = T.Parser $ \t pos more lose succ -> let s = f s' in case Text32.commonPrefixes s0 s of
        Nothing              -> lose t pos more [] "string"
        Just (_pfx,ssfx,tsfx)
          | Text32.null ssfx -> let l = toPos (Text32.length s000)
                                in succ t (pos + l) more (substring pos l t)
          | Text32.null tsfx -> stringSuspended f s000 ssfx t pos more lose succ
          | otherwise        -> lose t pos more [] "string"
{-# INLINE stringSuspended #-}

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = go where
    go = do
        t <- Text32.takeWhile p <$> get
        continue <- inputSpansChunks (size t)
        when continue go
{-# INLINE skipWhile #-}

takeTill :: (Char -> Bool) -> Parser Text32
takeTill p = takeWhile (not . p) ; {-# INLINE takeTill #-}

takeWhile :: (Char -> Bool) -> Parser Text32
takeWhile p = do
    h        <- Text32.takeWhile p <$> get
    continue <- inputSpansChunks (size h)
    if continue then takeWhileAcc p [h]
                else return h
{-# INLINE takeWhile #-}

takeWhileAcc :: (Char -> Bool) -> [Text32] -> Parser Text32
takeWhileAcc p = go where
    go acc = do
        h        <- Text32.takeWhile p <$> get
        continue <- inputSpansChunks (size h)
        if continue then go (h:acc)
                    else return $ concatReverse (h:acc)
{-# INLINE takeWhileAcc #-}

takeRest :: Parser [Text32]
takeRest = go [] where
    go acc = wantInput >>= \case
        False -> return (reverse acc)
        True  -> do
            s <- get
            advance (size s)
            go (s:acc)
{-# INLINE takeRest #-}

takeText :: Parser Text32
takeText = Text32.concat `fmap` takeRest ; {-# INLINE takeText #-}

takeWhile1 :: (Char -> Bool) -> Parser Text32
takeWhile1 p = do
    (`when` demandInput) =<< endOfChunk
    h <- Text32.takeWhile p <$> get
    let size' = size h
    when (size' == 0) $ fail "takeWhile1"
    advance size'
    endOfChunk >>= \case
        True  -> takeWhileAcc p [h]
        False -> return h
{-# INLINE takeWhile1 #-}

anyChar :: Parser Char
anyChar = satisfy $ const True ; {-# INLINE anyChar #-}

char :: Char -> Parser Char
char c = satisfy (== c) ; {-# INLINE char #-}

notChar :: Char -> Parser Char
notChar c = satisfy (/= c) ; {-# INLINE notChar #-}

peekChar :: Parser (Maybe Char)
peekChar = T.Parser $ \t pos more _lose succ -> if
     | pos < lengthOf t -> let !c = Text32.unsafeIndex t (fromPos pos) in succ t pos more (Just c)
     | more == Complete -> succ t pos more Nothing
     | otherwise        ->
       let succ' t' pos' more' =
             let !c = Text32.unsafeIndex t' (fromPos pos')
             in succ t' pos' more' (Just c)
           lose' t' pos' more' = succ t' pos' more' Nothing
       in prompt t pos more lose' succ'
{-# INLINE peekChar #-}

peekChar' :: Parser Char
peekChar' = do
  s <- ensure 1
  return $! Text32.unsafeHead s
{-# INLINE peekChar' #-}

failK    :: Failure a
successK :: Success a a
failK    t p _more stack msg = Fail (Text32.unsafeDrop (fromPos p) t) stack msg ; {-# INLINE failK    #-}
successK t p _more stack     = Done (Text32.unsafeDrop (fromPos p) t) stack     ; {-# INLINE successK #-}

parse :: Parser a -> Text32 -> Result a
parseOnly :: Parser a -> Text32 -> Either String a
parse     m s =      runParser m s 0 Incomplete failK successK ; {-# INLINE parse #-}
parseOnly m s = case runParser m s 0 Complete   failK successK of
    Fail _ [] err   -> Left err
    Fail _ ctxs err -> Left (intercalate " > " ctxs ++ ": " ++ err)
    Done _ a        -> Right a
    _               -> error "parseOnly: impossible error!"
{-# INLINE parseOnly #-}

get :: Parser Text32
get = T.Parser $ \t pos more _lose succ -> succ t pos more (Text32.unsafeDrop (fromPos pos) t) ; {-# INLINE get #-}

endOfChunk :: Parser Bool
endOfChunk = T.Parser $ \t pos more _lose succ -> succ t pos more (pos == lengthOf t) ; {-# INLINE endOfChunk #-}

inputSpansChunks :: Pos -> Parser Bool
inputSpansChunks i = T.Parser $ \t pos_ more _lose succ ->
  let pos = pos_ + i
  in if pos < lengthOf t || more == Complete
     then succ t pos more False
     else let lose' t' pos' more' = succ t' pos' more' False
              succ' t' pos' more' = succ t' pos' more' True
          in prompt t pos more lose' succ'
{-# INLINE inputSpansChunks #-}


advance :: Pos -> Parser ()
advance n = T.Parser $ \t pos more _ succ -> succ t (pos+n) more () ; {-# INLINE advance #-}


-- === Ensure === -

ensure :: Int -> Parser Text32
ensure n = T.Parser $ \t pos more lose succ -> case lengthAtLeast pos n t of
      Just n' -> succ t pos more (substring pos n' t)
      Nothing -> ensureSuspended n t pos more lose succ
{-# INLINE ensure #-}

ensureSuspended :: Int -> Text32 -> Pos -> More
                -> Failure r -> Success Text32 r -> Result r
ensureSuspended n t pos more lose succ = runParser (demandInput >> go) t pos more lose succ
  where go = T.Parser $ \t' pos' more' lose' succ' -> case lengthAtLeast pos' n t' of
            Just n' -> succ' t' pos' more' (substring pos n' t')
            Nothing -> runParser (demandInput >> go) t' pos' more' lose' succ'
{-# INLINE ensureSuspended #-}

lengthAtLeast :: Pos -> Int -> Text32 -> Maybe Pos
lengthAtLeast pos n t = if p' <= Text32.length t then Just (toPos p') else Nothing where
    !p' = fromPos pos + n
{-# INLINE lengthAtLeast #-}

substring :: Pos -> Pos -> Text32 -> Text32
substring p n b = Text32.unsafeSlice (fromPos p) (fromPos n) b ; {-# INLINE substring #-}

lengthOf :: Text32 -> Pos
lengthOf t = toPos $ Text32.length t ; {-# INLINE lengthOf #-}

size :: Text32 -> Pos
size t = toPos $ Text32.length t ; {-# INLINE size #-}
