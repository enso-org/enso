{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Attoparsec.List where

import           Prelude hiding (getChar, length, null, succ, take, takeWhile)
import qualified Prelude as P

import qualified Data.Attoparsec.Internal       as Parsec
import qualified Data.Attoparsec.Internal.Types as Parsec
import qualified Data.Parser                    as Parser
import qualified Data.Vector                    as Vector

import Control.Applicative ((<$>))
import Control.Monad       (when)
import Control.Monad       (void)
import Data.Coerce         (coerce)
import Data.Item           (Item)
import Data.List           (intercalate)
import Data.Vector         (Vector)

import Data.Convert

import Data.Parser.Instances.Attoparsec ()


class MayBeNull a where
    null :: a -> Bool

class KnownLength a where
    length :: a -> Int

class UnsafeHead a where
    unsafeHead :: a -> Item a

class UnsafeSlice a where
    unsafeSlice :: Int -> Int -> a -> a

instance MayBeNull [a] where
    null = P.null
    {-# INLINE null #-}

instance KnownLength [a] where
    length = P.length
    {-# INLINE length #-}

instance UnsafeHead [a] where
    unsafeHead = \case
        (a : _) -> a
        _       -> error "unsafeHead"
    {-# INLINE unsafeHead #-}

instance MayBeNull (Vector a) where
    null = Vector.null
    {-# INLINE null #-}

instance KnownLength (Vector a) where
    length = Vector.length
    {-# INLINE length #-}

instance UnsafeHead (Vector a) where
    unsafeHead = Vector.unsafeHead
    {-# INLINE unsafeHead #-}

instance UnsafeSlice (Vector a) where
    unsafeSlice = Vector.unsafeSlice
    {-# INLINE unsafeSlice #-}


-- === Config === --

type (Tokens a) = Vector a
type (Token a)  = Item (Tokens a)


-- === Definitions === --

type Parser  a     = Parsec.Parser  (Tokens a)
type Result  a     = Parsec.IResult (Tokens a)
type Failure a   r = Parsec.Failure (Tokens a) (Tokens a)   r
type Success a t r = Parsec.Success (Tokens a) (Tokens a) t r

type instance Parsec.State (Tokens a) = (Tokens a)

instance Parsec.Chunk (Tokens a) where
    type ChunkElem (Tokens a) = (Token a)
    nullChunk             = null
    pappendChunk          = (<>)
    atBufferEnd     (~_)  = toPos . length
    -- bufferElemAt    _ i b = (,1) <$> Vector.index b (fromPos i)
    -- chunkElemToChar _     = id
    {-# INLINE nullChunk       #-}
    {-# INLINE pappendChunk    #-}
    {-# INLINE atBufferEnd     #-}
    -- {-# INLINE bufferElemAt    #-}
    -- {-# INLINE chunkElemToChar #-}


-- === Parsec.Pos coertions === --

fromPos :: Parsec.Pos -> Int
fromPos = coerce
{-# INLINE fromPos #-}

toPos :: Int -> Parsec.Pos
toPos = coerce
{-# INLINE toPos   #-}


-- === Lookahead === --

-- tryPeekToken :: Parser (Maybe (Token a))
-- tryPeekToken = Parsec.Parser $ \t pos more _lose succ -> if
--      | pos < lengthOf t -> let !c = Vector.unsafeIndex t (fromPos pos)
--                             in succ t pos more (Just c)
--      | more == Parsec.Complete -> succ t pos more Nothing
--      | otherwise        ->
--        let succ' t' pos' more' =
--              let !c = Vector.unsafeIndex t' (fromPos pos')
--              in succ t' pos' more' (Just c)
--            lose' t' pos' more' = succ t' pos' more' Nothing
--        in Parsec.prompt t pos more lose' succ'
-- {-# INLINE tryPeekToken #-}

peekToken :: Parser a (Token a)
peekToken = do
    (!_, !t) <- ensure 1
    pure $! unsafeHead t
{-# INLINE peekToken #-}


-- === Primitive parsers === --

satisfy :: ((Token a) -> Bool) -> Parser a (Token a)
satisfy = \p -> do
  h <- peekToken
  if p h then h <$ advance 1
         else fail "satisfy"
{-# INLINE satisfy #-}

skip :: ((Token a) -> Bool) -> Parser a ()
skip = \p -> do
  h <- peekToken
  if p h then advance 1
         else fail "skip"
{-# INLINE skip #-}

satisfyWith :: ((Token a) -> a) -> (a -> Bool) -> Parser a a
satisfyWith = \f p -> do
  h <- peekToken
  let c = f h
  if p c then c <$ advance 1
         else fail "satisfyWith"
{-# INLINE satisfyWith #-}

takeWith :: Int -> ((Tokens a) -> Bool) -> Parser a (Tokens a)
takeWith = \n p -> do
  (k, s) <- ensure n
  if p s then advance k >> return s
         else fail "takeWith"
{-# INLINE takeWith #-}

-- | Consume exactly @n@ tokens of input.
take :: Int -> Parser a (Tokens a)
take = \n -> takeWith (max n 0) (const True)
{-# INLINE take #-}

-- -- -- | @string s@ parses a sequence of tokens that identically match
-- -- -- @s@. Returns the parsed string (i.e. @s@).  This parser consumes no
-- -- -- input if it fails (even if a partial match).
-- tokens :: (Tokens a) -> Parser (Tokens a)
-- tokens = \s -> tokens_ (tokensSuspended id) id s
-- {-# INLINE tokens #-}

-- tokens_ ::
--     (forall r. (Tokens a) -> (Tokens a) -> (Tokens a) -> Parsec.Pos -> Parsec.More
--                       -> Failure r -> Success (Tokens a) r -> Result r)
--     -> ((Tokens a) -> (Tokens a))
--     -> (Tokens a) -> Parser (Tokens a)
-- tokens_ suspended f s0 = Parsec.Parser $ \t pos more lose succ ->
--   let s  = f s0
--       ft = f (Vector.unsafeDrop (fromPos pos) t)
--   in case Vector.commonPrefixes s ft of
--        Nothing
--          | Vector.null s  -> succ t pos more mempty
--          | Vector.null ft -> suspended s s t pos more lose succ
--          | otherwise      -> lose t pos more [] "string"
--        Just (pfx,ssfx,tsfx)
--          | Vector.null ssfx   -> let l = toPos (Vector.length pfx)
--                                  in succ t (pos + l) more (substring pos l t)
--          | not (Vector.null tsfx) -> lose t pos more [] "string"
--          | otherwise              -> suspended s ssfx t pos more lose succ
-- {-# INLINE tokens_ #-}

-- tokensSuspended ::
--     ((Tokens a) -> (Tokens a))
--     -> (Tokens a) -> (Tokens a) -> (Tokens a) -> Parsec.Pos -> Parsec.More
--     -> Failure r
--     -> Success (Tokens a) r
--     -> Result r
-- tokensSuspended f s000 s0 t0 pos0 more0 lose0 succ0
--     = Parsec.runParser (Parsec.demandInput_ >>= go) t0 pos0 more0 lose0 succ0 where
--         go s' = Parsec.Parser $ \t pos more lose succ ->
--             let s = f s'
--             in case Vector.commonPrefixes s0 s of
--                 Nothing              -> lose t pos more [] "string"
--                 Just (_pfx,ssfx,tsfx)
--                   | Vector.null ssfx -> let l = toPos (Vector.length s000)
--                                         in succ t (pos + l) more
--                                            (substring pos l t)
--                   | Vector.null tsfx -> tokensSuspended f s000 ssfx t pos more
--                                                         lose succ
--                   | otherwise        -> lose t pos more [] "string"
-- {-# INLINE tokensSuspended #-}

-- skipWhile :: ((Token a) -> Bool) -> Parser ()
-- skipWhile = \p -> let
--     go = do
--         t <- Vector.takeWhile p <$> get
--         continue <- inputSpansChunks (size t)
--         when continue go
--     in go
-- {-# INLINE skipWhile #-}

-- takeTill :: ((Token a) -> Bool) -> Parser (Tokens a)
-- takeTill = \p -> takeWhile (not . p)
-- {-# INLINE takeTill #-}

takeWhile :: (a -> Bool) -> Parser a (Tokens a)
takeWhile = \p -> do
    h        <- Vector.takeWhile p <$> get
    continue <- inputSpansChunks (size h)
    if continue then takeWhileAcc p [h]
                else return h
{-# INLINE takeWhile #-}

takeWhileAcc :: (a -> Bool) -> [Tokens a] -> Parser a (Tokens a)
takeWhileAcc = \p -> let
    go acc = do
        h        <- Vector.takeWhile p <$> get
        continue <- inputSpansChunks (size h)
        if continue then go (h:acc)
                    else return $ Parsec.concatReverse (h:acc)
    in go
{-# INLINE takeWhileAcc #-}

-- takeRest :: Parser [(Tokens a)]
-- takeRest = go [] where
--     go acc = Parsec.wantInput >>= \case
--         False -> return (reverse acc)
--         True  -> do
--             s <- get
--             advance (size s)
--             go (s:acc)
-- {-# INLINE takeRest #-}

-- takeText :: Parser (Tokens a)
-- takeText = Vector.concat <$> takeRest
-- {-# INLINE takeText #-}

takeWhile1 :: ((Token a) -> Bool) -> Parser a (Tokens a)
takeWhile1 = \p -> do
    (`when` Parsec.demandInput) =<< endOfChunk
    h <- Vector.takeWhile p <$> get
    let size' = size h
    when (size' == 0) $ fail "takeWhile1"
    advance size'
    endOfChunk >>= \case
        True  -> takeWhileAcc p [h]
        False -> return h
{-# INLINE takeWhile1 #-}

anyToken :: Parser a (Token a)
anyToken = satisfy $ const True
{-# INLINE anyToken #-}

token :: Eq a => (Token a) -> Parser a (Token a)
token = satisfy . (==)
{-# INLINE token #-}

notToken :: Eq a => (Token a) -> Parser a (Token a)
notToken = satisfy . (/=)
{-# INLINE notToken #-}

failK    :: Failure a t
successK :: Success a t t
failK    = \t p _more -> Parsec.Fail (Vector.unsafeDrop (fromPos p) t)
successK = \t p _more -> Parsec.Done (Vector.unsafeDrop (fromPos p) t)
{-# INLINE failK    #-}
{-# INLINE successK #-}

parse     :: Parser a t -> (Tokens a) -> Result a t
parseOnly :: Parser a t -> (Tokens a) -> Either String t
parse     m s =      Parsec.runParser m s 0 Parsec.Incomplete failK successK
parseOnly m s = case Parsec.runParser m s 0 Parsec.Complete   failK successK of
    Parsec.Fail _ [] err   -> Left err
    Parsec.Fail _ ctxs err -> Left (intercalate " > " ctxs ++ ": " ++ err)
    Parsec.Done _ a        -> Right a
    _                      -> error "parseOnly: impossible error!"
-- {-# INLINE parse #-}
-- {-# INLINE parseOnly #-}

get :: Parser a (Tokens a)
get = Parsec.Parser $ \t pos more _lose succ
    -> succ t pos more (Vector.unsafeDrop (fromPos pos) t)
{-# INLINE get #-}

endOfChunk :: Parser a Bool
endOfChunk = Parsec.Parser $ \t pos more _lose succ
    -> succ t pos more (pos == lengthOf t)
{-# INLINE endOfChunk #-}

inputSpansChunks :: Parsec.Pos -> Parser a Bool
inputSpansChunks i = Parsec.Parser $ \t pos_ more _lose succ ->
  let pos = pos_ + i
  in if pos < lengthOf t || more == Parsec.Complete
     then succ t pos more False
     else let lose' t' pos' more' = succ t' pos' more' False
              succ' t' pos' more' = succ t' pos' more' True
          in Parsec.prompt t pos more lose' succ'
{-# INLINE inputSpansChunks #-}

advance :: Parsec.Pos -> Parser a ()
advance = \n -> Parsec.Parser $ \t pos more _ succ -> succ t (pos + n) more ()
{-# INLINE advance #-}


-- === Ensure === -

ensure :: Int -> Parser a (Parsec.Pos, (Tokens a))
ensure n = Parsec.Parser $ \t pos more lose succ ->
    case lengthAtLeast pos n t of
        Just n' -> succ t pos more (n', substring pos n' t)
        Nothing -> ensureSuspended n t pos more lose succ
{-# INLINE ensure #-}

ensureSuspended :: Int -> (Tokens a) -> Parsec.Pos -> Parsec.More
                -> Failure a r -> Success a (Parsec.Pos, (Tokens a)) r -> Result a r
ensureSuspended n t pos more lose succ
    = Parsec.runParser (Parsec.demandInput >> go) t pos more lose succ
  where go = Parsec.Parser $ \t' pos' more' lose' succ' ->
            case lengthAtLeast pos' n t' of
                Just n' -> succ' t' pos' more' (n', substring pos n' t')
                Nothing -> Parsec.runParser (Parsec.demandInput >> go)
                                            t' pos' more' lose' succ'
{-# INLINE ensureSuspended #-}

lengthAtLeast :: Parsec.Pos -> Int -> (Tokens a) -> Maybe Parsec.Pos
lengthAtLeast = \pos n t -> let
    !p' = fromPos pos + n
    in if p' <= length t
        then Just $! toPos p'
        else Nothing where
{-# INLINE lengthAtLeast #-}

substring :: Parsec.Pos -> Parsec.Pos -> (Tokens a) -> (Tokens a)
substring = \p n b -> unsafeSlice (fromPos p) (fromPos n) b
{-# INLINE substring #-}

lengthOf :: (Tokens a) -> Parsec.Pos
lengthOf = \t -> toPos $ Vector.length t
{-# INLINE lengthOf #-}

size :: (Tokens a) -> Parsec.Pos
size = \t -> toPos $ Vector.length t
{-# INLINE size #-}



type instance Parser.Token (Parser a) = a

instance Eq a
      => Parser.TokenParser (Parser a) where
    satisfy    = satisfy       ; {-# INLINE satisfy    #-}
    takeWhile  = takeWhile     ; {-# INLINE takeWhile  #-}
    takeWhile1 = takeWhile1    ; {-# INLINE takeWhile1 #-}
    anyToken   = anyToken      ; {-# INLINE anyToken   #-}
    token_     = void . token  ; {-# INLINE token_     #-}
    -- tokens_    = void . SText.string ; {-# INLINE tokens_    #-}
    peekToken  = peekToken     ; {-# INLINE peekToken  #-}
    -- peekToken' = SText.peekChar      ; {-# INLINE peekToken' #-}

-- instance Parser.PartialParser (Parser a) where
--     -- parsePartialT = pure .: SText.parse      ; {-# INLINE parsePartialT #-}
--     feedPartialT  = pure .: Parsec.feed             ; {-# INLINE feedPartialT  #-}
--     closePartialT = flip Parser.feedPartialT mempty ; {-# INLINE closePartialT #-}
