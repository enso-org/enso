{-# LANGUAGE Strict #-}

module Data.Attoparsec.Text32 where

import Control.Applicative ((<$>))
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Attoparsec.Combinator ((<?>))
import Data.Attoparsec.Internal
import Data.Attoparsec.Internal.Types hiding (Parser, Failure, Success)
-- import qualified Data.Attoparsec.Text.Buffer as Buf
-- import Data.Attoparsec.Text.Buffer (Buffer, buffer)
import Data.Char (chr, ord)
import Data.List (intercalate)
import Data.String (IsString(..))
import Data.Text.Internal (Text(..))
import Prelude hiding (getChar, succ, take, takeWhile)
import qualified Data.Attoparsec.Internal.Types as T
-- import qualified Data.Attoparsec.Text.FastSet as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Unsafe as T

import Data.VectorText (VectorText)
import qualified Data.VectorText as VectorText
import Control.Lens hiding (lengthOf)

type Parser = T.Parser VectorText
type Result = IResult VectorText

data Buffer = Buffer
    { _txt :: {-# UNPACK #-} !VectorText
    , _off :: {-# UNPACK #-} !Int
    }
makeLenses ''Buffer

buffer :: VectorText -> Buffer
buffer t = Buffer t 0 ; {-# INLINE buffer #-}

type instance State VectorText = Buffer

type Failure r = T.Failure VectorText Buffer r
type Success a r = T.Success VectorText Buffer a r


instance Chunk VectorText where
  type ChunkElem VectorText = Char
  nullChunk                  = VectorText.null                        ; {-# INLINE nullChunk       #-}
  pappendChunk               = bufAppend                              ; {-# INLINE pappendChunk    #-}
  atBufferEnd  _             = Pos . VectorText.length . view txt     ; {-# INLINE atBufferEnd     #-}
  bufferElemAt _ (Pos i) b   = (,1) <$> VectorText.index (b ^. txt) i ; {-# INLINE bufferElemAt    #-}
  chunkElemToChar _          = id                                     ; {-# INLINE chunkElemToChar #-}

bufAppend :: Buffer -> VectorText -> Buffer
bufAppend b t = b & txt %~ (<> t) ; {-# INLINE bufAppend #-}

-- instance (a ~ Text) => IsString (Parser a) where
--     fromString = string . T.pack

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- ensure 1
  let !h = VectorText.unsafeHead c
  if p h then advance 1 >> return h
         else fail "satisfy"
{-# INLINE satisfy #-}

skip :: (Char -> Bool) -> Parser ()
skip p = do
  c <- ensure 1
  let !h = VectorText.unsafeHead c
  if p h then advance 1
         else fail "skip"

satisfyWith :: (Char -> a) -> (a -> Bool) -> Parser a
satisfyWith f p = do
  s <- ensure 1
  let c = f $! VectorText.unsafeHead s
  if p c then advance 1 >> return c
         else fail "satisfyWith"
{-# INLINE satisfyWith #-}

-- takeWith :: Int -> (Text -> Bool) -> Parser Text
-- takeWith n p = do
--   (k,s) <- ensure n
--   if p s
--     then advance k >> return s
--     else fail "takeWith"
--
-- -- | Consume exactly @n@ characters of input.
-- take :: Int -> Parser Text
-- take n = takeWith (max n 0) (const True)
-- {-# INLINE take #-}
--
-- -- | @string s@ parses a sequence of characters that identically match
-- -- @s@. Returns the parsed string (i.e. @s@).  This parser consumes no
-- -- input if it fails (even if a partial match).
-- --
-- -- /Note/: The behaviour of this parser is different to that of the
-- -- similarly-named parser in Parsec, as this one is all-or-nothing.
-- -- To illustrate the difference, the following parser will fail under
-- -- Parsec given an input of @\"for\"@:
-- --
-- -- >string "foo" <|> string "for"
-- --
-- -- The reason for its failure is that the first branch is a
-- -- partial match, and will consume the letters @\'f\'@ and @\'o\'@
-- -- before failing.  In attoparsec, the above parser will /succeed/ on
-- -- that input, because the failed first branch will consume nothing.
string :: VectorText -> Parser VectorText
string s = string_ (stringSuspended id) id s
{-# INLINE string #-}
--
string_ :: (forall r. VectorText -> VectorText -> Buffer -> Pos -> More
            -> Failure r -> Success VectorText r -> Result r)
        -> (VectorText -> VectorText)
        -> VectorText -> Parser VectorText
string_ suspended f s0 = T.Parser $ \t pos more lose succ ->
  let s  = f s0
    --   ft = f (Buf.unbufferAt (fromPos pos) t) - ???
      ft = f (VectorText.drop (fromPos pos) $ t ^. txt)
  in case VectorText.commonPrefixes s ft of
       Nothing
         | VectorText.null s  -> succ t pos more mempty
         | VectorText.null ft -> suspended s s t pos more lose succ
         | otherwise          -> lose t pos more [] "string"
       Just (pfx,ssfx,tsfx)
         | VectorText.null ssfx       -> let l = Pos (VectorText.length pfx)
                                in succ t (pos + l) more (substring pos l t)
         | not (VectorText.null tsfx) -> lose t pos more [] "string"
         | otherwise         -> suspended s ssfx t pos more lose succ
{-# INLINE string_ #-}
--
stringSuspended :: (VectorText -> VectorText)
                -> VectorText -> VectorText -> Buffer -> Pos -> More
                -> Failure r
                -> Success VectorText r
                -> Result r
stringSuspended f s000 s0 t0 pos0 more0 lose0 succ0 =
    runParser (demandInput_ >>= go) t0 pos0 more0 lose0 succ0
  where
    go s' = T.Parser $ \t pos more lose succ ->
      let s = f s'
      in case VectorText.commonPrefixes s0 s of
        Nothing         -> lose t pos more [] "string"
        Just (_pfx,ssfx,tsfx)
          | VectorText.null ssfx -> let l = Pos (VectorText.length s000)
                           in succ t (pos + l) more (substring pos l t)
          | VectorText.null tsfx -> stringSuspended f s000 ssfx t pos more lose succ
          | otherwise   -> lose t pos more [] "string"
--
-- -- | Satisfy a literal string, ignoring case.
-- --
-- -- Note: this function is currently quite inefficient. Unicode case
-- -- folding can change the length of a string (\"&#223;\" becomes
-- -- "ss"), which makes a simple, efficient implementation tricky.  We
-- -- have (for now) chosen simplicity over efficiency.
-- stringCI :: Text -> Parser Text
-- stringCI s = go 0
--   where
--     go !n
--       | n > T.length fs = fail "stringCI"
--       | otherwise = do
--       (k,t) <- ensure n
--       if T.toCaseFold t == fs
--         then advance k >> return t
--         else go (n+1)
--     fs = T.toCaseFold s
-- {-# INLINE stringCI #-}
-- {-# DEPRECATED stringCI "this is very inefficient, use asciiCI instead" #-}
--
-- -- | Satisfy a literal string, ignoring case for characters in the ASCII range.
-- asciiCI :: Text -> Parser Text
-- asciiCI s = string_ (stringSuspended asciiToLower) asciiToLower s
--   where
--     asciiToLower = T.map f
--       where
--         offset = ord 'a' - ord 'A'
--         f c | 'A' <= c && c <= 'Z' = chr (ord c + offset)
--             | otherwise            = c
-- {-# INLINE asciiCI #-}
--
-- -- | Skip past input for as long as the predicate returns 'True'.
-- skipWhile :: (Char -> Bool) -> Parser ()
-- skipWhile p = go
--  where
--   go = do
--     t <- T.takeWhile p <$> get
--     continue <- inputSpansChunks (size t)
--     when continue go
-- {-# INLINE skipWhile #-}
--
-- -- | Consume input as long as the predicate returns 'False'
-- -- (i.e. until it returns 'True'), and return the consumed input.
-- --
-- -- This parser does not fail.  It will return an empty string if the
-- -- predicate returns 'True' on the first character of input.
-- --
-- -- /Note/: Because this parser does not fail, do not use it with
-- -- combinators such as 'Control.Applicative.many', because such
-- -- parsers loop until a failure occurs.  Careless use will thus result
-- -- in an infinite loop.
-- takeTill :: (Char -> Bool) -> Parser Text
-- takeTill p = takeWhile (not . p)
-- {-# INLINE takeTill #-}
--
-- -- | Consume input as long as the predicate returns 'True', and return
-- -- the consumed input.
-- --
-- -- This parser does not fail.  It will return an empty string if the
-- -- predicate returns 'False' on the first character of input.
-- --
-- -- /Note/: Because this parser does not fail, do not use it with
-- -- combinators such as 'Control.Applicative.many', because such
-- -- parsers loop until a failure occurs.  Careless use will thus result
-- -- in an infinite loop.
takeWhile :: (Char -> Bool) -> Parser VectorText
takeWhile p = do
    h <- VectorText.takeWhile p <$> get
    continue <- inputSpansChunks (size h)
    -- only use slow concat path if necessary
    if continue
      then takeWhileAcc p [h]
      else return h
{-# INLINE takeWhile #-}

takeWhileAcc :: (Char -> Bool) -> [VectorText] -> Parser VectorText
takeWhileAcc p = go where
    go acc = do
        h <- VectorText.takeWhile p <$> get
        continue <- inputSpansChunks (size h)
        if continue then go (h:acc)
                    else return $ concatReverse (h:acc)
{-# INLINE takeWhileAcc #-}
--
-- takeRest :: Parser [Text]
-- takeRest = go []
--  where
--   go acc = do
--     input <- wantInput
--     if input
--       then do
--         s <- get
--         advance (size s)
--         go (s:acc)
--       else return (reverse acc)
--
-- -- | Consume all remaining input and return it as a single string.
-- takeText :: Parser Text
-- takeText = T.concat `fmap` takeRest
--
-- -- | Consume all remaining input and return it as a single string.
-- takeLazyText :: Parser L.Text
-- takeLazyText = L.fromChunks `fmap` takeRest
--
-- data Scan s = Continue s
--             | Finished s {-# UNPACK #-} !Int Text
--
-- scan_ :: (s -> [Text] -> Parser r) -> s -> (s -> Char -> Maybe s) -> Parser r
-- scan_ f s0 p = go [] s0
--  where
--   scanner s !n t =
--     case T.uncons t of
--       Just (c,t') -> case p s c of
--                        Just s' -> scanner s' (n+1) t'
--                        Nothing -> Finished s n t
--       Nothing     -> Continue s
--   go acc s = do
--     input <- get
--     case scanner s 0 input of
--       Continue s'  -> do continue <- inputSpansChunks (size input)
--                          if continue
--                            then go (input : acc) s'
--                            else f s' (input : acc)
--       Finished s' n t -> do advance (size input - size t)
--                             f s' (T.take n input : acc)
-- {-# INLINE scan_ #-}
--
-- -- | A stateful scanner.  The predicate consumes and transforms a
-- -- state argument, and each transformed state is passed to successive
-- -- invocations of the predicate on each character of the input until one
-- -- returns 'Nothing' or the input ends.
-- --
-- -- This parser does not fail.  It will return an empty string if the
-- -- predicate returns 'Nothing' on the first character of input.
-- --
-- -- /Note/: Because this parser does not fail, do not use it with
-- -- combinators such as 'Control.Applicative.many', because such
-- -- parsers loop until a failure occurs.  Careless use will thus result
-- -- in an infinite loop.
-- scan :: s -> (s -> Char -> Maybe s) -> Parser Text
-- scan = scan_ $ \_ chunks -> return $! concatReverse chunks
-- {-# INLINE scan #-}
--
-- -- | Like 'scan', but generalized to return the final state of the
-- -- scanner.
-- runScanner :: s -> (s -> Char -> Maybe s) -> Parser (Text, s)
-- runScanner = scan_ $ \s xs -> let !sx = concatReverse xs in return (sx, s)
-- {-# INLINE runScanner #-}
--
-- -- | Consume input as long as the predicate returns 'True', and return
-- -- the consumed input.
-- --
-- -- This parser requires the predicate to succeed on at least one
-- -- character of input: it will fail if the predicate never returns
-- -- 'True' or if there is no input left.
takeWhile1 :: (Char -> Bool) -> Parser VectorText
takeWhile1 p = do
  (`when` demandInput) =<< endOfChunk
  h <- VectorText.takeWhile p <$> get
  let size' = size h
  when (size' == 0) $ fail "takeWhile1"
  advance size'
  eoc <- endOfChunk
  if eoc
    then takeWhileAcc p [h]
    else return h
{-# INLINE takeWhile1 #-}
--
-- -- | Match any character in a set.
-- --
-- -- >vowel = inClass "aeiou"
-- --
-- -- Range notation is supported.
-- --
-- -- >halfAlphabet = inClass "a-nA-N"
-- --
-- -- To add a literal @\'-\'@ to a set, place it at the beginning or end
-- -- of the string.
-- inClass :: String -> Char -> Bool
-- inClass s = (`Set.member` mySet)
--     where mySet = Set.charClass s
--           {-# NOINLINE mySet #-}
-- {-# INLINE inClass #-}
--
-- -- | Match any character not in a set.
-- notInClass :: String -> Char -> Bool
-- notInClass s = not . inClass s
-- {-# INLINE notInClass #-}
--
-- -- | Match any character.
anyChar :: Parser Char
anyChar = satisfy $ const True ; {-# INLINE anyChar #-}

-- -- | Match a specific character.
char :: Char -> Parser Char
char c = satisfy (== c) ; {-# INLINE char #-}

-- -- | Match any character except the given one.
-- notChar :: Char -> Parser Char
-- notChar c = satisfy (/= c) <?> "not " ++ show c
-- {-# INLINE notChar #-}
--
-- -- | Match any character, to perform lookahead. Returns 'Nothing' if
-- -- end of input has been reached. Does not consume any input.
-- --
-- -- /Note/: Because this parser does not fail, do not use it with
-- -- combinators such as 'Control.Applicative.many', because such
-- -- parsers loop until a failure occurs.  Careless use will thus result
-- -- in an infinite loop.
peekChar :: Parser (Maybe Char)
peekChar = T.Parser $ \t pos more _lose succ ->
  case () of
    _| pos < lengthOf t ->
       let !c = VectorText.unsafeIndex (t ^. txt) (fromPos pos) -- Buf.iter t (fromPos pos) ???
       in succ t pos more (Just c)
     | more == Complete ->
       succ t pos more Nothing
     | otherwise ->
       let succ' t' pos' more' =
             let !c = VectorText.unsafeIndex (t' ^. txt) (fromPos pos')
             in succ t' pos' more' (Just c)
           lose' t' pos' more' = succ t' pos' more' Nothing
       in prompt t pos more lose' succ'
{-# INLINE peekChar #-}
--
-- -- | Match any character, to perform lookahead.  Does not consume any
-- -- input, but will fail if end of input has been reached.
peekChar' :: Parser Char
peekChar' = do
  s <- ensure 1
  return $! VectorText.unsafeHead s
{-# INLINE peekChar' #-}
--
-- -- | Match either a single newline character @\'\\n\'@, or a carriage
-- -- return followed by a newline character @\"\\r\\n\"@.
-- endOfLine :: Parser ()
-- endOfLine = (char '\n' >> return ()) <|> (string "\r\n" >> return ())
--
-- | Terminal failure continuation.
failK :: Failure a
failK t (Pos pos) _more stack msg = Fail (VectorText.drop pos $ t ^. txt) stack msg
{-# INLINE failK #-}
--
-- -- | Terminal success continuation.
successK :: Success a a
successK t (Pos pos) _more a = Done (VectorText.drop pos $ t ^. txt) a
{-# INLINE successK #-}

-- -- | Run a parser.
parse :: Parser a -> VectorText -> Result a
parse m s = runParser m (buffer s) 0 Incomplete failK successK ; {-# INLINE parse #-}
--
-- -- | Run a parser that cannot be resupplied via a 'Partial' result.
-- --
-- -- This function does not force a parser to consume all of its input.
-- -- Instead, any residual input will be discarded.  To force a parser
-- -- to consume all of its input, use something like this:
-- --
-- -- @
-- --'parseOnly' (myParser 'Control.Applicative.<*' 'endOfInput')
-- -- @
parseOnly :: Parser a -> VectorText -> Either String a
parseOnly m s = case runParser m (buffer s) 0 Complete failK successK of
                  Fail _ [] err   -> Left err
                  Fail _ ctxs err -> Left (intercalate " > " ctxs ++ ": " ++ err)
                  Done _ a        -> Right a
                  _               -> error "parseOnly: impossible error!"
{-# INLINE parseOnly #-}
--
get :: Parser VectorText
get = T.Parser $ \t pos more _lose succ -> succ t pos more (VectorText.drop (fromPos pos) $ t ^. txt) ; {-# INLINE get #-}
--
endOfChunk :: Parser Bool
endOfChunk = T.Parser $ \t pos more _lose succ -> succ t pos more (pos == lengthOf t) ; {-# INLINE endOfChunk #-}
--
inputSpansChunks :: Pos -> Parser Bool
inputSpansChunks i = T.Parser $ \t pos_ more _lose succ ->
  let pos = pos_ + i
  in if pos < lengthOf t || more == Complete
     then succ t pos more False
     else let lose' t' pos' more' = succ t' pos' more' False
              succ' t' pos' more' = succ t' pos' more' True
          in prompt t pos more lose' succ'
{-# INLINE inputSpansChunks #-}
--
advance :: Pos -> Parser ()
advance n = T.Parser $ \t pos more _lose succ -> succ t (pos+n) more () ; {-# INLINE advance #-}

ensureSuspended :: Int -> Buffer -> Pos -> More
                -> Failure r -> Success VectorText r
                -> Result r
ensureSuspended n t pos more lose succ =
    runParser (demandInput >> go) t pos more lose succ
  where go = T.Parser $ \t' pos' more' lose' succ' ->
          case lengthAtLeast pos' n t' of
            Just n' -> succ' t' pos' more' (substring pos n' t')
            Nothing -> runParser (demandInput >> go) t' pos' more' lose' succ'
{-# INLINE ensureSuspended #-}

-- -- | If at least @n@ elements of input are available, return the
-- -- current input, otherwise fail.
ensure :: Int -> Parser VectorText
ensure n = T.Parser $ \t pos more lose succ -> case lengthAtLeast pos n t of
      Just n' -> succ t pos more (substring pos n' t)
      Nothing -> ensureSuspended n t pos more lose succ
{-# INLINE ensure #-}
--
-- -- | Return both the result of a parse and the portion of the input
-- -- that was consumed while it was being parsed.
-- match :: Parser a -> Parser (Text, a)
-- match p = T.Parser $ \t pos more lose succ ->
--   let succ' t' pos' more' a = succ t' pos' more'
--                               (substring pos (pos'-pos) t', a)
--   in runParser p t pos more lose succ'
--
-- -- | Ensure that at least @n@ code points of input are available.
-- -- Returns the number of words consumed while traversing.
lengthAtLeast :: Pos -> Int -> Buffer -> Maybe Pos
lengthAtLeast pos n t = if p' < VectorText.length (t ^. txt) then Just (Pos p') else Nothing where
    p' = fromPos pos + n
{-# INLINE lengthAtLeast #-}

--   where go i !p
--           | i == n    = Just (Pos p - pos)
--           | p == len  = Nothing
--           | otherwise = go (i+1) (p + Buf.iter_ t p)
--         Pos len = lengthOf t
-- {-# INLINE lengthAtLeast #-}
--
substring :: Pos -> Pos -> Buffer -> VectorText
substring (Pos pos) (Pos n) b = VectorText.take n $ VectorText.drop pos (b ^. txt) ; {-# INLINE substring #-}
--
lengthOf :: Buffer -> Pos
lengthOf = Pos . VectorText.length . view txt
--
size :: VectorText -> Pos
size = Pos . VectorText.length ; {-# INLINE size #-}
-- size (Text _ _ l) = Pos l
