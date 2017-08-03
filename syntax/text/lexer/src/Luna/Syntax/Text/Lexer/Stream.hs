module Luna.Syntax.Text.Lexer.Stream where

import Prologue hiding (empty, unless)


import           Control.Exception          (Exception)
import           Control.Monad              (unless)
import qualified Data.ByteString            as B
import qualified Data.Text                  as T
import qualified Data.Text.Internal         as TI
import           Data.Typeable              (Typeable)

import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Types      as A
import           Data.Attoparsec.Types (Parser, IResult)
import           Data.Conduit
import Control.Monad.Trans.Resource (MonadThrow, monadThrow)







-------------------
-- === Delta === --
-------------------

-- === Definition === --

newtype Delta = Delta Int deriving (Eq, NFData, Num, Ord)
makeLenses ''Delta


-- === Instances === --

instance Show   Delta where show   = show . unwrap ; {-# INLINE show   #-}
instance Mempty Delta where mempty = Delta 0       ; {-# INLINE mempty #-}

instance Convertible Int Delta where convert = coerce ; {-# INLINE convert #-}
instance Convertible Delta Int where convert = coerce ; {-# INLINE convert #-}


---------------------
-- === Error ? === --
---------------------

-- | The context and message from a 'A.Fail' value.
data ParseError = ParseError
    { errorContexts :: ![String]
    , errorMessage  :: !String
    , errorSpan     :: {-# UNPACK #-} !Delta
    } | DivergentParser
    deriving (Generic, Show, Typeable)
instance Exception ParseError
instance NFData ParseError



------------------
-- === Span === --
------------------

-- === Definition === --

data Span = Span
    { _len :: {-# UNPACK #-} !Delta
    , _off :: {-# UNPACK #-} !Delta
    } deriving (Eq, Generic, Ord)
makeLenses ''Span


-- === Instances === --

instance NFData Span
instance Show Span where
    show (Span len off) = "(" <> show len <> "," <> show off <> ")" ; {-# INLINE show #-}





-- | A class of types which may be consumed by an Attoparsec parser.
class AttoparsecInput a where
    runParser     :: Parser  a b -> a -> IResult a b
    feed          :: IResult a b -> a -> IResult a b
    empty         :: a
    isNull        :: a -> Bool
    notEmpty      :: [a] -> [a]
    getLength :: a -> Delta
    stripEnd  :: a -> a -> a

instance AttoparsecInput B.ByteString where
    runParser      = Data.Attoparsec.ByteString.parse      ; {-# INLINE runParser #-}
    feed           = Data.Attoparsec.ByteString.feed       ; {-# INLINE feed      #-}
    empty          = B.empty                               ; {-# INLINE empty     #-}
    isNull         = B.null                                ; {-# INLINE isNull    #-}
    notEmpty       = filter (not . B.null)                 ; {-# INLINE notEmpty  #-}
    getLength      = Delta . B.length                      ; {-# INLINE getLength #-}
    stripEnd b1 b2 = B.take (B.length b1 - B.length b2) b1 ; {-# INLINE stripEnd  #-}

instance AttoparsecInput T.Text where
    runParser = Data.Attoparsec.Text.parse ; {-# INLINE runParser #-}
    feed      = Data.Attoparsec.Text.feed  ; {-# INLINE feed      #-}
    empty     = T.empty                    ; {-# INLINE empty     #-}
    isNull    = T.null                     ; {-# INLINE isNull    #-}
    notEmpty  = filter (not . T.null)      ; {-# INLINE notEmpty  #-}
    getLength = Delta . T.length           ; {-# INLINE getLength #-}
    stripEnd (TI.Text arr1 off1 len1) (TI.Text _ _ len2) = TI.text arr1 off1 (len1 - len2) ; {-# INLINE stripEnd #-}


conduitParserEither :: (Monad m, AttoparsecInput a) => Parser a (b, Int) -> ConduitM a (Either ParseError (Span, b)) m ()
conduitParserEither parser = conduit mempty where
    conduit !pos = await >>= maybe (return ()) go where
        go x = leftover x >> sinkParserPos pos parser >>= \case
            Left e -> yield $ Left e
            Right (!pos', !off, !res) -> do
                yield $ Right (Span (pos' - pos) off, res)
                conduit pos'
{-# SPECIALIZE conduitParserEither :: Monad m => Parser T.Text       (b, Int) -> Conduit T.Text       m (Either ParseError (Span, b)) #-}
{-# SPECIALIZE conduitParserEither :: Monad m => Parser B.ByteString (b, Int) -> Conduit B.ByteString m (Either ParseError (Span, b)) #-}


sinkParserPos :: forall a b m any. (AttoparsecInput a, Monad m) => Delta -> Parser a (b, Int) -> ConduitM a any m (Either ParseError (Delta, Delta, b))
sinkParserPos pos0 p = sink empty pos0 (runParser p) where
    sink :: a -> Delta -> (a -> IResult a (b, Int)) -> ConduitM a any m (Either ParseError (Delta, Delta, b))
    sink prev pos parse = await >>= maybe close push where

        close    = go True prev $ feed (parse empty) empty
        push str = if isNull str then sink prev pos parse
                                 else go False str (parse str)
        go isEnd str = \case
            A.Done    rest (out, off)   -> (Right . (, convert off, out) $! npos rest) <$ unless (isNull rest) (leftover rest)
            A.Fail    rest contexts msg -> return . Left . ParseError contexts msg $! npos rest
            A.Partial parse'            -> if isEnd then return $ Left DivergentParser else sink str cpos parse'
            where !pos' = if isEnd then pos else cpos
                  !cpos = updateOffset pos prev
                  !npos = updateOffset pos' . stripEnd str

    updateOffset :: AttoparsecInput a => Delta -> a -> Delta
    updateOffset s x = s + getLength x ; {-# INLINE updateOffset #-}
{-# INLINE sinkParserPos #-}
{-# SPECIALIZE sinkParserPos :: Monad m => Delta -> Parser T.Text (b, Int) -> ConduitM T.Text any m (Either ParseError (Delta, Delta, b)) #-}
