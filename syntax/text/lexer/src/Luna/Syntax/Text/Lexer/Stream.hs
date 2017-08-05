module Luna.Syntax.Text.Lexer.Stream where

import Prologue hiding (empty, unless, span)


import           Control.Exception          (Exception)
import           Control.Monad              (unless)
import qualified Data.ByteString            as B
import qualified Data.Text                  as T
import qualified Data.Text.Internal         as TI
import           Data.Typeable              (Typeable)

import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text
import qualified Data.Attoparsec.Types      as Parser
import           Data.Attoparsec.Types (Parser, IResult)
import           Data.Conduit
import Control.Monad.Trans.Resource (MonadThrow, monadThrow)

import Data.Text.Position (Delta)


---------------------------
-- === Conduit utils === --
---------------------------

awaitJust :: Monad m => (a -> ConduitM a o m ()) -> ConduitM a o m ()
awaitJust f = await >>= maybe (return ()) f ; {-# INLINE awaitJust #-}

withPeek :: Monad m => (a -> ConduitM a o m ()) -> ConduitM a o m ()
withPeek f = awaitJust $ \x -> leftover x >> f x ; {-# INLINE withPeek #-}

whenNonEmpty :: Monad m => ConduitM a o m () -> ConduitM a o m ()
whenNonEmpty = withPeek . const ; {-# INLINE whenNonEmpty #-}

useRight :: Monad m => (t -> ConduitM i (Either a b) m ()) -> Either a t -> ConduitM i (Either a b) m ()
useRight f = \case
    Left  e -> yield $ Left e
    Right v -> f v
{-# INLINE useRight #-}

--
--
-- -------------------
-- -- === Delta === --
-- -------------------
--
-- -- === Definition === --
--
-- newtype Delta = Delta Int deriving (Eq, NFData, Num, Ord)
-- makeLenses ''Delta
--
--
-- -- === Instances === --
--
-- instance Show   Delta where show   = show . unwrap ; {-# INLINE show   #-}
-- instance Mempty Delta where mempty = Delta 0       ; {-# INLINE mempty #-}
--
-- instance Convertible Int Delta where convert = coerce ; {-# INLINE convert #-}
-- instance Convertible Delta Int where convert = coerce ; {-# INLINE convert #-}
--


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

data Token a = Token
    { _span   :: !Delta
    , _offset :: !Delta
    , _symbol :: !a
    } deriving (Eq, Generic)
makeLenses ''Token


-- === Instances === --

instance NFData a => NFData (Token a)
instance Show a => Show (Token a) where
    showsPrec d t = showParen' d
        $ showString "Token "
        . showsPrec' (t ^. span)
        . showString " "
        . showsPrec' (t ^. offset)
        . showString " "
        . showsPrec' (t ^. symbol)



-----------------------------------------
-- === Attoparsec Conduit bindings === --
-----------------------------------------

class AttoparsecInput a where
    runParser :: Parser  a b -> a -> IResult a b
    feed      :: IResult a b -> a -> IResult a b
    empty     :: a
    isNull    :: a -> Bool
    getLength :: a -> Delta
    stripEnd  :: a -> a -> a

instance AttoparsecInput B.ByteString where
    runParser      = Data.Attoparsec.ByteString.parse      ; {-# INLINE runParser #-}
    feed           = Data.Attoparsec.ByteString.feed       ; {-# INLINE feed      #-}
    empty          = B.empty                               ; {-# INLINE empty     #-}
    isNull         = B.null                                ; {-# INLINE isNull    #-}
    getLength      = convert . B.length                    ; {-# INLINE getLength #-}
    stripEnd b1 b2 = B.take (B.length b1 - B.length b2) b1 ; {-# INLINE stripEnd  #-}

instance AttoparsecInput T.Text where
    runParser = Data.Attoparsec.Text.parse ; {-# INLINE runParser #-}
    feed      = Data.Attoparsec.Text.feed  ; {-# INLINE feed      #-}
    empty     = T.empty                    ; {-# INLINE empty     #-}
    isNull    = T.null                     ; {-# INLINE isNull    #-}
    getLength = convert . T.length         ; {-# INLINE getLength #-}
    stripEnd (TI.Text arr1 off1 len1) (TI.Text _ _ len2) = TI.text arr1 off1 (len1 - len2) ; {-# INLINE stripEnd #-}


conduitParserEither :: (Monad m, AttoparsecInput a, Default cfg) => (cfg -> Parser a ((b, Int), cfg)) -> ConduitM a (Either ParseError (Token b)) m ()
conduitParserEither parser = loop def mempty where
    loop !cfg !pos = whenNonEmpty $ sinkPosParser pos (parser cfg) >>= useRight go where
        go (!pos', !off, !cfg', !res) = yield (Right $ Token (pos' - pos) off res) >> loop cfg' pos'
{-# INLINE conduitParserEither #-}
-- {-# SPECIALIZE conduitParserEither :: Monad m => Parser T.Text       (b, Int) -> Conduit T.Text       m (Either ParseError (Span, b)) #-}
-- {-# SPECIALIZE conduitParserEither :: Monad m => Parser B.ByteString (b, Int) -> Conduit B.ByteString m (Either ParseError (Span, b)) #-}

sinkPosParser :: forall a b cfg m any. (AttoparsecInput a, Monad m) => Delta -> Parser a ((b, Int), cfg) -> ConduitM a any m (Either ParseError (Delta, Delta, cfg, b))
sinkPosParser pos0 p = sink empty pos0 (runParser p) where
    sink :: a -> Delta -> (a -> IResult a ((b, Int), cfg)) -> ConduitM a any m (Either ParseError (Delta, Delta, cfg, b))
    sink prev pos parse = await >>= maybe close push where

        close    = go True prev $ feed (parse empty) empty
        push str = if isNull str then sink prev pos parse
                                 else go False str (parse str)
        go isEnd str = \case
            Parser.Done    rest ((out, off), cfg) -> (Right . (, convert off, cfg, out) $! npos rest - convert off) <$ unless (isNull rest) (leftover rest)
            Parser.Fail    rest contexts msg -> return . Left . ParseError contexts msg $! npos rest
            Parser.Partial parse'            -> if isEnd then return $ Left DivergentParser else sink str cpos parse'
            where !pos' = if isEnd then pos else cpos
                  !cpos = updateOffset pos prev
                  !npos = updateOffset pos' . stripEnd str

    updateOffset :: AttoparsecInput a => Delta -> a -> Delta
    updateOffset s x = s + getLength x ; {-# INLINE updateOffset #-}
{-# INLINE sinkPosParser #-}
-- {-# SPECIALIZE sinkPosParser :: Monad m => Delta -> Parser T.Text (b, Int) -> ConduitM T.Text any m (Either ParseError (Delta, Delta, b)) #-}
