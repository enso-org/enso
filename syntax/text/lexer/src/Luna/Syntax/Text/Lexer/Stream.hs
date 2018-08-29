{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Syntax.Text.Lexer.Stream where

import Prologue hiding (unless, span)

import           Control.Exception          (Exception)
import           Control.Monad              (unless)
import qualified Data.ByteString            as B
import qualified Data.Text                  as T
import qualified Data.Text.Internal         as TI
import           Data.Typeable              (Typeable)

import qualified Data.Attoparsec.Types      as Parser
import           Data.Attoparsec.Types (Parser, IResult)
import           Data.Conduit

import Data.Text.Position (Delta)
import Data.Parser hiding (Token)
import Data.Parser.Instances.Attoparsec ()
import Luna.Syntax.Text.Lexer.Token
import Data.Text32 (Text32)
import Data.Conduit.Utils
import qualified Data.Text32 as Text32



-------------------
-- === Error === --
-------------------

-- | The context and message from a 'A.Fail' value.
data ParseError = ParseError
    { errorContexts :: ![String]
    , errorMessage  :: !String
    , errorSpan     :: {-# UNPACK #-} !Delta
    } | DivergentParser
    deriving (Generic, Show, Typeable)
instance Exception ParseError
instance NFData ParseError



-----------------------------------------
-- === Attoparsec Conduit bindings === --
-----------------------------------------

class AttoparsecInput a where
    isNull    :: a -> Bool
    getLength :: a -> Delta
    stripEnd  :: a -> a -> a

instance AttoparsecInput B.ByteString where
    isNull         = B.null                                ; {-# INLINE isNull    #-}
    getLength      = convert . B.length                    ; {-# INLINE getLength #-}
    stripEnd b1 b2 = B.take (B.length b1 - B.length b2) b1 ; {-# INLINE stripEnd  #-}

instance AttoparsecInput T.Text where
    isNull    = T.null                     ; {-# INLINE isNull    #-}
    getLength = convert . T.length         ; {-# INLINE getLength #-}
    stripEnd (TI.Text arr1 off1 len1) (TI.Text _ _ len2) = TI.text arr1 off1 (len1 - len2) ; {-# INLINE stripEnd #-}

instance AttoparsecInput Text32 where
    isNull    = Text32.null                     ; {-# INLINE isNull    #-}
    getLength = convert . Text32.length         ; {-# INLINE getLength #-}
    stripEnd v v' = Text32.unsafeTake (Text32.length v - Text32.length v') v ; {-# INLINE stripEnd #-}


conduitParserEither :: (AttoparsecInput a, PartialParser (Parser a), Default cfg, Monad m, Mempty a)
                    => cfg -> (cfg -> Parser a ((b, Int), cfg)) -> ConduitM a (Either ParseError (Token b)) m ()
conduitParserEither !cfg parser = loop cfg mempty where
    loop !lcfg !pos = whenNonEmpty $ sinkPosParser pos (parser lcfg) >>= useRight go where
        go (!pos', !off, !cfg', !res) = yield (Right $ Token (pos' - pos) off res) >> loop cfg' pos'
{-# INLINE conduitParserEither #-}

sinkPosParser :: forall a b cfg m any. (AttoparsecInput a, PartialParser (Parser a), Monad m, Mempty a)
              => Delta -> Parser a ((b, Int), cfg) -> ConduitM a any m (Either ParseError (Delta, Delta, cfg, b))
sinkPosParser !pos0 p = sink mempty pos0 (parsePartial p) where
    sink :: a -> Delta -> (a -> IResult a ((b, Int), cfg)) -> ConduitM a any m (Either ParseError (Delta, Delta, cfg, b))
    sink prev !pos parse = await >>= maybe close push where

        close    = go True prev $ closePartial (parse mempty)
        push str = if isNull str then sink prev pos parse
                                 else go False str (parse str)
        go isEnd str = \case
            Parser.Done    rest ((out, off), cfg) -> (Right . (, convert off, cfg, out) $! npos rest - convert off) <$ unless (isNull rest) (leftover rest)
            Parser.Fail    rest contexts msg -> pure . Left . ParseError contexts msg $! npos rest
            Parser.Partial parse'            -> if isEnd then pure $ Left DivergentParser else sink str cpos parse'
            where !pos' = if isEnd then pos else cpos
                  !cpos = updateOffset pos prev
                  !npos = updateOffset pos' . stripEnd str

    updateOffset :: AttoparsecInput a => Delta -> a -> Delta
    updateOffset s x = s + getLength x ; {-# INLINE updateOffset #-}
{-# INLINE sinkPosParser #-}
-- {-# SPECIALIZE sinkPosParser :: Monad m => Delta -> Parser T.Text (b, Int) -> ConduitM T.Text any m (Either ParseError (Delta, Delta, b)) #-}

