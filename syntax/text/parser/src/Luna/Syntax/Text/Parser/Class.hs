{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Parser.Class where

import           Prologue hiding (String, Symbol, Tok, Type)
import qualified Prologue as P

import qualified Luna.IR as IR
-- import           Luna.Syntax.Text.Layer.Loc
-- import OCI.Pass.Class
-- import OCI.Pass.Definition

import Luna.Syntax.Text.Parser.CodeSpan
-- import Luna.Syntax.Text.Parser.Marker   (MarkedExprMap)
-- -- import Luna.Syntax.Text.Source (Source)
-- import qualified OCI.IR                           as IR
import qualified Data.List              as List
import qualified Data.Set               as Set
import qualified Luna.IR                as IR
import qualified Luna.Pass              as Pass
import qualified Luna.Syntax.Text.Lexer as Lexer
import qualified Luna.Syntax.Text.Lexer as Lexer
import qualified Text.Megaparsec        as Parser
import qualified Text.Megaparsec.Error  as Error
import qualified Text.Megaparsec.Error  as Error


import Control.Monad.State.Layered    (StatesT)
import Data.Set                       (Set)
import Data.Text.Position             (Delta)
import Luna.Pass                      (Pass)
import Luna.Syntax.Text.Parser.Errors (Invalids)
import Luna.Syntax.Text.Parser.Marker (MarkedExprMap, MarkerState,
                                       UnmarkedExprs)
import Luna.Syntax.Text.Source        (Source)
import Text.Megaparsec                hiding (Pos, Stream, parse, uncons, (<?>))


data Parser

type instance Pass.Spec Parser t = ParserSpec t
type family   ParserSpec  t where
    ParserSpec (Pass.In  Pass.Attrs) = '[Source, Invalids]
    ParserSpec (Pass.In  IR.Terms)   = CodeSpan
                                    ': Pass.BasicPassSpec (Pass.In IR.Terms)
    ParserSpec (Pass.Out t)          = ParserSpec (Pass.In t)
    ParserSpec t                     = Pass.BasicPassSpec t

Pass.cache_phase1 ''Parser
Pass.cache_phase2 ''Parser


-----------------
-- === IRB === --
-----------------

-- === Definition === --

-- | IRB:  IR Builder
--   IRBS: IR Builder Spanned
--   Both IRB and IRBS are Luna IR building monads, however the construction
--   of IRBS is handled by functions which guarantee that IRB has all code
--   spanning information encoded

type    IRB    = StatesT '[UnmarkedExprs, MarkedExprMap] (Pass Parser)
newtype IRBS a = IRBS (IRB a)
    deriving (Functor, Applicative, Monad)
makeLenses ''IRBS


-- === Utils === --

fromIRBS :: IRBS a -> IRB a
fromIRBS (IRBS a) = a ; {-# INLINE fromIRBS #-}

liftIRBS1 :: (t1             -> IRB out) -> IRBS t1                       -> IRB out
liftIRBS2 :: (t1 -> t2       -> IRB out) -> IRBS t1 -> IRBS t2            -> IRB out
liftIRBS3 :: (t1 -> t2 -> t3 -> IRB out) -> IRBS t1 -> IRBS t2 -> IRBS t3 -> IRB out
liftIRBS1 f t1       = bind  f (fromIRBS t1)                             ; {-# INLINE liftIRBS1 #-}
liftIRBS2 f t1 t2    = bind2 f (fromIRBS t1) (fromIRBS t2)               ; {-# INLINE liftIRBS2 #-}
liftIRBS3 f t1 t2 t3 = bind3 f (fromIRBS t1) (fromIRBS t2) (fromIRBS t3) ; {-# INLINE liftIRBS3 #-}


-- === Instances === --

instance Show (IRBS a) where
    show _ = "IRBS"



-- --------------------
-- -- === Result === --
-- --------------------

-- newtype Result = Result (IR.Term IR.Unit) deriving (Show, Eq, Mempty)
-- type instance Attr.Type Result = Attr.Atomic







infix 1 <?>
(<?>) :: MonadParsec e s m => m a -> P.String -> m a
(<?>) = (Parser.<?>) ; {-# INLINE (<?>) #-}

type Symbol      = Lexer.Symbol
type Stream      = [Tok]
type Error       = Void -- Error.Dec
type Tok         = Lexer.Token Lexer.Symbol
type MonadParser = MonadParsec Error Stream

app_tmp a (x,y) = (a:x, y)

-- FIXME[WD]: Describe the hacks
instance Parser.Stream Stream where
    type Token  Stream = Tok
    type Tokens Stream = [Tok]

    tokenToChunk  _ = pure   ; {-# INLINE tokenToChunk  #-}
    tokensToChunk _ = id     ; {-# INLINE tokensToChunk #-}
    chunkToTokens _ = id     ; {-# INLINE chunkToTokens #-}
    chunkLength   _ = length ; {-# INLINE chunkLength   #-}
    chunkEmpty    _ = null   ; {-# INLINE chunkEmpty    #-}
    positionAt1   _ p _ = p  -- FIXME
    positionAtN   _ p _ = p  -- FIXME
    advance1      _ _ p _ = p  -- FIXME
    advanceN      _ _ p _ = p  -- FIXME
    take1_              = \case (a:as) -> Just (a,as)
                                []     -> Nothing
    takeN_        0 s      = Just ([],s)
    takeN_        i (a:as) = app_tmp a <$> takeN_ (i - 1) as
    takeN_        _ _      = Nothing

    takeWhile_    f     = \case []     -> ([], [])
                                (a:as) -> if f a then app_tmp a (takeWhile_ f as)
                                                 else ([], (a:as))

    -- takeWhile_ :: (Token s -> Bool) -> s -> (Tokens s, s)
    -- uncons = List.uncons
    -- updatePos _ _ cpos _ = (cpos, cpos)
    -- updatePos _ _ (cpos@(Parser.SourcePos n l c)) (Lexer.Token (Lexer.Span w o) _) = (cpos, Parser.SourcePos n (Parser.unsafePos . unsafeConvert $ unwrap o + 1)
    --                                                                                                            (Parser.unsafePos $ Parser.unPos c + (unsafeConvert $ unwrap $ w + o)))

instance Parser.ShowToken Tok where
    showTokens = show





