{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# EXT      InlineAll                 #-}

module Luna.Syntax.Text.Parser.Pass where

import Prologue

import qualified Control.Monad.State.Layered         as State
import qualified Luna.IR                             as IR
import qualified Luna.IR.Component.Term.Construction as Term
import qualified Luna.Pass                           as Pass
import qualified Luna.Syntax.Text.Lexer              as Lexer
import qualified Luna.Syntax.Text.Parser.Class       as Parser
import qualified Luna.Syntax.Text.Parser.Parser      as Class
import qualified Text.Megaparsec                     as Parser


import Control.Monad.State.Layered       (StatesT)
import Control.Monad.State.Layered       (StateT)
import Data.Text.Position                (FileOffset)
import Luna.Pass                         (Pass)
import Luna.Syntax.Text.Parser.Class     as X (Error)
import Luna.Syntax.Text.Parser.Class     (Stream)
import Luna.Syntax.Text.Parser.CodeSpan  (CodeSpan, CodeSpanRange)
import Luna.Syntax.Text.Parser.Errors    (Invalids)
import Luna.Syntax.Text.Parser.Hardcoded (hardcode)
import Luna.Syntax.Text.Parser.Loc       (LeftSpanner)
import Luna.Syntax.Text.Parser.Marker    (MarkedExprMap, MarkerState,
                                          UnmarkedExprs)
import Luna.Syntax.Text.Parser.Parser    (ParserBase)
import Luna.Syntax.Text.Parser.Reserved  (Reservation)
import Luna.Syntax.Text.Scope            (Scope)
import Text.Megaparsec                   (ParseError, ParsecT)
import Text.Parser.Backend.Megaparsec    ()
import Text.Parser.Indent                (Indent)


runParserInternal :: MonadIO m => ParserBase a -> Stream
                  -> m (Either (ParseError Parser.Tok Error) a)
runParserInternal p s = liftIO $ Parser.runParserT p "" s

runParserT :: MonadIO m => Class.Parser a -> Stream
           -> m (Either (ParseError Parser.Tok Error) a)
runParserT p s = flip runParserInternal s
               $ State.evalDefT @CodeSpanRange
               $ State.evalDefT @Reservation
               $ State.evalDefT @Scope
               $ State.evalDefT @LeftSpanner
               $ State.evalDefT @MarkerState
            --    $ State.evalDefT @Position
               $ State.evalDefT @FileOffset
               $ State.evalDefT @Indent
               $ hardcode >> p

data Parser
type instance Pass.Spec Parser t = TestPassSpec t
type family   TestPassSpec  t where
    TestPassSpec (Pass.In  Pass.Attrs) = '[Invalids]
    TestPassSpec (Pass.In  IR.Terms)   = CodeSpan
                                      ': Pass.BasicPassSpec (Pass.In IR.Terms)
    TestPassSpec (Pass.Out t)          = TestPassSpec (Pass.In t)
    TestPassSpec t                     = Pass.BasicPassSpec t

Pass.cache_phase1 ''Parser
Pass.cache_phase2 ''Parser






-----------------
-- === IRB === --
-----------------

-- -- === Definition === --

type IRB = StatesT '[UnmarkedExprs, MarkedExprMap] (Pass Parser)

type IRBParser  a = Class.Parser (IRB  a)


------------------
-- === IRBS === --
------------------

-- | IRBS is abbreviation to 'IR Builder Spanned', which is IRB with
--   code span information attached.


-- === Definition === --

newtype IRBS a = IRBS { fromIRBS :: IRB a }
    deriving (Functor, Applicative, Monad)
makeLenses ''IRBS

type IRBSParser a = Class.Parser (IRBS a)


-- === Utils === --

liftIRBS1 :: (t1             -> IRB out) -> IRBS t1                       -> IRB out
liftIRBS2 :: (t1 -> t2       -> IRB out) -> IRBS t1 -> IRBS t2            -> IRB out
liftIRBS3 :: (t1 -> t2 -> t3 -> IRB out) -> IRBS t1 -> IRBS t2 -> IRBS t3 -> IRB out
liftIRBS1 f t1       = bind  f (fromIRBS t1)                             ; {-# INLINE liftIRBS1 #-}
liftIRBS2 f t1 t2    = bind2 f (fromIRBS t1) (fromIRBS t2)               ; {-# INLINE liftIRBS2 #-}
liftIRBS3 f t1 t2 t3 = bind3 f (fromIRBS t1) (fromIRBS t2) (fromIRBS t3) ; {-# INLINE liftIRBS3 #-}


-- === Instances === --

instance Show (IRBS a) where
    show _ = "IRBS"


withAsgBldr :: (IRB a -> IRB b) -> IRBS a -> IRBS b
withAsgBldr f (IRBS ir) = IRBS $ f ir
