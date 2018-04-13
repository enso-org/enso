{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# EXT      InlineAll                 #-}

module Luna.Syntax.Text.Parser.Parser (module Luna.Syntax.Text.Parser.Parser, module X) where

import Prologue

-- import qualified OCI.IR as IR
-- import OCI.IR hiding (IRBuilder, snapshot)
-- import Luna.Syntax.Text.Layer.Loc
-- import Luna.IR (UID, Succs)
-- import OCI.Pass
-- import OCI.Pass.Definition
-- import Text.Parser.Indent (Indent)
-- import Control.Monad.State.Dependent
import Text.Parser.Backend.Megaparsec ()
-- import Data.Text.Position
-- import Luna.IR.ToRefactor2 (Listener, listener, tpElemPass, addElemEventListener)


-- import Type.Any (AnyType)


-- import Luna.Syntax.Text.Source
import qualified Control.Monad.State.Layered         as State
import qualified Luna.IR                             as IR
import qualified Luna.IR.Component.Term.Construction as Term
import qualified Luna.Pass                           as Pass
import qualified Luna.Syntax.Text.Lexer              as Lexer
import qualified Luna.Syntax.Text.Parser.Class       as Parser
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
import Luna.Syntax.Text.Parser.Reserved  (Reservation)
import Luna.Syntax.Text.Scope            (Scope)
import Text.Megaparsec                   (ParseError, ParsecT)
import Text.Parser.Indent                (Indent)


-- import qualified Luna.Syntax.Text.Parser.Reserved as Reserved



-- type ParserBase = ParsecT Error Text (StateT Scope IO)
type ParserBase2 = ParsecT Error Stream IO

-- type SymParser = StatesT '[Indent, FileOffset, Position, MarkerState
--     , LeftSpanner, Scope, Reservation, CodeSpanRange] ParserBase2
type SymParser = StatesT ParserStates ParserBase2
type ParserStates
    = '[ Indent
       , FileOffset
       , MarkerState
       , LeftSpanner
       , Scope
       , Reservation
       , CodeSpanRange
       ]


runParserInternal :: MonadIO m => ParserBase2 a -> Stream
                  -> m (Either (ParseError Parser.Tok Error) a)
runParserInternal p s = liftIO $ Parser.runParserT p "" s

runParserT :: MonadIO m => SymParser a -> Stream
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
-- newtype IRB a = IRB { fromIRB :: Pass Parser a } deriving (Functor, Applicative, Monad)
-- makeLenses ''IRB


---------------------
-- === AsgBldr === --
---------------------

-- === Definition === --

newtype AsgBldr a = AsgBldr { fromAsgBldr :: IRB a }
    deriving (Functor, Applicative, Monad)
makeLenses ''AsgBldr

instance Show (AsgBldr a) where
    show _ = "AsgBldr"




-- === Instances === --

-- instance Applicative IRB where
--     pure a = IRB $ pure a           ; {-# INLINE pure #-}
--     IRB f <*> IRB a = IRB $ f <*> a ; {-# INLINE (<*>) #-}

--     instance Monad IRB where
--         IRB ma >>= f = IRB $ ma >>= fromIRB . f ; {-# INLINE (>>=) #-}


-- newtype IRB a = IRB { fromIRB :: ∀ m. IRBuilding m => m a } deriving (Functor)

-- type IRBuilding m = ( MonadRef m, MonadState MarkedExprMap m, MonadState UnmarkedExprs m
--                      , Req m '[ Emitter // New   // '[AnyExpr, AnyExprLink]
--                               , Writer  // Net   // '[AnyExpr, AnyExprLink]
--                               , Editor  // Layer // AnyExpr // CodeSpan
--                               , Editor  // Attr  // Invalids
--                               ]
--                      -- Constraints from sub-ir parsing and cache cleaning
--                      , MonadRef m, MonadPassManager m, MonadState Cache m
--                      )



-- type IRParser a = SymParser (IRB a)
type AsgParser a = SymParser (AsgBldr a)




-- instance Show (IRB a) where show _ = "IRB" -- FIXME: remove?


-- instance MonadFix IRB where
--     mfix f = IRB $ mdo
--         a <- fromIRB $ f a
--         return a




-- type instance Generalizable (IRB a) (IRB b) = Generalizable a b

-- withIRx :: (forall m. IRBuilding m => m a -> m b) -> IRB a -> IRB b
-- withIRx f (IRB a) = IRB $ f a

-- withIRx2 :: (forall m. IRBuilding m => m SomeExpr -> m SomeExpr) -> IRB SomeExpr -> IRB SomeExpr
-- withIRx2 f (IRB a) = IRB $ f a

withAsgBldr :: (IRB a -> IRB b) -> AsgBldr a -> AsgBldr b
withAsgBldr f (AsgBldr ir) = AsgBldr $ f ir

-- runIRBx :: IRB a -> (forall m. IRBuilding m => m a)
-- runIRBx (IRB f) = f


-- ---------------------
-- -- === AsgBldr === --
-- ---------------------

-- -- === Definition === --

-- newtype AsgBldr a = AsgBldr { fromAsgBldr :: IRB a } deriving (Functor)
-- makeLenses ''AsgBldr


-- -- === Instances === --

-- instance Applicative AsgBldr where
--     pure = AsgBldr . pure                       ; {-# INLINE pure #-}
--     AsgBldr f <*> AsgBldr a = AsgBldr $ f <*> a ; {-# INLINE (<*>) #-}

-- instance Monad AsgBldr where
--     AsgBldr ma >>= f = AsgBldr $ ma >>= unwrap . f ; {-# INLINE (>>=) #-}


-- instance MonadFix AsgBldr where
--     mfix f = AsgBldr $ mfix (unwrap . f)



-- ------------------------
-- -- === ParsedExpr === --
-- ------------------------

-- -- === Definition === --

-- newtype ParsedExpr = ParsedExpr SomeExpr deriving (Show)
-- makeLenses ''ParsedExpr


-- -----------------------------
-- -- === ReparsingStatus === --
-- -----------------------------

-- -- === Definition === --

-- newtype ReparsingStatus = ReparsingStatus [ReparsingChange] deriving (Show, Mempty, Default)
--                                         {- old -}{- new -}
-- data    ReparsingChange = AddedExpr              SomeExpr
--                         | UnchangedExpr SomeExpr SomeExpr
--                         | ChangedExpr   SomeExpr SomeExpr
--                         | RemovedExpr   SomeExpr
--                         deriving (Show)
-- makeLenses ''ReparsingStatus



-- data Parsing
-- type instance Abstract Parsing = Parsing
-- type instance Inputs  Net   Parsing = '[AnyExpr, AnyExprLink]
-- type instance Outputs Net   Parsing = '[AnyExpr, AnyExprLink]
-- type instance Inputs  Layer Parsing = '[AnyExpr // Model, AnyExpr // UID, Link' AnyExpr // UID, Link' AnyExpr // Model, AnyExpr // Succs, AnyExpr // CodeSpan]
-- type instance Outputs Layer Parsing = '[AnyExpr // CodeSpan]
-- type instance Inputs  Attr  Parsing = '[Invalids, Source]
-- type instance Outputs Attr  Parsing = '[Invalids, ParsedExpr, MarkedExprMap]
-- type instance Inputs  Event Parsing = '[] -- will never be used
-- type instance Outputs Event Parsing = '[New // AnyExpr, New // AnyExprLink]
-- type instance Preserves     Parsing = '[]


-- data Reparsing
-- type instance Abstract Reparsing = Reparsing
-- type instance Inputs  Net   Reparsing = '[AnyExpr, AnyExprLink]
-- type instance Outputs Net   Reparsing = '[AnyExpr, AnyExprLink]
-- type instance Inputs  Layer Reparsing = '[AnyExpr // Model, AnyExpr // UID, Link' AnyExpr // UID, Link' AnyExpr // Model, AnyExpr // Succs, AnyExpr // CodeSpan]
-- type instance Outputs Layer Reparsing = '[AnyExpr // CodeSpan]
-- type instance Inputs  Attr  Reparsing = '[Invalids, Source, ParsedExpr, MarkedExprMap]
-- type instance Outputs Attr  Reparsing = '[Invalids, ParsedExpr, MarkedExprMap, ReparsingStatus]
-- type instance Inputs  Event Reparsing = '[] -- will never be used
-- type instance Outputs Event Reparsing = '[New // AnyExpr, New // AnyExprLink] -- , Delete // AnyExpr]
-- type instance Preserves     Reparsing = '[]




-- snapshot2 :: IRB a -> IRB (IR, a)
-- snapshot2 (IRB m) = IRB $ evalWithFreshIR @Parsing $ flip (,) <$> m <*> IR.snapshot

-- snapshotRooted2 :: IRB a -> IRB (Rooted a)
-- snapshotRooted2 = fmap (uncurry Rooted) . snapshot2

-- snapshotRooted :: AsgBldr a -> AsgBldr (Rooted a)
-- snapshotRooted (AsgBldr a) = AsgBldr $ snapshotRooted2 a
