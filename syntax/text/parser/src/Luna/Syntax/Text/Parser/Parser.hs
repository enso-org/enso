{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# EXT      InlineAll                 #-}

module Luna.Syntax.Text.Parser.Parser (module Luna.Syntax.Text.Parser.Parser, module X) where

import Luna.Prelude

import qualified OCI.IR as IR
import OCI.IR hiding (IRBuilder, snapshot)
import Luna.Syntax.Text.Layer.Loc
import Luna.IR (UID, Succs)
import OCI.Pass
import OCI.Pass.Definition
import Luna.Syntax.Text.Parser.CodeSpan
import Text.Parser.Indent (Indent)
import Control.Monad.State.Dependent
import Text.Parser.Backend.Megaparsec ()
import Data.Text.Position
import Luna.IR.ToRefactor2 (Listener, listener, tpElemPass, addElemEventListener)

import           Text.Megaparsec       (ParsecT)

import Type.Any (AnyType)
import Luna.Syntax.Text.Parser.Marker (MarkerState, MarkedExprMap, UnmarkedExprs)


import Luna.Syntax.Text.Scope (Scope)
import Luna.Syntax.Text.Source

import qualified Luna.Syntax.Text.Lexer       as Lexer
import Luna.Syntax.Text.Parser.Class as X
import Luna.Syntax.Text.Parser.Class (Stream, Symbol)
import Luna.Syntax.Text.Parser.Errors (Invalids)
import Luna.Syntax.Text.Parser.Loc (LeftSpanner)
import           Luna.Syntax.Text.Parser.Reserved (Reservation)
import qualified Luna.Syntax.Text.Parser.Reserved as Reserved



type ParserBase = ParsecT Error Text (StateT Scope IO)
type ParserBase2 = ParsecT Error Stream IO

-- type SymParser = StatesT '[DeltaSpan, Indent, Offset] ParserBase
type SymParser = StatesT '[Indent, FileOffset, Position, MarkerState, LeftSpanner, Scope, Reservation, CodeSpanRange] ParserBase2


-------------------------
-- === IRParserRaw === --
-------------------------

-- === Definition === --

type IRBuilding m = ( MonadRef m, MonadState MarkedExprMap m, MonadState UnmarkedExprs m
                     , Req m '[ Emitter // New   // '[AnyExpr, AnyExprLink]
                              , Writer  // Net   // '[AnyExpr, AnyExprLink]
                              , Editor  // Layer // AnyExpr // CodeSpan
                              , Editor  // Attr  // Invalids
                              ]
                     -- Constraints from sub-ir parsing and cache cleaning
                     , MonadRef m, MonadPassManager m, MonadState Cache m
                     )



type IRParser a = SymParser (IRB a)
type AsgParser a = SymParser (AsgBldr a)





newtype IRB a = IRB { fromIRB :: forall m. IRBuilding m => m a } deriving (Functor)
instance Show (IRB a) where show _ = "IRB" -- FIXME: remove?

instance Applicative IRB where
    pure a                      = IRB $ pure a
    IRB f <*> IRB a = IRB $ f <*> a

instance Monad IRB where
    IRB ma >>= f = IRB $ ma >>= fromIRB . f

instance MonadFix IRB where
    mfix f = IRB $ mdo
        a <- fromIRB $ f a
        return a




type instance Generalizable (IRB a) (IRB b) = Generalizable a b

withIRx :: (forall m. IRBuilding m => m a -> m b) -> IRB a -> IRB b
withIRx f (IRB a) = IRB $ f a

withIRx2 :: (forall m. IRBuilding m => m SomeExpr -> m SomeExpr) -> IRB SomeExpr -> IRB SomeExpr
withIRx2 f (IRB a) = IRB $ f a

withAsgBldr :: (forall m. IRBuilding m => m a -> m b) -> AsgBldr a -> AsgBldr b
withAsgBldr f (AsgBldr ir) = AsgBldr $ withIRx f ir

runIRBx :: IRB a -> (forall m. IRBuilding m => m a)
runIRBx (IRB f) = f



newtype AsgBldr a = AsgBldr (IRB a) deriving (Functor, Show)
makeLenses ''AsgBldr


instance Applicative AsgBldr where
    pure = AsgBldr . pure
    AsgBldr f <*> AsgBldr a = AsgBldr $ f <*> a


instance Monad AsgBldr where
    AsgBldr ma >>= f = AsgBldr $ ma >>= unwrap . f

instance MonadFix AsgBldr where
    mfix f = AsgBldr $ mfix (unwrap . f)



------------------------
-- === ParsedExpr === --
------------------------

-- === Definition === --

newtype ParsedExpr = ParsedExpr SomeExpr deriving (Show)
makeLenses ''ParsedExpr


-----------------------------
-- === ReparsingStatus === --
-----------------------------

-- === Definition === --

newtype ReparsingStatus = ReparsingStatus [ReparsingChange] deriving (Show, Mempty, Default)
                                        {- old -}{- new -}
data    ReparsingChange = AddedExpr              SomeExpr
                        | UnchangedExpr SomeExpr SomeExpr
                        | ChangedExpr   SomeExpr SomeExpr
                        | RemovedExpr   SomeExpr
                        deriving (Show)
makeLenses ''ReparsingStatus



data Parsing
type instance Abstract Parsing = Parsing
type instance Inputs  Net   Parsing = '[AnyExpr, AnyExprLink]
type instance Outputs Net   Parsing = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer Parsing = '[AnyExpr // Model, AnyExpr // UID, Link' AnyExpr // UID, Link' AnyExpr // Model, AnyExpr // Succs, AnyExpr // CodeSpan]
type instance Outputs Layer Parsing = '[AnyExpr // CodeSpan]
type instance Inputs  Attr  Parsing = '[Invalids, Source]
type instance Outputs Attr  Parsing = '[Invalids, ParsedExpr, MarkedExprMap]
type instance Inputs  Event Parsing = '[] -- will never be used
type instance Outputs Event Parsing = '[New // AnyExpr, New // AnyExprLink]
type instance Preserves     Parsing = '[]


data Reparsing
type instance Abstract Reparsing = Reparsing
type instance Inputs  Net   Reparsing = '[AnyExpr, AnyExprLink]
type instance Outputs Net   Reparsing = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer Reparsing = '[AnyExpr // Model, AnyExpr // UID, Link' AnyExpr // UID, Link' AnyExpr // Model, AnyExpr // Succs, AnyExpr // CodeSpan]
type instance Outputs Layer Reparsing = '[AnyExpr // CodeSpan]
type instance Inputs  Attr  Reparsing = '[Invalids, Source, ParsedExpr, MarkedExprMap]
type instance Outputs Attr  Reparsing = '[Invalids, ParsedExpr, MarkedExprMap, ReparsingStatus]
type instance Inputs  Event Reparsing = '[] -- will never be used
type instance Outputs Event Reparsing = '[New // AnyExpr, New // AnyExprLink] -- , Delete // AnyExpr]
type instance Preserves     Reparsing = '[]




snapshot2 :: IRB a -> IRB (IR, a)
snapshot2 (IRB m) = IRB $ evalWithFreshIR @Parsing $ flip (,) <$> m <*> IR.snapshot

snapshotRooted2 :: IRB a -> IRB (Rooted a)
snapshotRooted2 = fmap (uncurry Rooted) . snapshot2

snapshotRooted :: AsgBldr a -> AsgBldr (Rooted a)
snapshotRooted (AsgBldr a) = AsgBldr $ snapshotRooted2 a
