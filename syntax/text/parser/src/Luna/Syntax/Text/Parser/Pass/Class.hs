{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Text.Parser.Pass.Class where

import           Prologue hiding (String, Symbol, Tok, Type)
import qualified Prologue as P

import qualified Control.Monad.State.Layered          as State
import qualified Data.Graph.Data.Graph.Class          as Graph
import qualified Data.List                            as List
import qualified Data.Set                             as Set
import qualified Luna.IR                              as IR
import qualified Luna.IR                              as IR
import qualified Luna.Pass                            as Pass
import qualified Luna.Pass.Attr                       as Attr
import qualified Luna.Syntax.Text.Lexer               as Lexer
import qualified Luna.Syntax.Text.Parser.State.Marker as Marker
import qualified OCI.Pass.Definition.Declaration      as Pass
import qualified OCI.Pass.Definition.Interface        as Pass
import qualified Text.Megaparsec                      as Parser
import qualified Text.Megaparsec.Error                as Error
import qualified Text.Megaparsec.Error                as Error

import qualified Data.Graph.Component.Node.Construction as Term
import qualified Data.Graph.Data                        as Component
import qualified Data.Graph.Data.Component.Class        as XX


import Control.Monad.State.Layered           (StatesT)
import Data.Set                              (Set)
import Data.Text.Position                    (Delta)
import Luna.Pass                             (Pass)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Data.Invalid  (Invalids)
import Luna.Syntax.Text.Parser.Data.Result   (Result)
import Luna.Syntax.Text.Source               (Source)



data Parsing

type instance Graph.Components      Parsing          = '[IR.Terms, IR.Links]
type instance Graph.ComponentLayers Parsing IR.Links = '[IR.Target, IR.Source]
type instance Graph.ComponentLayers Parsing IR.Terms
   = '[IR.Users, IR.Model, IR.Type, CodeSpan]


-------------------------
-- === Parser pass === --
-------------------------

-- === Definition === --

data Parser
type ParserPass pass = Pass.Interface Parser pass
type instance Pass.Spec Parser t = ParserSpec t
type family   ParserSpec  t where
    ParserSpec (Pass.In  Pass.Attrs) = '[Source, Result, Invalids]
    ParserSpec (Pass.In  IR.Terms)   = CodeSpan
                                    ': Pass.BasicPassSpec (Pass.In IR.Terms)
    ParserSpec (Pass.Out t)          = ParserSpec (Pass.In t)
    ParserSpec t                     = Pass.BasicPassSpec t

-- Pass.cache_phase1 ''Parser
-- Pass.cache_phase2 ''Parser

type IRBMonad m =
    ( MonadIO m
    , Pass.Interface Parser m
    , State.Monad Marker.TermOrphanList m
    , State.Monad Marker.TermMap m
    )



------------------------------
-- === IRB (IR Builder) === --
------------------------------

-- === Definition === --

newtype IRB a = IRB { fromIRB :: ∀ m. IRBMonad m => m a }
    deriving (Functor)

irb0 :: (∀ m. IRBMonad m => m a) -> IRB a
irb1 :: (∀ m. IRBMonad m => t1 -> m a) -> t1 -> IRB a
irb2 :: (∀ m. IRBMonad m => t1 -> t2 -> m a) -> t1 -> t2 -> IRB a
irb3 :: (∀ m. IRBMonad m => t1 -> t2 -> t3 -> m a) -> t1 -> t2 -> t3 -> IRB a
irb4 :: (∀ m. IRBMonad m => t1 -> t2 -> t3 -> t4 -> m a)
                         -> t1 -> t2 -> t3 -> t4 -> IRB a
irb5 :: (∀ m. IRBMonad m => t1 -> t2 -> t3 -> t4 -> t5 -> m a)
                         -> t1 -> t2 -> t3 -> t4 -> t5 -> IRB a
irb0 = IRB                                         ; {-# INLINE irb0 #-}
irb1 = \f t1 -> IRB $ f t1                         ; {-# INLINE irb1 #-}
irb2 = \f t1 t2 -> IRB $ f t1 t2                   ; {-# INLINE irb2 #-}
irb3 = \f t1 t2 t3 -> IRB $ f t1 t2 t3             ; {-# INLINE irb3 #-}
irb4 = \f t1 t2 t3 t4 -> IRB $ f t1 t2 t3 t4       ; {-# INLINE irb4 #-}
irb5 = \f t1 t2 t3 t4 t5 -> IRB $ f t1 t2 t3 t4 t5 ; {-# INLINE irb5 #-}


-- === API === --

withIRB :: (∀ m. IRBMonad m => (m a -> m b)) -> IRB a -> IRB b
withIRB = \f (IRB ma) -> IRB $ f ma
{-# INLINE withIRB #-}

runIRB :: ParserPass (Pass stage Parser)
       => IRB a
       -> StatesT '[Marker.TermOrphanList, Marker.TermMap] (Pass stage Parser) a
runIRB = \(IRB ma) -> ma
{-# INLINE runIRB #-}


-- === Instances === --

instance Applicative IRB where
    pure  = \a -> IRB $ pure a                ; {-# INLINE pure  #-}
    (<*>) = \(IRB f) (IRB a) -> IRB $ f <*> a ; {-# INLINE (<*>) #-}

instance Monad IRB where
    (>>=) = \(IRB ma) f -> IRB $ ma >>= fromIRB . f
    {-# INLINE (>>=) #-}

instance MonadIO IRB where
    liftIO = \ma -> IRB $ liftIO ma
    {-# INLINE liftIO #-}

instance Show (IRB a) where
    show _ = "IRB"


---------------------------------------
-- === IRBS (IR Builder Spanned) === --
---------------------------------------

-- | Both IRB and IRBS are Luna IR building monads, however the construction
--   of IRBS is handled by functions which guarantee that IRB has all code
--   spanning information encoded

-- === Definition === --

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

