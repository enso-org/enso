{-# LANGUAGE UndecidableInstances #-}

-- FIXME[WD]: refactor me
module Data.Graph.Builders where

import Prelude.Luna


import Data.Prop
import Control.Monad.Event
import Data.Direction
import Data.Index
import Data.Container          hiding (Impossible, impossible)
import Luna.Evaluation.Runtime as Runtime
import Type.Bool
import Data.Graph.Backend.VectorGraph hiding (Dynamic)
import Data.Graph.Builder.Class (MonadBuilder, modify)

---------------------------------
-- === Connection Building === --
---------------------------------

-- === Definitions === --

type family NameConnection src tgt where
    NameConnection src I   = Impossible
    NameConnection src tgt = If (Runtime.Model tgt == Static) src (Connection src tgt)

type family NameConnection2 src tgt where
    NameConnection2 src tgt = Impossible
    NameConnection2 src tgt = If (Runtime.Model tgt == Static) src (Connection src tgt)


type ConCtx src tgt conn = ( conn ~ Connection src tgt
                           , src  ~ (conn # Source)
                           , tgt  ~ (conn # Target)
                           )

type Linkable        t       m = Connectible      t   t   m
type Connectible     src tgt m = Connectible'     src tgt m (Connection     src tgt)
type ConnectibleName src tgt m = ConnectibleName' src tgt m (NameConnection src tgt)

class ConCtx src tgt conn => Connectible'        src tgt m conn |    src tgt -> conn, conn     -> src tgt where connection      ::             src -> tgt -> m (Ref Edge conn)
class                        ConnectibleName'    src tgt m conn |    src tgt -> conn, conn tgt -> src     where nameConnection  ::             src -> tgt -> m conn
class                        ConnectibleNameH rt src tgt m conn | rt src tgt -> conn, conn rt  -> src     where nameConnectionH :: Proxy rt -> src -> tgt -> m conn


-- === Instances === --


instance (LayerConstructor m (Ref Edge c), Dispatcher CONNECTION (Ref Edge c) m, c ~ Connection (Ref Node src) (Ref Node tgt)) -- Unlayered c ~ Arc src tgt
      => Connectible' (Ref Node src) (Ref Node tgt) m c where
         connection src tgt = dispatch CONNECTION =<< constructLayer (arc src tgt)

instance (ConnectibleNameH mod src tgt m conn
         , mod ~ Runtime.Model tgt)  => ConnectibleName'         src tgt m  conn where nameConnection          = nameConnectionH (Proxy :: Proxy mod) ; {-# INLINE nameConnection  #-}
instance                                ConnectibleName'         I   I   m  I    where nameConnection          = impossible
instance (Monad m, conn ~ src)       => ConnectibleNameH Static  src tgt m  conn where nameConnectionH _ src _ = return src                           ; {-# INLINE nameConnectionH #-}
instance Connectible' src tgt m conn => ConnectibleNameH Dynamic src tgt m  (Ref Edge conn) where nameConnectionH _       = connection                           ; {-# INLINE nameConnectionH #-}





reserveConnection :: MonadBuilder (Hetero (VectorGraph n e c)) m => m (Ref Edge a)
reserveConnection = Ref <$> modify (wrapped' ∘ edgeGraph $ swap ∘ ixed reserve)

class NamedConnectionReservation     src tgt m conn where reserveNamedConnection  ::             src -> Proxy tgt -> m conn
class NamedConnectionReservationH rt src tgt m conn where reserveNamedConnectionH :: Proxy rt -> src -> Proxy tgt -> m conn

instance (rt ~ Runtime.Model tgt, NamedConnectionReservationH rt src tgt m conn)
      => NamedConnectionReservation src tgt m conn where reserveNamedConnection = reserveNamedConnectionH (Proxy :: Proxy rt)

instance (Monad m, conn ~ src) => NamedConnectionReservationH Static  src tgt m conn where reserveNamedConnectionH _ src _ = return src
--instance (Monad m) => NamedConnectionReservationH Dynamic src tgt m conn where reserveNamedConnectionH _ _ _ = reserveConnection

rawConnection :: Ref Node src -> Ref Node tgt -> Connection (Ref Node src) (Ref Node tgt)
rawConnection src tgt = arc src tgt

--instance (MonadBuilder (Hetero (VectorGraph n e c)) m, Castable a e) => Constructor m (Ref Edge a) where
--    construct e = Ref <$> modify (wrapped' ∘ edgeGraph $ swap ∘ ixed add (cast e)) ; {-# INLINE construct #-}
