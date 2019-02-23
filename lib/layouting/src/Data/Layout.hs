{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE PatternSynonyms           #-}

module Data.Layout where

import qualified Prelude as P

import Prologue hiding ( Bounded, div, concat, putStr, length, putStrLn, take
                       , drop, nested, lines )

import qualified Data.Foldable             as Foldable
import qualified Data.Text                 as Text
import           Data.Text.Terminal        hiding (plain) -- FIXME[WD]: TerminalText instances might not suit this module well
import Data.Sequence.Class



-- === Concatenation utils === --

betweenWith :: (a -> a -> a) -> a -> a -> a -> a
betweenWith f l r m = l `f` m `f` r

surroundedWith :: (a -> a -> a) -> a -> a -> a -> a
surroundedWith f m l r = betweenWith f l r m

between :: Semigroup a => a -> a -> a -> a
between = betweenWith (<>)

between' :: Semigroup a => a -> a -> a
between' a = between a a


-- === Text combinators === --

space :: IsString a => a
space = " "

parensed, bracked, braced, chevroned, spaced, quoted, singleQuoted, backticked :: (Semigroup a, IsString a) => a -> a
parensed     = between "(" ")"
bracked      = between "[" "]"
braced       = between "{" "}"
chevroned    = between "<" ">"
spaced       = between' " "
quoted       = between' "\""
singleQuoted = between' "'"
backticked   = between' "`"


-- === Text layouting === --

enumerateWith :: (Monoid a, Foldable f) => a -> a -> f a -> a
enumerateWith sep lastSep els = case Foldable.toList els of
    [] -> mempty
    ss -> intercalate sep (unsafeInit ss) <> lastSep <> unsafeLast ss

enumerateAlt, enumerateSeq :: (Monoid a, IsString a, Foldable f) => f a -> a
enumerateAlt = enumerateWith ", " " or "
enumerateSeq = enumerateWith ", " " and "


-------------------
-- === Delta === --
-------------------

newtype Delta = Delta Word64 deriving (Generic, Show, Num, Ord, Eq, Enum)
makeLenses ''Delta

instance Convertible' a Word64 => Convertible a Delta where convert = wrap . convert'
instance Convertible' Word64 a => Convertible Delta a where convert = convert' . unwrap
instance NFData    Delta
instance Default   Delta where def    = 0
instance Mempty    Delta where mempty = def
instance Semigroup Delta where (<>)   = (+)



----------------------
-- === Builders === --
----------------------

-- === Definition === --

class ElemBuilderT t m a where plainT :: m a -> t m a
class ElemBuilder    m a where plain  ::   a ->   m a

instance {-# OVERLAPPABLE #-} (ElemBuilder m a, ElemBuilderT t m a)
      => ElemBuilder (t m)    a where plain = plainT . plain
instance ElemBuilder Identity a where plain = pure


-- === Rendering === --

class RenderT t m a where renderT :: t m a -> m a
class Render    m a where render  ::   m a ->   a
                          nested  ::   m a -> m a

instance {-# OVERLAPPABLE #-} (RenderT t m a, Render m a, ElemBuilderT t m a)
      => Render (t m)    a where render = render . renderT
                                 nested = plainT . renderT
instance Render Identity a where render = runIdentity
                                 nested = id


-- === Utils === --

phantom :: (ElemBuilder t a, Mempty a) => t a
phantom = plain mempty



-----------------------------
-- === Bounded objects === --
-----------------------------

-- === Definition === --

data Bounds = Bounds { _width  :: !Delta
                     , _height :: !Delta
                     } deriving (Show)

data Bounded a = Bounded { __bounds :: Bounds
                         , __elem   :: a
                         } deriving (Show, Functor, Traversable, Foldable)

makeClassy ''Bounds
makeLenses ''Bounded


-- === Measurable === --

class Measurable a where
    measure :: a -> Bounds

instance Measurable (Bounded a) where
    measure = view bounds


-- === Utils === --

bounded :: Lens (Bounded a) (Bounded b) a b
bounded = bounded_elem

unbound :: Bounded a -> a
unbound = view bounded


-- === Intances === --

instance Mempty a => Mempty (Bounded a) where
    mempty = Bounded mempty mempty

instance Mempty Bounds where mempty = Bounds mempty mempty
instance HasBounds (Bounded a) where bounds = bounded_bounds

instance Concatenable Bounds where
    concat t (Bounds w h) (Bounds w' h') = case t of
        Horizontal -> Bounds (w <> w') (max h h')
        Vertical   -> Bounds (max w w') (h <> h')



-----------------------------
-- === Cartesian space === --
-----------------------------

-- === Definition == --

data Dir = Vertical
         | Horizontal
         deriving (Show)

data CartTree t m a = Empty
                    | Plain  !(m a)
                    | Concat !Dir !(t m a) !(t m a)
                    deriving (Show, Functor, Traversable, Foldable)


-- === Concatenation === --

class Concatenable a where
    concat :: Dir -> a -> a -> a

instance Concatenable a => Concatenable (Identity a) where
    concat d l r = Identity $ concat d (runIdentity l) (runIdentity r)


-- === Utils === --

hcat, vcat :: Concatenable a => a -> a -> a
hcat = concat Horizontal
vcat = concat Vertical

infixr 6 </>
(</>)  :: Concatenable a => a -> a -> a
(</>) = vcat


-- === Instances === --

instance Mempty (CartTree t m a) where
    mempty = Empty

instance Convertible2' (CartTree t) t => Semigroup (CartTree t m a) where
    Empty <> a = a
    a <> Empty = a
    a <> b = Concat Horizontal (convert2' a) (convert2' b)

instance Convertible2' (CartTree t) t => Concatenable (CartTree t m a) where
    concat d a b = Concat d (convert2' a) (convert2' b)

instance Convertible2' (CartTree t) t => P.Monoid (CartTree t m a) where
    mempty  = mempty
    mappend = (<>)



---------------------
-- === Spacing === --
---------------------

-- === Definition === --

class Spacing a where
    spacing :: Bounds -> a


-- === Utils === --

hspacing, vspacing :: (ElemBuilder t a, Spacing a) => Delta -> t a
hspacing = plain . spacing . flip Bounds 1
vspacing = plain . spacing .      Bounds 1

infixr 6 <+>
(<+>) :: (ElemBuilder t a, Semigroup (t a), Spacing a) => t a -> t a -> t a
(<+>) = mappendWith $ hspacing 1

infixr 6 <//>
infixr 6 <///>
infixr 6 <////>
(<//>), (<///>), (<////>) :: (Concatenable (t a), ElemBuilder t a, Spacing a) => t a -> t a -> t a
a <//>   b = vcat a (vcat (vspacing 1) b)
a <///>  b = vcat a (vcat (vspacing 2) b)
a <////> b = vcat a (vcat (vspacing 3) b)


-----------------------
-- === LineBlock === --
-----------------------

-- === Definition === --

data LineBlock a = LineBlock { __bounds :: Bounds, _lines :: [a] } deriving (Show, Functor, Foldable, Traversable)
makeLenses ''LineBlock


-- === Running === --

renderLineBlock :: (IsString a, Monoid a, Item a ~ Char, FiniteSequence a) => LineBlock a -> a
renderLineBlock = intercalate "\n" . fmap stripEnd . view lines

concatLineBlock :: (IsString a, Monoid a) => LineBlock a -> a
concatLineBlock = intercalate "\n" . view lines


-- === Instances === --

-- Measurements
instance HasBounds  (LineBlock a) where bounds = lineBlock_bounds
instance Measurable (LineBlock a) where measure = view bounds

-- Monoids
instance                            Mempty    (LineBlock a) where mempty = LineBlock mempty mempty
instance GenLineBlockConcatCtx a => Semigroup (LineBlock a) where (<>)   = concat Horizontal
instance GenLineBlockConcatCtx a => P.Monoid  (LineBlock a) where
    mempty  = mempty
    mappend = (<>)

-- Concatenation
type GenLineBlockConcatCtx a = (Convertible String a, Monoid a)
instance GenLineBlockConcatCtx a => Concatenable (LineBlock a) where
    concat d l r = LineBlock nbs newtb where
        lbs   = measure l
        rbs   = measure r
        maxw  = max (lbs ^. width)  (rbs ^. width)
        maxh  = max (lbs ^. height) (rbs ^. height)
        relw  = width  %~ (maxw -)
        relh  = height %~ (maxh -)
        eqw t = zipWith (<>) (t ^. lines) (spacing (relw $ t ^. bounds) ^. lines)
        eqh t = t ^. lines <> spacing (relh $ t ^. bounds) ^. lines
        nbs   = concat d lbs rbs
        newtb = case d of
            Horizontal -> zipWith (<>) (eqh l) (eqh r)
            Vertical   -> eqw l <> eqw r

-- Conversions
instance (IsString a, Measurable a) => IsString (LineBlock a) where
    fromString s = LineBlock (measure a) $ pure a where a = fromString s

instance (Convertible' Text a, Measurable a) => Convertible Text (LineBlock a) where
    convert s = LineBlock (measure a) $ pure a where a = convert' s


-- Spacing
instance (Convertible String a, Mempty a) => Spacing (LineBlock a) where
    spacing b@(Bounds w h) = LineBlock b lines where
        lines = replicate h $ convert (replicate w ' ')

-- Conversions




--------------------------
-- === BlockBuilder === --
--------------------------

-- === Definition === --

type    BlockBuilder      = BlockBuilderT Identity
newtype BlockBuilderT m a = BlockBuilderT (CartTree BlockBuilderT m a) deriving (Show, Functor, Traversable, Foldable, Mempty, Semigroup, P.Monoid, Concatenable)


-- === Running === --

instance ElemBuilderT BlockBuilderT m a where
    plainT = BlockBuilderT . Plain

instance (Mempty (m a), Concatenable (m a)) => RenderT BlockBuilderT m a where
    renderT (BlockBuilderT t) = case t of
        Plain  a     -> a
        Empty        -> mempty
        Concat d l r -> concat d (renderT l) (renderT r)


-- -- === BlockBuilder modification === --

append :: BlockBuilder a -> BlockBuilder a -> BlockBuilder a
append a block@(BlockBuilderT b) = case b of
    Concat d l r -> BlockBuilderT $ Concat d l (append a r)
    _            -> block <> a

prepend :: BlockBuilder a -> BlockBuilder a -> BlockBuilder a
prepend a block@(BlockBuilderT b) = case b of
    Concat d l r -> BlockBuilderT $ Concat d (prepend a l) r
    _            -> a <> block


-- === Instances === --

-- Conversions
instance Convertible2 (CartTree BlockBuilderT) BlockBuilderT where convert2 = wrap
instance (ElemBuilder m a, IsString a)          => IsString           (BlockBuilderT m a) where fromString = plain . fromString
instance (ElemBuilder m a, Convertible' Text a) => Convertible Text   (BlockBuilderT m a) where convert    = plain . convert'
instance (ElemBuilder m a, Convertible' Text a) => Convertible String (BlockBuilderT m a) where convert    = convertVia @Text
instance (ElemBuilder m a, Convertible' Text a) => Convertible Char   (BlockBuilderT m a) where convert    = convertVia @String

-- Lenses
makeLenses ''BlockBuilderT



-------------------------
-- === LineBuilder === --
-------------------------

-- === Definition === --

type    LineBuilder      = LineBuilderT Identity
newtype LineBuilderT m a = LineBuilderT (CartTree LineBuilderT m a) deriving (Show, Functor, Traversable, Foldable, Mempty, Semigroup, P.Monoid, Concatenable)


-- === Running === --

instance ElemBuilderT LineBuilderT m a where
    plainT = LineBuilderT . Plain

instance (Concatenable (m a), Monoid (m a)) => RenderT LineBuilderT m a where
    renderT bb = foldl (</>) mempty $ (mconcat <$> lines) where
        lines  = reverse $ reverse <$> rlines
        rlines = uncurry (flip (:)) $ rndr mempty mempty bb
        rndr :: [[m a]] -> [m a] -> LineBuilderT m a -> ([[m a]], [m a])
        rndr lines line (LineBuilderT t) = case t of
            Empty        -> (lines, line)
            Plain  a     -> (lines, a:line)
            Concat d l r -> case d of
                Horizontal -> uncurry rndr (rndr lines line l) r
                Vertical   -> rndr (uncurry (flip (:)) (rndr lines line l)) mempty r


-- === Utils === --

block :: (Render t a, Concatenable (t a), ElemBuilder t a, Mempty a) => t a -> t a
block t = nested t </> phantom

indented :: (ElemBuilder t a, Spacing a, Semigroup (t a)) => t a -> t a
indented t = hspacing 4 <> t


-- === Instances === --

-- Conversions

instance Convertible2 (CartTree LineBuilderT) LineBuilderT where convert2 = wrap
instance (ElemBuilder m a, IsString a)          => IsString           (LineBuilderT m a) where fromString = plain . fromString
instance (ElemBuilder m a, Convertible' Text a) => Convertible Text   (LineBuilderT m a) where convert    = plain . convert'
instance (ElemBuilder m a, Convertible' Text a) => Convertible String (LineBuilderT m a) where convert    = convertVia @Text
instance (ElemBuilder m a, Convertible' Text a) => Convertible Char   (LineBuilderT m a) where convert    = convertVia @String

-- Lenses
makeLenses ''LineBuilderT



-----------------
-- === Doc === --
-----------------

-- | The `Doc` type is just an alias to predefined layouting transformers. It is unified type allowing many fancy utils, like inserting indented code blocks.
type Doc a = LineBuilderT BlockBuilder (LineBlock a)



-- FIXME [WD]: we might need to refactor it out somewhere
-----------------------------
-- === Basic renderers === --
-----------------------------

-- === Text rendering === --

instance Measurable Text where
    measure t = Bounds (convert $ Text.length t) 1

instance Stylable a => Stylable (LineBlock a)
instance (Stylable a, Functor m) => Stylable (BlockBuilderT m a)
instance (Stylable a, Functor m) => Stylable (LineBuilderT  m a)

instance Measurable TermText where
    measure t = Bounds (convert $ length t) 1





-- main :: IO ()
-- main = do
--     let b1  = "foo1" :: Doc TermText
--         b2  = b1 </> styled [fg $ dark green] (indented (block $ b1 </> b1)) <> b1
--         out = render $ nested b2 <> nested b2 </> nested b2 <> nested b2
--     putStrLn $ concatLineBlock out
--     print "end"
