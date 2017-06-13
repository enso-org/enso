{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE PatternSynonyms           #-}

module Data.Layout where

import qualified Prologue as P
import Prologue hiding ((:>), Empty, Bounded, div, simple, concat, putStr, swapped, length, putStrLn, take, drop)

import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import qualified Data.Text.Lazy            as LazyText
import qualified Data.Text.Lazy.Builder    as Text
import           Data.Text.Terminal


instance Default a => Default (NonEmpty a) where def    = def    :| mempty
instance Mempty  a => Mempty  (NonEmpty a) where mempty = mempty :| mempty



-------------------
-- === Delta === --
-------------------

newtype Delta = Delta Word64 deriving (Show, Num, Ord, Eq, Enum)
makeLenses ''Delta

instance Convertible' a Word64 => Convertible a Delta where convert = wrap . convert'
instance Convertible' Word64 a => Convertible Delta a where convert = convert' . unwrap
instance Default   Delta where def    = 0
instance Mempty    Delta where mempty = def
instance Semigroup Delta where (<>)   = (+)



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


-- === Concatenation === --

class Concatenable a where
    concat :: Dir -> a -> a -> a

data Dir = Vertical
         | Horizontal
         deriving (Show)


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



-------------------
-- === Block === --
-------------------

-- === Definition === --

data Block a = Block { __bounds   :: Bounds
                     , _blockBody :: BlockBody a
                     } deriving (Show, Functor, Traversable, Foldable)

data BlockBody a = Phantom
                 | Plain  !a
                 | Concat !Dir !(Block a) !(Block a)
                 deriving (Show, Functor, Traversable, Foldable)

makeLenses ''Block


-- === Rendering === --

class ElemRenderer a out where
    renderElem    :: Bounds -> a -> out
    renderPhantom :: Bounds      -> out

render :: forall a b. (ElemRenderer a b, Concatenable b, Mempty b) => Block a -> b
render (Block s t) = case t of
    Phantom      -> renderPhantom @a s
    Plain  a     -> renderElem s a
    Concat d l r -> concat d (render l) (render r)

-- | Renders anything that is similar to Text by rendering line by line. It is useful when implementic horizontal and vertical block concatenation.
textRender :: forall a. (ElemRenderer a (Bounded [a]), Concatenable (Bounded [a]), IsString a, Monoid a) => Block a -> a
textRender block = mconcat (intersperse "\n" lineList) where
    Bounded _ lineList = render block :: Bounded [a]


-- === General utils === --

betweenWith :: (a -> a -> a) -> a -> a -> a -> a
betweenWith f l r m = l `f` m `f` r

surroundedWith :: (a -> a -> a) -> a -> a -> a -> a
surroundedWith f m l r = betweenWith f l r m

between :: Semigroup a => a -> a -> a -> a
between = betweenWith (<>)

between' :: Semigroup a => a -> a -> a
between' a = between a a


-- === General combinators === --

plain2 :: Measurable a => a -> Block a
plain2 a = Block (measure a) (Plain a)

append :: Block a -> Block a -> Block a
append a block@(Block s b) = case b of
    Concat d l r -> Block s $ Concat d l (append a r)
    _            -> block <> a

prepend :: Block a -> Block a -> Block a
prepend a block@(Block s b) = case b of
    Concat d l r -> Block s $ Concat d (prepend a l) r
    _            -> a <> block

text :: Text -> Block Text
text = plain2

hcat, vcat :: Concatenable a => a -> a -> a
hcat = concat Horizontal
vcat = concat Vertical

hspacing, vspacing :: Delta -> Block a
hspacing d = Block (Bounds d 1) Phantom
vspacing d = Block (Bounds 1 d) Phantom

indented :: Block a -> Block a
indented a = hspacing 4 <> a

infixr 5 </>
(</>) :: Concatenable a => a -> a -> a
(</>) = vcat

infixr 5 <+>
(<+>) :: Block a -> Block a -> Block a
(<+>) = mappendWith $ hspacing 1


-- === Text combinators === --

space :: IsString a => a
space = " "

parensed, bracked, braced, chevroned, spaced, quoted, squoted, backticked :: (Semigroup a, IsString a) => a -> a
parensed   = between "(" ")"
bracked    = between "[" "]"
braced     = between "{" "}"
chevroned  = between "<" ">"
spaced     = between' " "
quoted     = between' "\""
squoted    = between' "'"
backticked = between' "`"


-- === Instances === --

instance Mempty    (Block a) where mempty = Block mempty Phantom
instance Semigroup (Block a) where
    Block (Bounds 0 0) Phantom <> a = a
    a <> Block (Bounds 0 0) Phantom = a
    a <> b = concat Horizontal a b

instance Measurable   (Block a) where measure = view bounds
instance HasBounds    (Block a) where bounds  = block_bounds
instance Concatenable (Block a) where
    concat d a b = Block bounds $ Concat d a b where
        bounds = concat d (measure a) (measure b)

instance (Measurable a, IsString a)          => IsString           (Block a) where fromString = plain2 . fromString
instance (Measurable a, Convertible' Text a) => Convertible Text   (Block a) where convert    = plain2 . convert'
instance (Measurable a, Convertible' Text a) => Convertible String (Block a) where convert    = convertVia @Text
instance (Measurable a, Convertible' Text a) => Convertible Char   (Block a) where convert    = convertVia @String



-----------------------------
-- === Basic renderers === --
-----------------------------

-- === Text rendering === --

instance Measurable Text where
    measure t = Bounds (convert $ Text.length t) 1

instance ElemRenderer Text (Bounded [Text]) where
    renderPhantom b@(Bounds w h)   = Bounded b . replicate h . convert $ replicate w ' '
    renderElem    b@(Bounds w h) t = Bounded b . pure $ if
        | tlen > w -> Text.take (unsafeConvert $ unwrap w) t
        | tlen < w  -> t <> convert (replicate (w - tlen) ' ')
        | otherwise -> t
        where tlen = convert $ Text.length t

instance Concatenable (Bounded [Text]) where
    concat d (Bounded (Bounds w h) b) (Bounded (Bounds w' h') b') = newtb where
        unih    = max h h'
        uniw    = max w w'
        eqh w b = b <> replicate (unih - convert (P.length b)) (convert $ replicate w ' ')
        eqw b   = fmap (\t -> t <> convert (replicate (uniw - convert (Text.length t)) ' ')) b
        newtb = case d of
            Horizontal -> Bounded (Bounds (w <> w') unih) $ zipWith (<>) (eqh w b) (eqh w' b')
            Vertical   -> Bounded (Bounds uniw (h <> h')) $ eqw b <> eqw b'


-- === TermText rendering === --
-- FIXME[WD]: The implementation for TermText rendering is VERY similar to Text rendering. We need some wise refactor here.

instance Stylable a => Stylable (Block a) where
    cleanStyles = fmap cleanStyles
    withStyle   = fmap . withStyle

instance Measurable TermText where
    measure t = Bounds (convert $ length t) 1

instance ElemRenderer TermText (Bounded [TermText]) where
    renderPhantom b@(Bounds w h)   = Bounded b . replicate h . convert $ replicate w ' '
    renderElem    b@(Bounds w h) t = Bounded b . pure $ if
        | tlen > w -> take (convert w) t
        | tlen < w  -> t <> convert (replicate (w - tlen) ' ')
        | otherwise -> t
        where tlen = convert $ length t

instance Concatenable (Bounded [TermText]) where
    concat d (Bounded (Bounds w h) b) (Bounded (Bounds w' h') b') = newtb where
        unih    = max h h'
        uniw    = max w w'
        eqh w b = b <> replicate (unih - convert (P.length b)) (convert $ replicate w ' ')
        eqw b   = fmap (\t -> t <> convert (replicate (uniw - convert (length t)) ' ')) b
        newtb = case d of
            Horizontal -> Bounded (Bounds (w <> w') unih) $ zipWith (<>) (eqh w b) (eqh w' b')
            Vertical   -> Bounded (Bounds uniw (h <> h')) $ eqw b <> eqw b'



-- === Examples
--
-- main :: IO ()
-- main = do
--     let b1  = "foo" :: Block TermText
--         b2  = "foo" </> vspacing 1 </> "a" :: Block TermText
--         b'  = b1 <+> b2
--         out = textRender (b' <> styled [fg $ dark green, blinking] b' <> b')
--     putStrLn out
--     main2
--     print "end"
--
--
-- main2 :: IO ()
-- main2 = do
--     let t1 = "hello" :: TermText
--         tx = withStyle [fg $ dark green] $ t1 <> styled [fg $ dark red, italic, blinking] t1 <> t1
--     putStrLn tx
--     -- print tx
--     putStrLn ""
