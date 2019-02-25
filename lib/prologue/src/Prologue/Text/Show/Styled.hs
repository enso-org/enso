{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}

module Prologue.Text.Show.Styled where

import Prelude hiding (Monoid)

import qualified Data.Text.IO     as Text
import qualified Text.Show.Pretty as Formatted
import qualified Text.PrettyPrint as Formatted

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text)
import Data.Convert
import Data.Monoids           (Monoid, intercalate)
import Data.String            (IsString)


-- TODO 
-- We should replace Text with some TextBuilder + Cache, which will
-- automatically replace Text.Builders with new one after Text chunks 
-- concatenation.

-------------------
-- === Class === --
-------------------

-- === Definition === --

type family StyledShowOutput style

class StyledShow style a where 
    styledShow :: style -> a -> StyledShowOutput style

class StyledShow1 style a where
    styledShow1 :: ∀ t1. style -> a t1 -> StyledShowOutput style

class StyledShow2 style a where
    styledShow2 :: ∀ t1 t2. style -> a t1 t2 -> StyledShowOutput style


-- === Redirect instances === --

instance {-# OVERLAPPABLE #-} StyledShow1 style a 
    => StyledShow style (a t) where
    styledShow = styledShow1 ; {-# INLINE styledShow #-}

instance {-# OVERLAPPABLE #-} StyledShow2 style a 
    => StyledShow1 style (a t) where
    styledShow1 = styledShow2 ; {-# INLINE styledShow1 #-}



--------------------------------
-- === Monadic StyledShow === --
--------------------------------

-- === Definition === --

class StyledShowM style a m where
    styledShowM :: style -> a -> m (StyledShowOutput style)
    
class StyledShowM1 style a m where
    styledShowM1 :: ∀ t1. style -> a t1 -> m (StyledShowOutput style)
    
class StyledShowM2 style a m where
    styledShowM2 :: ∀ t1 t2. style -> a t1 t2 -> m (StyledShowOutput style)

    
-- === Redirect instances === --

instance {-# OVERLAPPABLE #-} StyledShowM1 style a m 
    => StyledShowM style (a t) m where
    styledShowM = styledShowM1 ; {-# INLINE styledShowM #-}

instance {-# OVERLAPPABLE #-} StyledShowM2 style a m 
    => StyledShowM1 style (a t) m where
    styledShowM1 = styledShowM2 ; {-# INLINE styledShowM1 #-}




-----------------------
-- === Formatted === --
-----------------------

-- TODO
-- We should change the whole printing API to use Text instead

format__ :: Text -> Text
format__ txt = convert . show $ case Formatted.parseValue s of
    Just v  -> Formatted.valToDoc v
    Nothing -> Formatted.text s
    where s = convert txt
{-# INLINE format__ #-}

putLnFmtd :: MonadIO m => Text -> m ()
putLnFmtd = liftIO . Text.putStrLn . format__



-----------------------------
-- === StructShowStyle === --
-----------------------------

-- === Definition === --

data StructShowStyle = StructShowStyle deriving (Show)
type instance StyledShowOutput StructShowStyle = Text

type StructShow  = StyledShow  StructShowStyle
type StructShow1 = StyledShow1 StructShowStyle
type StructShow2 = StyledShow2 StructShowStyle

type StructShowM  = StyledShowM  StructShowStyle
type StructShowM1 = StyledShowM1 StructShowStyle
type StructShowM2 = StyledShowM2 StructShowStyle

-- TODO
-- We should rename all 'structShowX' functions to just 'showX' and replace all
-- usages of 'show' with it, because using Strings is not good here.
structShow  :: StructShow  a => a       -> Text
structShow1 :: StructShow1 a => a t1    -> Text
structShow2 :: StructShow2 a => a t1 t2 -> Text
structShow  = styledShow  StructShowStyle ; {-# INLINE structShow  #-}
structShow1 = styledShow1 StructShowStyle ; {-# INLINE structShow1 #-}
structShow2 = styledShow2 StructShowStyle ; {-# INLINE structShow2 #-}

showM  :: StructShowM  a m => a       -> m Text
showM1 :: StructShowM1 a m => a t1    -> m Text
showM2 :: StructShowM2 a m => a t1 t2 -> m Text
showM  = styledShowM  StructShowStyle ; {-# INLINE showM  #-}
showM1 = styledShowM1 StructShowStyle ; {-# INLINE showM1 #-}
showM2 = styledShowM2 StructShowStyle ; {-# INLINE showM2 #-}


-- === Instances === --

instance 
    ( out ~ StyledShowOutput style
    , Monad m
    , Monoid out
    , IsString out
    , StyledShowM style a m
    ) => StyledShowM style [a] m where
    styledShowM style a = lstfmt <$> mapM (styledShowM style) a where
        lstfmt = braced . intercalate ","
        braced = \a -> "[" <> a <> "]"


        
-----------------------------
-- === PrettyShowStyle === --
-----------------------------

-- === Definition === --

data PrettyShowStyle = PrettyShowStyle deriving (Show)
type instance StyledShowOutput PrettyShowStyle = Text

type PrettyShow  = StyledShow  PrettyShowStyle
type PrettyShow1 = StyledShow1 PrettyShowStyle
type PrettyShow2 = StyledShow2 PrettyShowStyle

type PrettyShowM  = StyledShowM  PrettyShowStyle
type PrettyShowM1 = StyledShowM1 PrettyShowStyle
type PrettyShowM2 = StyledShowM2 PrettyShowStyle

prettyShow  :: PrettyShow  a => a       -> Text
prettyShow1 :: PrettyShow1 a => a t1    -> Text
prettyShow2 :: PrettyShow2 a => a t1 t2 -> Text
prettyShow  = styledShow  PrettyShowStyle ; {-# INLINE prettyShow  #-}
prettyShow1 = styledShow1 PrettyShowStyle ; {-# INLINE prettyShow1 #-}
prettyShow2 = styledShow2 PrettyShowStyle ; {-# INLINE prettyShow2 #-}

prettyShowM  :: PrettyShow  a => a       -> Text
prettyShowM1 :: PrettyShow1 a => a t1    -> Text
prettyShowM2 :: PrettyShow2 a => a t1 t2 -> Text
prettyShowM  = styledShow  PrettyShowStyle ; {-# INLINE prettyShowM  #-}
prettyShowM1 = styledShow1 PrettyShowStyle ; {-# INLINE prettyShowM1 #-}
prettyShowM2 = styledShow2 PrettyShowStyle ; {-# INLINE prettyShowM2 #-}

