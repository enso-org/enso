{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Luna.System.Pragma
-- Copyright   :  (C) 2016 New Byte Order
-- License     :  AllRightsReserved
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Usage:
--
-- Pragmas can be defined as standalone data types and can be registered
-- in PragmaMap's. Each PragmaMap contains PragmaStack, which handles
-- such operations as push or pop for pragmas.
--
--
-- @
-- data ImplicitSelf = ImplicitSelf deriving (Show, Read, Typeable)
-- data TabSize      = TabSize Int  deriving (Show, Read, Typeable)
--
-- instance IsPragma ImplicitSelf
-- instance IsPragma TabSize
--
--
-- implicitSelf = pragma                :: SwitchPragma ImplicitSelf
-- tabSize      = pragmaDef (TabSize 4) :: ValPragma    TabSize
--
-- handleErr = \case
--     Right a -> return a
--     Left  e -> fail $ show e
--
-- main = do
--     ps <- return (def :: PragmaMap)
--     ps <- handleErr $ register implicitSelf ps
--     ps <- handleErr $ register tabSize ps
--
--     ps <- handleErr $ push tabSize (TabSize 8) ps
--
--     print $ lookup implicitSelf ps
--     print $ lookup tabSize ps
--
--     ps <- handleErr $ enable implicitSelf ps
--     ps <- handleErr $ pop_ tabSize ps
--
--     print $ lookup implicitSelf ps
--     print $ lookup tabSize ps
--     print $ pragmaNames ps
-- @
--
-- outputs:
--
-- @
-- Left  (Error LookupUndefined "Switch")
-- Right (Pragma { _name = "Val"   , _defVal = Just (TabSize 4), _val = TabSize 8 })
-- Right (Pragma { _name = "Switch", _defVal = Nothing         , _val = Enabled   })
-- Right (Pragma { _name = "Val"   , _defVal = Just (TabSize 4), _val = TabSize 4 })
-- ["TabSize","ImplicitSelf"]
-- @
----------------------------------------------------------------------------

module Luna.Extension.Pragma where

import           Prelude.Luna           as P hiding (noneOf, lookup)
import           Luna.Syntax.Name
import           Data.Map               (Map)
import           Data.Container.Hetero  (Hetero)

----------------------------------------------------------------------
-- Lookup & status
----------------------------------------------------------------------

data Error t = Error t Text deriving (Show)

data Unregistered    = Unregistered deriving (Show)
data Occupied        = Occupied     deriving (Show)
data LookupErrorType = LookupUnregistered
                     | LookupUndefined
                     deriving (Show)

type AccessError   = Error Unregistered
type LookupError   = Error LookupErrorType
type RegisterError = Error Occupied

--class Possibly t a where


--class AccessClass a where
--    unregistered :: a

--instance AccessClass Unregistered    where unregistered = Unregistered
--instance AccessClass LookupErrorType where unregistered = LookupUnregistered

----------------------------------------------------------------------
-- PragmaDef
----------------------------------------------------------------------

type family Value t a :: *

newtype PragmaVal   t a = PragmaVal               (Value t a)
data    Pragma      t a = Pragma      Name        (Value t a)
data    PragmaDef   t a = PragmaDef   Name (Maybe (Value t a))
data    PragmaStack t a = PragmaStack Name        [Value t a]

deriving instance Show (Value t a) => Show (Pragma    t a)
deriving instance Show (Value t a) => Show (PragmaDef t a)

instance HasName (Pragma      t a) where name = lens (\(Pragma      n _) -> n) (\(Pragma      _ v) n -> Pragma      n v)
instance HasName (PragmaDef   t a) where name = lens (\(PragmaDef   n _) -> n) (\(PragmaDef   _ v) n -> PragmaDef   n v)
instance HasName (PragmaStack t a) where name = lens (\(PragmaStack n _) -> n) (\(PragmaStack _ v) n -> PragmaStack n v)

--data Pragma t a v = Pragma { _name    :: Text
--                           , _default :: Maybe (Value t a)
--                           , _value   :: v
--                           } deriving (Functor)

--deriving instance (Show (PragmaVal t a), Show v) => Show (Pragma t a v)

--makeLenses ''Pragma

--type PragmaInst  t a = Pragma t a (PragmaVal t a)
--type PragmaDef   t a = Pragma t a ()
--type PragmaStack t a = Pragma t a [PragmaVal t a]

--defOf :: Pragma t a v -> PragmaDef t a
--defOf = fmap $ const ()

---- == Type classes ==

--class IsPragma a where
--    parse :: (TokenParsing m, CharParsing m, Monad m) => PragmaDef t a -> m a
--    default parse :: (TokenParsing m, CharParsing m, Monad m, Read a) => PragmaDef t a -> m a
--    parse p = read . toString . (\s -> p^.name <> " " <> fromString s) <$> many (noneOf "\n\r")

---- Tells what should happen when defPragma is succesfully parsed
--class PragmaCons t where
--    pragmaCons :: PragmaDef t a -> a -> PragmaVal t a

---- == Utils ==

--baseTName :: Typeable a => PragmaDef t a -> String
--baseTName (_::PragmaDef t a) = show $ typeOf (undefined :: a)

--pragma :: Typeable a => PragmaDef t a
--pragma = def

--pragmaDef :: Typeable a => PragmaVal t a -> PragmaDef t a
--pragmaDef (a :: PragmaVal t a) = (def :: PragmaDef t a) & defVal .~ Just a :: PragmaDef t a


---- == Instances ==

--instance out ~ PragmaStack t a => HMap.IsKey (Pragma t a v) Text out
--    where toKey = HMap.Key . view name

--instance (Typeable a, Default val) => Default (Pragma t a val) where
--    def = Pragma (fromString . show $ typeOf (undefined :: a)) def def

------------------------------------------------------------------------
---- Type hidding
------------------------------------------------------------------------

data HPragmaValStack where
    HPragmaValStack :: [PragmaVal t a] -> HPragmaValStack


--instance (v~PragmaVal t a, IsPragma a, PragmaCons t) => HideType (Pragma t a [v]) HPragma where
--    hideType                = HPragma
--    revealType (HPragma p) = unsafeCoerce p


--runHPragma :: (TokenParsing m, CharParsing m, Monad m) => HPragma -> m HPragma
--runHPragma (HPragma a) = fmap HPragma (runPragmaStack a)
--    where runPragmaStack :: (TokenParsing m, CharParsing m, Monad m, IsPragma a, PragmaCons t) => PragmaStack t a -> m (PragmaStack t a)
--          runPragmaStack p@(Pragma n d as) = (\a -> Pragma n d [a]) . pragmaCons (defOf p) <$> parse (defOf p)



------------------------------------------------------------------------
---- PragmaMap
------------------------------------------------------------------------

newtype PragmaMap = PragmaMap { _values :: Hetero (Map Name HPragmaValStack) } deriving (Monoid)
makeClassy ''PragmaMap


--class MonadPragmaMap m where
--    getPragmaMap :: m PragmaMap
--    putPragmaMap :: PragmaMap -> m ()

---- == Instances ==

--instance Show PragmaMap where
--    show ps = "PragmaMap " <> show (pragmaNames ps)

--instance Default PragmaMap where
--    def = PragmaMap mempty

---- == Utils ==

--parseByName :: (TokenParsing m, CharParsing m, Monad m) => HasPragmaMap s => Text -> s -> Either AccessError (m s)
--parseByName k ps = case lookupByName k ps of
--    Just psable -> Right $ fmap (\a -> P.set lens (H a) ps) (Maps.insert k <$> (runHPragma psable) <*> pure base)
--    Nothing     -> Left  $ Error unregistered k
--    where lens   = pragmaMap.values
--          H base = view lens ps

--lookupByName :: HasPragmaMap s => Text -> s -> Maybe HPragma
--lookupByName k = Maps.lookup k . unH . view (pragmaMap . values)

--pragmaNames :: HasPragmaMap s => s -> [Text]
--pragmaNames = HMap.baseKeys . view (pragmaMap . values)

----

--type PragmaCtx t a s = (HasPragmaMap s, IsPragma a, PragmaCons t)

--withRegistered :: (PragmaCtx t a s, AccessClass e) => PragmaDef t a -> s -> (PragmaStack t a -> x) -> Either (Error e) x
--withRegistered p ps f = case lookupStack p ps of
--    Nothing -> Left  $ Error unregistered (p^.name)
--    Just s  -> Right $ f s

--lookupStack :: PragmaCtx t a s => PragmaDef t a -> s -> Maybe (PragmaStack t a)
--lookupStack p = HMap.lookup p . view (pragmaMap.values)

--set :: PragmaCtx t a s => PragmaDef t a -> PragmaVal t a -> s -> Either AccessError s
--set p@(Pragma n d _) a ps = withRegistered p ps . const $ setStack p (Pragma n d [a]) ps

--push :: PragmaCtx t a s => PragmaDef t a -> PragmaVal t a -> s -> Either AccessError s
--push p a ps = withRegistered p ps $ \(Pragma n d s) -> setStack p (Pragma n d (a:s)) ps

--pop :: PragmaCtx t a s => PragmaDef t a -> s -> Either LookupError (PragmaInst t a, s)
--pop p ps = join $ withRegistered p ps $ \(Pragma n d s) -> case s of
--    []   -> Left  $ Error LookupUndefined (p^.name)
--    x:xs -> Right $ (Pragma n d x, setStack p (Pragma n d xs) ps)

--pop_ :: PragmaCtx t a s => PragmaDef t a -> s -> Either LookupError s
--pop_ = fmap snd .: pop

--setStack :: PragmaCtx t a s => PragmaDef t a -> PragmaStack t a -> s -> s
--setStack p a = pragmaMap.values %~ HMap.insert p a

--lookup :: PragmaCtx t a s => PragmaDef t a -> s -> Either LookupError (PragmaInst t a)
--lookup p@(Pragma n d _) ps = case fmap fst $ pop p ps of
--    Right a             -> Right a
--    Left  e@(Error t _) -> case t of
--        LookupUnregistered -> Left e
--        LookupUndefined    -> case p^.defVal of
--            Nothing -> Left e
--            Just a  -> Right (Pragma n d a)

--register :: PragmaCtx t a s => PragmaDef t a -> s -> Either RegisterError s
--register p@(Pragma n d _) s = case lookup p s of
--    Right _ -> Left  $ Error Occupied (p^.name)
--    Left  _ -> Right $ setStack p (Pragma n d mempty) s


------------------------------------------------------------------------
---- SwitchPragma
------------------------------------------------------------------------

data Switch         = Enabled | Disabled deriving (Show, Typeable, Eq, Ord)
--type SwitchPragma a = PragmaDef Switch a

type instance Value Switch a = Switch


---- == Utils ==

--enable :: (HasPragmaMap s, IsPragma a) => SwitchPragma a -> s -> Either AccessError s
--enable p = push p Enabled

--disable :: (HasPragmaMap s, IsPragma a) => SwitchPragma a -> s -> Either AccessError s
--disable p = push p Disabled

--isEnabled :: Pragma t a Switch -> Bool
--isEnabled = (==Enabled) . view val

---- == Instances ==

--instance PragmaCons Switch where
--    pragmaCons _ = const Enabled



------------------------------------------------------------------------
---- ValPragma
------------------------------------------------------------------------

data Val = Val deriving (Show, Typeable, Eq, Ord)
--type ValPragma a = PragmaDef Val a
type instance Value Val a = a

---- == Instances ==

--instance PragmaCons Val where
--    pragmaCons _ = id
