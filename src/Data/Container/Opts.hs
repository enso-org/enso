{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# LANGUAGE PolyKinds                 #-}


module Data.Container.Opts where

import Prelude
import Type.Bool
import Data.Typeable


------------------
-- === Opts === --
------------------

type family ParamsOf op cont :: [*]
type family ModsOf   op cont :: [*]


data Opt a = P a -- Provided
           | N   -- Not provided
           deriving (Show)

-- Knowledge

data Knowledge a = Known a 
                 | Unknown
                 deriving (Show)

-- Mods

data Ixed      = Ixed

-- Parameters

data Safe      = Safe
data Unchecked = Unchecked
data Unsafe    = Unsafe
data Inplace   = Inplace

-- Formatters

data Try       = Try
data Raw       = Raw




-------------------------
type family MatchOpts (provided :: [*]) (selected :: [*]) :: [Opt *] where
    MatchOpts (p ': ps) sel = (p `CheckIfKnown` sel) ': MatchOpts ps sel 
    MatchOpts '[]       sel = '[]


type family CheckIfKnown flag flags :: Opt * where
    CheckIfKnown f (f  ': fs) = P f 
    CheckIfKnown f (f' ': fs) = CheckIfKnown f fs
    CheckIfKnown f '[]        = N  


-------------------------
-- === Opt queries === --
-------------------------

data Query    (mods :: [*])     (params :: [*])     = Query
data OptQuery (mods :: [Opt *]) (params :: [Opt *]) = OptQuery





------------------------
-- === OptBuilder === --
------------------------

newtype OptBuilder (mods :: [*]) (params :: [*]) a = OptBuilder a deriving (Show, Functor)
type    OptBuilderBase = OptBuilder '[] '[]

class                                                FuncTrans mods params f a | a mods params -> f        where transFunc :: OptBuilder mods params f -> a
instance (mods ~ mods', params ~ params', f ~ f') => FuncTrans mods params f (OptBuilder mods' params' f') where transFunc = id
instance (f ~ (Query mods params -> a -> b))    => FuncTrans mods params f (a -> b)                        where transFunc (OptBuilder f) = f Query

class                                                                     FuncBuilder f     a | a -> f                      where buildFunc :: f -> a
instance {-# OVERLAPPABLE #-} (f ~ a, g ~ b)                           => FuncBuilder (f -> g) (a -> b)                     where buildFunc = id
instance {-# OVERLAPPABLE #-} (t ~ (f -> g), mods ~ '[], params ~ '[]) => FuncBuilder (f -> g) (OptBuilder mods params t) where buildFunc = OptBuilder

-- utils

optBuilder :: f -> OptBuilderBase f
optBuilder = OptBuilder


queryBuilder :: FuncTrans '[] '[] f a => f -> a
queryBuilder = transFunc . optBuilder

extendOptBuilder :: Query newMods newParams 
                 -> Query collectedMods collectedParams 
                 -> OptBuilder mods params a
                 -> OptBuilder (Concat newMods   (Concat collectedMods   mods  )) 
                               (Concat newParams (Concat collectedParams params))
                               a
extendOptBuilder _ _ (OptBuilder a) = OptBuilder a

appFunc :: (f -> g) -> OptBuilder ms ps f -> OptBuilder ms ps g
appFunc = fmap

withTransFunc f = transFunc . appFunc f

--------------------------------


type Concat lst lst' = Concat' (Reverse lst) lst'

type family Concat' lst lst' where
    Concat' (x ': xs) lst = Concat' xs (x ': lst)
    Concat' '[]       lst = lst




type Reverse lst = Reverse' lst '[]

type family Reverse' (lst :: [*]) (lst' :: [*]) where
  Reverse' '[]       lst = lst
  Reverse' (l ': ls) lst = Reverse' ls (l ': lst)



----------------------------------

type family OptData provided datas opt where
    OptData (o ': ps) (d,ds) o = d 
    OptData (p ': ps) (d,ds) o = OptData ps ds o

type family QueryData provided query datas where
    QueryData p (q ': qs) d = (OptData p d q, QueryData p qs d)
    QueryData p '[]       d = ()


class GetOptData (provided :: [*]) datas opt where getOptData :: Proxy provided -> datas -> Proxy opt -> OptData provided datas opt
instance {-# OVERLAPPABLE #-} ( datas ~ (a,as)
                               , GetOptData ps as o
                               , OptData ps as o ~ OptData (p ': ps) (a, as) o
                               )             => GetOptData (p ': ps)         datas o   where getOptData _ (a,as) o = getOptData (Proxy :: Proxy ps) as o
instance {-# OVERLAPPABLE #-} datas ~ (a,as) => GetOptData (p ': ps)         datas p   where getOptData _ (a,as) _ = a


class    GetQueryData (provided :: [*]) (query :: [*]) datas where getQueryData :: Proxy provided -> Proxy query -> datas -> QueryData provided query datas
instance {-# OVERLAPPABLE #-} (GetQueryData p qs datas, GetOptData p datas q)
                           => GetQueryData p (q ': qs) datas where getQueryData p q datas = (getOptData p datas (Proxy :: Proxy q), getQueryData p (Proxy :: Proxy qs) datas)
instance {-# OVERLAPPABLE #-} GetQueryData p '[]       datas where getQueryData _ _ _     = ()


(.:) = (.) . (.)


ixed      = queryBuilder $ transFunc .: extendOptBuilder (Query :: Query '[ Ixed ] '[]                )
raw       = queryBuilder $ transFunc .: extendOptBuilder (Query :: Query '[]            '[ Raw       ])
try       = queryBuilder $ transFunc .: extendOptBuilder (Query :: Query '[]            '[ Try       ])
unchecked = queryBuilder $ transFunc .: extendOptBuilder (Query :: Query '[]            '[ Unchecked ])
unsafe    = queryBuilder $ transFunc .: extendOptBuilder (Query :: Query '[]            '[ Unsafe    ])
inplace   = queryBuilder $ transFunc .: extendOptBuilder (Query :: Query '[]            '[ Inplace   ])