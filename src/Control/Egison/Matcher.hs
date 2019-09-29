{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Egison.Matcher (
  -- Something matcher
  Something(..),
  -- Eql and Integer matchers
  ValuePat(..),
  Eql(..),
  Integer(..),
  -- Pair matcher
  PairPat(..),
  Pair(..),
  -- Matchers for collections
  CollectionPat(..),
  List(..),
  Multiset(..),
  Set(..),
  ) where

import           Prelude hiding (Integer)
import           Data.List (tails)
import           Control.Egison.Core
import           Control.Egison.Match
import           Control.Egison.QQ

--
-- Something matcher
--

data Something = Something
instance Matcher Something a

--
-- Eql and Integer matchers
--

class ValuePat m a where
  valuePat :: (Matcher m a, Eq a) => (HList ctx -> a) -> Pattern a m ctx '[]

-- Eql matcher
data Eql = Eql
instance Matcher Eql a

instance Eq a => ValuePat Eql a where
  valuePat f = Pattern (\ctx _ tgt -> [MNil | f ctx == tgt])

-- Integer matcher
data Integer = Integer
instance Integral a => Matcher Integer a

instance Integral a => ValuePat Integer a where
  valuePat f = Pattern (\ctx _ tgt -> [MNil | f ctx == tgt])

---
--- Pair matcher
---

data Pair m1 m2 = Pair m1 m2
instance (Matcher m1 a1, Matcher m2 a2) => Matcher (Pair m1 m2) (a1, a2)

class PairPat m a where
  pair :: (Matcher m a , a ~ (b1, b2), m ~ (Pair m1 m2)) => Pattern b1 m1 ctx xs -> Pattern b2 m2 (ctx :++: xs) ys -> Pattern a m ctx (xs :++: ys)

instance (Matcher m1 a1, Matcher m2 a2) => PairPat (Pair m1 m2) (a1, a2) where
  pair p1 p2 = Pattern (\_ (Pair m1 m2) (t1, t2) -> [MCons (MAtom p1 m1 t1) $ MCons (MAtom p2 m2 t2) MNil])

---
--- Matchers for collections
---

class CollectionPat m a where
  nil  :: (Matcher m a) => Pattern a m ctx '[]
  cons :: (Matcher m a, a ~ [a'], m ~ (f m')) => Pattern a' m' ctx xs -> Pattern a m (ctx :++: xs) ys -> Pattern a m ctx (xs :++: ys)
  join :: (Matcher m a) => Pattern a m ctx xs -> Pattern a m (ctx :++: xs) ys -> Pattern a m ctx (xs :++: ys)

-- List matcher
newtype List m = List m
instance (Matcher m a) => Matcher (List m) [a]

instance (Matcher m a, Eq a, ValuePat m a) => ValuePat (List m) [a] where
  valuePat f = Pattern (\ctx (List m) tgt ->
                            match (f ctx, tgt) (Pair (List m) (List m)) $
                              [[mc| pair nil nil => [MNil] |],
                               [mc| pair (cons $x $xs) (cons #x #xs) => [MNil] |],
                               [mc| Wildcard => [] |]])

instance Matcher m a => CollectionPat (List m) [a] where
  nil = Pattern (\_ _ t -> [MNil | null t])
  cons p1 p2 = Pattern (\_ (List m) tgt ->
                              case tgt of
                                [] -> []
                                x:xs -> [MCons (MAtom p1 m x) $ MCons (MAtom p2 (List m) xs) MNil])
  join Wildcard p2 = Pattern (\_ m tgt -> map (\ts -> MCons (MAtom p2 m ts) MNil) (tails tgt))
  join p1 p2 = Pattern (\_ m tgt -> map (\(hs, ts) -> MCons (MAtom p1 m hs) $ MCons (MAtom p2 m ts) MNil) (splits tgt))

splits :: [a] -> [([a], [a])]
splits []     = [([], [])]
splits (x:xs) = ([], x:xs) : [(x:ys, zs) | (ys, zs) <- splits xs]

-- Multiset matcher
newtype Multiset m = Multiset m
instance (Matcher m a) => Matcher (Multiset m) [a]

instance (Matcher m a, Eq a, ValuePat m a) => ValuePat (Multiset m) [a] where
  valuePat f = Pattern (\ctx (Multiset m) tgt ->
                            match (f ctx, tgt) (Pair (List m) (Multiset m)) $
                              [[mc| pair nil nil => [MNil] |],
                               [mc| pair (cons $x $xs) (cons #x #xs) => [MNil] |],
                               [mc| Wildcard => [] |]])

instance (Matcher m a) => CollectionPat (Multiset m) [a] where
  nil = Pattern (\_ _ tgt -> [MNil | null tgt])
  cons p Wildcard = Pattern (\_ (Multiset m) tgt -> map (\x -> MCons (MAtom p m x) MNil) tgt)
  cons p1 p2 = Pattern (\_ (Multiset m) tgt -> map (\(x, xs) -> MCons (MAtom p1 m x) $ MCons (MAtom p2 (Multiset m) xs) MNil)
                                                   (matchAll tgt (List m) [[mc| join $hs (cons $x $ts) => (x, hs ++ ts) |]]))
  join p1 p2 = undefined

-- Set matcher
newtype Set m = Set m
instance (Matcher m a) => Matcher (Set m) [a]

instance (Matcher m a, Eq a,  Ord a, ValuePat m a) => ValuePat (Set m) [a] where
  valuePat f = undefined

instance Matcher m a => CollectionPat (Set m) [a] where
  nil = Pattern (\_ _ tgt -> [MNil | null tgt])
  cons p1 p2 = Pattern (\_ (Set m) tgt ->
                  map (\x -> MCons (MAtom p1 m x) $ MCons (MAtom p2 (Set m) tgt) MNil)
                      (matchAll tgt (List m) [[mc| join Wildcard (cons $x Wildcard) => x |]]))
  join p1 p2 = undefined
