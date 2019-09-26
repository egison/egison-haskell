{-# LANGUAGE FlexibleContexts      #-}
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
import           Control.Egison.Core
import           Control.Egison.Match
import           Control.Egison.QQ
import           Data.List.Unique

--
-- Something matcher
--

data Something = Something
instance Matcher Something

--
-- Eql and Integer matchers
--

class ValuePat mt a where
  valuePat :: (Matcher mt, Eq a) => (HList ctx -> a) -> Pattern a ctx mt '[]

-- Eql matcher
data Eql = Eql
instance Matcher Eql

instance Eq a => ValuePat Eql a where
  valuePat f = Pattern (\tgt ctx _ -> [MNil | f ctx == tgt])

-- Integer matcher
data Integer = Integer
instance Matcher Integer

instance Integral a => ValuePat Integer a where
  valuePat f = Pattern (\tgt ctx _ -> [MNil | f ctx == tgt])

---
--- Pair matcher
---

data Pair a b = Pair a b
instance (Matcher a, Matcher b) => Matcher (Pair a b)

class PairPat mt a where
  pair :: (Matcher mt, a ~ (b1, b2), mt ~ (Pair m1 m2)) => Pattern b1 ctx m1 xs -> Pattern b2 (ctx :++: xs) m2 ys -> Pattern a ctx mt (xs :++: ys)

instance (Matcher m1, Matcher m2) => PairPat (Pair m1 m2) (a1, a2) where
  pair p1 p2 = Pattern (\(t1, t2) _ (Pair m1 m2) -> [MCons (MAtom p1 m1 t1) $ MCons (MAtom p2 m2 t2) MNil])

---
--- Matchers for collections
---

class CollectionPat mt a where
  nil  :: (Matcher mt, a ~ [b]) => Pattern a ctx mt '[]
  cons :: (Matcher mt, a ~ [b], mt ~ (f m)) => Pattern b ctx m xs -> Pattern a (ctx :++: xs) mt ys -> Pattern a ctx mt (xs :++: ys)
  join :: (Matcher mt, a ~ [b]) => Pattern a ctx mt xs -> Pattern a (ctx :++: xs) mt ys -> Pattern a ctx mt (xs :++: ys)

-- List matcher
newtype List a = List a
instance (Matcher a) => Matcher (List a)

instance (Matcher m, Eq a) => ValuePat (List m) [a] where -- todo: Fix `Eq a` to `ValuePat m a`
  valuePat f = Pattern (\tgt ctx _ -> [MNil | f ctx == tgt])

instance Matcher m => CollectionPat (List m) [a] where
  nil = Pattern (\t ctx _ -> [MNil | null t])
  cons p1 p2 = Pattern (\tgt ctx (List m) ->
                              case tgt of
                                [] -> []
                                x:xs -> [MCons (MAtom p1 m x) $ MCons (MAtom p2 (List m) xs) MNil])
  join p1 p2 = Pattern (\tgt ctx m -> map (\(hs, ts) -> MCons (MAtom p1 m hs) $ MCons (MAtom p2 m ts) MNil) (unjoin tgt))

unjoin :: [a] -> [([a], [a])]
unjoin []     = [([], [])]
unjoin (x:xs) = ([], x:xs) : map (\(hs,ts) -> (x:hs, ts)) (unjoin xs)

-- Multiset matcher
newtype Multiset a = Multiset a
instance (Matcher a) => Matcher (Multiset a)

instance (Matcher m, Eq a, ValuePat m a) => ValuePat (Multiset m) [a] where
  valuePat f = Pattern (\tgt ctx (Multiset m) ->
                            match (f ctx, tgt) (Pair (List m) (Multiset m)) $
                              [[mc| pair nil nil => [MNil] |],
                               [mc| pair (cons $x $xs) (cons #x #xs) => [MNil] |],
                               [mc| Wildcard => [] |]])

instance (Matcher m) => CollectionPat (Multiset m) [a] where
  nil = Pattern (\tgt ctx _ -> [MNil | null tgt])
  cons p Wildcard = Pattern (\tgt ctx (Multiset m) -> map (\x -> MCons (MAtom p m x) MNil) tgt)
  cons p1 p2 = Pattern (\tgt ctx (Multiset m) -> map (\(x, xs) -> MCons (MAtom p1 m x) $ MCons (MAtom p2 (Multiset m) xs) MNil)
                                                     (matchAll tgt (List m) [[mc| join $hs (cons $x $ts) => (x, hs ++ ts) |]]))
  join p1 p2 = Pattern (\tgt ctx m -> map (\(xs, ys) -> MCons (MAtom p1 m xs) $ MCons (MAtom p2 m ys) MNil)
                                          (concatMap (`unjoinM` tgt) [0..(length tgt)]))

unjoinM :: Int -> [a] -> [([a], [a])]
unjoinM 0 xs = [([], xs)]
unjoinM n [] = []
unjoinM n (x:xs) = map (\(as, bs) -> (x:as, bs)) (unjoinM (n-1) xs) ++ map (\(as, bs) -> (as, x:bs)) (unjoinM n xs)

-- Set matcher
newtype Set a = Set a
instance (Matcher a) => Matcher (Set a)

instance (Matcher m, Eq a,  Ord a, ValuePat m a) => ValuePat (Set m) [a] where
  valuePat f = Pattern (\tgt ctx (Set m) ->
                  match (unique (f ctx), unique tgt) (Pair (List m) (Multiset m)) $
                    [[mc| pair nil nil => [MNil] |],
                     [mc| pair (cons $x $xs) (cons #x #xs) => [MNil] |],
                     [mc| Wildcard => [] |]])

instance Matcher m => CollectionPat (Set m) [a] where
  nil = Pattern (\tgt ctx _ -> [MNil | null tgt])
  cons p1 p2 = Pattern (\tgt ctx (Set m) ->
                  map (\x -> MCons (MAtom p1 m x) $ MCons (MAtom p2 (Set m) tgt) MNil) $ matchAll tgt (List m) [[mc| join Wildcard (cons $x Wildcard) => x |]])
  join p1 p2 = undefined
