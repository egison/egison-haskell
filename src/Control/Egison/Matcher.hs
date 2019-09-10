{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Egison.Matcher (
  -- pattern
  ValuePat(..),
  Pair(..),
  PairPat(..),
  CollectionPat(..),
  -- data type of matcher
  Eql(..),
  Something(..),
  List(..),
  Multiset(..),
  -- matcher
  eql,
  integer,
  something,
  list,
  multiset,
  set,
  ) where

import           Control.Egison.Core
import           Control.Egison.Match
import           Control.Egison.QQ
import           Data.List.Unique

--
-- Pattern
--

class ValuePat mt a where
  valuePat :: Eq a => (HList ctx -> a) -> Pattern a ctx mt '[]

data Pair a b = Pair a b

class PairPat mt a where
  pairPat :: a ~ (b1, b2) => mt ~ Matcher (Pair m1 m2) => Pattern b1 ctx (Matcher m1) xs -> Pattern b2 (ctx :++: xs) (Matcher m2) ys -> Pattern a ctx mt (xs :++: ys)

class CollectionPat mt a where
  nil :: a ~ [b] => Pattern a ctx mt '[]
  cons :: a ~ [b] => mt ~ Matcher (f m) => Pattern b ctx (Matcher m) xs -> Pattern a (ctx :++: xs) mt ys -> Pattern a ctx mt (xs :++: ys)
  join :: a ~ [b] => Pattern a ctx mt xs -> Pattern a (ctx :++: xs) mt ys -> Pattern a ctx mt (xs :++: ys)

--
-- Matchers
--

data Something = Something

integer :: Matcher Eql
integer = eql

something :: Matcher Something
something = Matcher Something

-- eq matcher
data Eql = Eql

eql :: Matcher Eql
eql = Matcher Eql

instance Eq a => ValuePat (Matcher Eql) a where
  valuePat f = Pattern (\tgt ctx _ -> [MNil | f ctx == tgt])

-- pair matcher
pair :: Matcher a -> Matcher b -> Matcher (Pair a b)
pair (Matcher m1) (Matcher m2) = Matcher (Pair m1 m2)

instance PairPat (Matcher (Pair m1 m2)) (a1, a2) where
  pairPat p1 p2 = Pattern (\(t1, t2) _ (Pair m1 m2) -> [MCons (MAtom p1 t1 m1) $ MCons (MAtom p2 t2 m2) MNil])

-- list matcher
list :: Matcher a -> Matcher (List a)
list (Matcher m) = Matcher (List m)

newtype List a = List a
instance Eq a => ValuePat (Matcher (List m)) [a] where
  valuePat f = Pattern (\tgt ctx _ -> [MNil | f ctx == tgt])

instance CollectionPat (Matcher (List m)) [a] where
  nil = Pattern (\t ctx _ -> [MNil | null t])
  cons p1 p2 = Pattern (\tgt ctx (List m) ->
                              case tgt of
                                [] -> []
                                x:xs -> [MCons (MAtom p1 x m) $ MCons (MAtom p2 xs $ List m) MNil])
  join p1 p2 = Pattern (\tgt ctx m -> map (\(hs, ts) -> MCons (MAtom p1 hs m) $ MCons (MAtom p2 ts m) MNil) (unjoin tgt))

unjoin :: [a] -> [([a], [a])]
unjoin []     = [([], [])]
unjoin (x:xs) = ([], x:xs) : map (\(hs,ts) -> (x:hs, ts)) (unjoin xs)

-- multiset matcher
newtype Multiset a = Multiset a

multiset :: Matcher a -> Matcher (Multiset a)
multiset (Matcher m) = Matcher (Multiset m)

instance (Eq a,  ValuePat (Matcher m) a) => ValuePat (Matcher (Multiset m)) [a] where
  valuePat f = Pattern (\tgt ctx (Multiset m) ->
                            match (f ctx, tgt) (pair (list $ Matcher m) (multiset $ Matcher m)) $
                              [mc| pairPat nil nil => [MNil] |] .*.
                              [mc| pairPat (cons $x $xs) (cons #x #xs) => [MNil] |] .*.
                              [mc| Wildcard => [] |] .*. PNil)

instance CollectionPat (Matcher (Multiset m)) [a] where
  nil = Pattern (\tgt ctx _ -> [MNil | null tgt])
  cons p Wildcard = Pattern (\tgt ctx (Multiset m) -> map (\x -> MCons (MAtom p x m) MNil) tgt)
  cons p1 p2 = Pattern (\tgt ctx (Multiset m) -> map (\(x, xs) -> MCons (MAtom p1 x m) $ MCons (MAtom p2 xs $ Multiset m) MNil) $ matchAll tgt (list $ Matcher m) $ [mc| join $hs (cons $x $ts) => (x, hs ++ ts) |] .*. PNil)
  join p1 p2 = Pattern (\tgt ctx m -> map (\(xs, ys) -> MCons (MAtom p1 xs m) $ MCons (MAtom p2 ys m) MNil) $ concatMap (`unjoinM` tgt) [0..(length tgt)])

unjoinM :: Int -> [a] -> [([a], [a])]
unjoinM 0 xs = [([], xs)]
unjoinM n [] = []
unjoinM n (x:xs) = map (\(as, bs) -> (x:as, bs)) (unjoinM (n-1) xs) ++ map (\(as, bs) -> (as, x:bs)) (unjoinM n xs)

-- set matcher
newtype Set a = Set a

set :: Matcher a -> Matcher (Set a)
set (Matcher m) = Matcher (Set m)

instance (Eq a,  Ord a, ValuePat (Matcher m) a) => ValuePat (Matcher (Set m)) [a] where
  valuePat f = Pattern (\tgt ctx (Set m) ->
                  match (unique (f ctx), unique tgt) (pair (list $ Matcher m) (multiset $ Matcher m)) $
                    [mc| pairPat nil nil => [MNil] |] .*.
                    [mc| pairPat (cons $x $xs) (cons #x #xs) => [MNil] |] .*.
                    [mc| Wildcard => [] |] .*. PNil)

instance CollectionPat (Matcher (Set m)) [a] where
  nil = Pattern (\tgt ctx _ -> [MNil | null tgt])
  cons p1 p2 = Pattern (\tgt ctx (Set m) ->
                  map (\x -> MCons (MAtom p1 x m) $ MCons (MAtom p2 tgt (Set m)) MNil) $ matchAll tgt (list $ Matcher m) $ [mc| join Wildcard (cons $x Wildcard) => x |] .*. PNil)
  join p1 p2 = undefined
