{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

module Control.Egison.Matcher (
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
  ) where

import           Control.Egison.Core
import           Control.Egison.Match
import           Control.Egison.QQ

--
-- Matchers
--

data Eql = Eql

data Something = Something

newtype List a = List a
newtype Multiset a = Multiset a

eql :: Matcher Eql
eql = Matcher Eql

integer :: Matcher Eql
integer = eql

something :: Matcher Something
something = Matcher Something

pair :: Matcher a -> Matcher b -> Matcher (Pair a b)
pair (Matcher m1) (Matcher m2) = Matcher (Pair m1 m2)

list :: Matcher a -> Matcher (List a)
list (Matcher m) = Matcher (List m)

multiset :: Matcher a -> Matcher (Multiset a)
multiset (Matcher m) = Matcher (Multiset m)

instance Eq a => ValuePat (Matcher Eql) a where
  valuePat f = Pattern (\tgt ctx _ -> [MNil | f ctx == tgt])

instance Eq a => ValuePat (Matcher (List m)) [a] where
  valuePat f = Pattern (\tgt ctx _ -> [MNil | f ctx == tgt])

instance (Eq a,  ValuePat (Matcher m) a) => ValuePat (Matcher (Multiset m)) [a] where
  valuePat f = Pattern (\tgt ctx (Multiset m) ->
                            match (f ctx, tgt) (pair (list $ Matcher m) (multiset $ Matcher m)) $
                              [mc| pairPat nil nil => [MNil] |] .*.
                              [mc| pairPat (cons $x $xs) (cons #x #xs) => [MNil] |] .*.
                              [mc| pairPat Wildcard Wildcard => [] |] .*. PNil)

instance PairPat (Matcher (Pair m1 m2)) (a1, a2) where
  pairPat p1 p2 = Pattern (\(t1, t2) _ (Pair m1 m2) -> [MCons (MAtom p1 t1 m1) $ MCons (MAtom p2 t2 m2) MNil])

instance CollectionPat (Matcher (List m)) [a] where
  nil = Pattern (\t ctx _ -> [MNil | null t])
  cons p1 p2 = Pattern (\tgt ctx (List m) ->
                              case tgt of
                                [] -> []
                                x:xs -> [MCons (MAtom p1 x m) $ MCons (MAtom p2 xs $ List m) MNil])
  join p1 p2 = Pattern (\tgt ctx m -> map (\(hs, ts) -> MCons (MAtom p1 hs m) $ MCons (MAtom p2 ts m) MNil) (unjoin tgt))

instance CollectionPat (Matcher (Multiset m)) [a] where
  nil = Pattern (\tgt ctx _ -> [MNil | null tgt])
  cons p Wildcard = Pattern (\tgt ctx (Multiset m) -> map (\x -> MCons (MAtom p x m) MNil) tgt)
  cons p1 p2 = Pattern (\tgt ctx (Multiset m) -> map (\(x, xs) -> MCons (MAtom p1 x m) $ MCons (MAtom p2 xs $ Multiset m) MNil) $ matchAll tgt (list $ Matcher m) $ [mc| join $hs (cons $x $ts) => (x, hs ++ ts) |] .*. PNil)
  join p1 p2 = Pattern (\tgt ctx m -> map (\(xs, ys) -> MCons (MAtom p1 xs m) $ MCons (MAtom p2 ys m) MNil) $ concatMap (`unjoinM` tgt) [0..(length tgt)])

unjoin :: [a] -> [([a], [a])]
unjoin []     = [([], [])]
unjoin (x:xs) = ([], x:xs) : map (\(hs,ts) -> (x:hs, ts)) (unjoin xs)

unjoinM :: Int -> [a] -> [([a], [a])]
unjoinM 0 xs = [([], xs)]
unjoinM n [] = []
unjoinM n (x:xs) = map (\(as, bs) -> (x:as, bs)) (unjoinM (n-1) xs) ++ map (\(as, bs) -> (as, x:bs)) (unjoinM n xs)
