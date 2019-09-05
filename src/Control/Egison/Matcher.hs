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

list :: Matcher a -> Matcher (List a)
list (Matcher m) = Matcher (List m)

multiset :: Matcher a -> Matcher (Multiset a)
multiset (Matcher m) = Matcher (Multiset m)

instance CollectionPat (Matcher (List m)) [a] where
  nilPat = Pattern (\t ctx _ -> [MNil | null t])
  consPat p1 p2 = Pattern (\tgt ctx (List m) ->
                              case tgt of
                                [] -> []
                                x:xs -> [MCons (MAtom p1 x m) $ MCons (MAtom p2 xs $ List m) MNil])
  joinPat p1 p2 = Pattern (\tgt ctx m -> map (\(hs, ts) -> MCons (MAtom p1 hs m) $ MCons (MAtom p2 ts m) MNil) (unjoin tgt))

instance CollectionPat (Matcher (Multiset m)) [a] where
  nilPat = Pattern (\tgt ctx _ -> [MNil | null tgt])
  consPat p Wildcard = Pattern (\tgt ctx (Multiset m) -> map (\x -> MCons (MAtom p x m) MNil) tgt)
  consPat p1 p2 = Pattern (\tgt ctx (Multiset m) ->
                              case tgt of
                                [] -> []
                                _ -> map (\(x, xs) -> MCons (MAtom p1 x m) $ MCons (MAtom p2 xs $ Multiset m) MNil) $ matchAll tgt (list $ Matcher m) $ PCons [mc| joinPat $hs (consPat $x $ts) => (x, hs ++ ts) |] PNil)
  joinPat p1 p2 = Pattern (\tgt ctx m ->
                              case tgt of
                                [] -> [MCons (MAtom p1 [] m) $ MCons (MAtom p2 [] m) MNil]
                                (a:as) ->
                                  let ls = concatMap (\(bs, cs) -> [(bs, a:cs), (a:bs, cs)]) $ matchAll as (Matcher m) $ PCons [mc| joinPat $bs $cs => (bs, cs) |] PNil in
                                  map (\(xs, ys) -> MCons (MAtom p1 xs m) $ MCons (MAtom p2 ys m) MNil) ls)

unjoin :: [a] -> [([a], [a])]
unjoin []     = [([], [])]
unjoin (x:xs) = ([], x:xs) : map (\(hs,ts) -> (x:hs, ts)) (unjoin xs)
