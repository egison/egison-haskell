{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

module Control.Egison.Matcher (
  eql,
  integer,
  something,
  Eql(..),
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
  consPat p1 p2 = Pattern (\t ctx (List m) ->
                              case t of
                                [] -> []
                                x:xs -> [MCons (MAtom p1 x m) $ MCons (MAtom p2 xs $ List m) MNil])
  joinPat p1 p2 = Pattern (\t ctx m -> map (\(hs, ts) -> MCons (MAtom p1 hs m) $ MCons (MAtom p2 ts m) MNil) (unjoin t))

instance CollectionPat (Matcher (Multiset m)) [a] where
  nilPat = Pattern (\t ctx _ -> [MNil | null t])
  consPat p Wildcard = Pattern (\t ctx (Multiset m) -> map (\x -> MCons (MAtom p x m) MNil) t)
  consPat p1 p2 = Pattern (\t ctx (Multiset m) ->
                              case t of
                                [] -> []
                                x:xs -> map (\(x, xs) -> MCons (MAtom p1 x m) $ MCons (MAtom p2 xs $ Multiset m) MNil) $ matchAll t (list $ Matcher m) $ PCons [mc| joinPat $hs (consPat $x $ts) => (x, hs ++ ts) |] PNil)
                                -- x:xs -> map (\(x, xs) -> MCons (MAtom p1 x m) $ MCons (MAtom p2 xs $ Multiset m) MNil) $ matchAll t (list $ Matcher m) [ (joinPatL (PatVar "hs") (consPatL (PatVar "x") (PatVar "ts")), \(HCons hs (HCons x (HCons ts HNil))) -> (x, hs ++ ts)) ])

unjoin :: [a] -> [([a], [a])]
unjoin []     = [([], [])]
unjoin (x:xs) = ([], x:xs) : map (\(hs,ts) -> (x:hs, ts)) (unjoin xs)
