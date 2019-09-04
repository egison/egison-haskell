{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Egison.Matcher (
  eql,
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

eql :: Matcher Eql
eql = Matcher Eql

list :: Matcher a -> Matcher (List a)
list (Matcher m) = Matcher (List m)

multiset :: Matcher a -> Matcher (Multiset a)
multiset (Matcher m) = Matcher (Multiset m)

instance CollectionPatL (Matcher (List m)) [a] where
  nilPatL = Pattern (\t ctx _ -> [MNil | null t])
  consPatL p1 p2 = Pattern (\t ctx (List m) ->
                              case t of
                                [] -> []
                                x:xs -> [MCons (MAtom p1 x m) $ MCons (MAtom p2 xs $ List m) MNil])
  joinPatL p1 p2 = Pattern (\t ctx m -> map (\(hs, ts) -> MCons (MAtom p1 hs m) $ MCons (MAtom p2 ts m) MNil) (unjoin t))

instance CollectionPatM (Matcher (Multiset m)) [a] where
  nilPatM = Pattern (\t ctx _ -> [MNil | null t])
  consPatM p Wildcard = Pattern (\t ctx (Multiset m) -> map (\x -> MCons (MAtom p x m) MNil) t)
  consPatM p1 p2 = Pattern (\t ctx (Multiset m) ->
                              case t of
                                [] -> []
                                x:xs -> map (\(x, xs) -> MCons (MAtom p1 x m) $ MCons (MAtom p2 xs $ Multiset m) MNil) $ matchAll t (list $ Matcher m) [ (joinPatL (PatVar "hs") (consPatL (PatVar "x") (PatVar "ts")), \(HCons hs (HCons x (HCons ts HNil))) -> (x, hs ++ ts)) ])

unjoin :: [a] -> [([a], [a])]
unjoin []     = [([], [])]
unjoin (x:xs) = ([], x:xs) : map (\(hs,ts) -> (x:hs, ts)) (unjoin xs)
