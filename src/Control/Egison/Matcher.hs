{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

module Control.Egison.Matcher (
  eql,
  Eql(..),
  -- integer,
  -- list,
  -- multiset,
  ) where

import           Control.Egison.Core
import           Control.Egison.Match
import           Control.Egison.QQ
-- import           Unsafe.Coerce

--
-- Matchers
--

data Eql = Eql

eql :: Matcher Eql
eql = Matcher Eql

data List a = List (Matcher a)

list :: Matcher a -> Matcher (List a)
list m = Matcher (List m)

-- instance Eq a => BasePat a (Matcher Eql) where
instance BasePat [Char] (Matcher Eql) where
  wildcard = Pattern (\t -> ([[]], Unit))
  patVar _ = Pattern (\t -> ([[]], Just t))
  valuePat' v = Pattern (\t -> ([[] | v == t], Unit))

instance BasePat [a] (Matcher (List a)) where
  wildcard = Pattern (\t -> ([[]], Unit))
  patVar _ = Pattern (\t -> ([[]], Just t))
  valuePat' v = Pattern (\t -> ([[] | v == t], Unit))

instance CollectionPat [a] (Matcher (List a)) where
  nilPat = Pattern (\t -> ([[] | null t], Unit))
  consPat p1 p2 = Pattern (\t -> case t of
                                   [] -> ([], Unit)
                                   x:xs -> ([[MAtom p1 x, MAtom p2 xs]], Unit))

-- integer :: Eq a => Matcher a
-- integer = eql

-- list :: Matcher a -> Matcher [a]
-- list m = Matcher (list' m)
--
-- list' :: Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
-- list' m (JoinPat p1 p2) t = map (\(hs, ts) -> [MAtom p1 (list m) hs, MAtom p2 (list m) ts]) (unjoin t)
-- list' m (UserPat "Join" [Pat p1, Pat p2]) t =
--   let p1' = unsafeCoerce p1 in
--   let p2' = unsafeCoerce p2 in
--   map (\(hs, ts) -> [MAtom p1' (list m) hs, MAtom p2' (list m) ts]) (unjoin t)
--
-- multiset :: Matcher a -> Matcher [a]
-- multiset m = Matcher (multiset' m)
--
-- multiset' :: Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
-- multiset' _ p@Wildcard t = [[MAtom p something t]]
-- multiset' _ p@(PatVar _) t = [[MAtom p something t]]
-- multiset' _ NilPat t = [[] | null t]
-- multiset' m (ConsPat p Wildcard) t = map (\x -> [MAtom p m x]) t
-- multiset' m (ConsPat p1 p2) t =
--   map (\(x, xs) -> [MAtom p1 m x, MAtom p2 (multiset m) xs])
--     (matchAll t (list m)
--       [ [mc| UserPat "Join" [Pat $hs, Pat (ConsPat $x $ts)] => (x, hs ++ ts)|] ])
--
-- unjoin :: [a] -> [([a], [a])]
-- unjoin []     = [([], [])]
-- unjoin (x:xs) = ([], x:xs) : map (\(hs,ts) -> (x:hs, ts)) (unjoin xs)
