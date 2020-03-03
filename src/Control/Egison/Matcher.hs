{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

-- | Matcher definitions.

module Control.Egison.Matcher (
  -- * @Something@ matcher
  Something(..),
  -- * @Eql@ and @Integer@ matchers
  ValuePat(..),
  Eql(..),
  Integer(..),
  -- * @Pair@ matcher
  PairPat(..),
  Pair(..),
  -- * Matchers for collections
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

-- | Something built-in matcher.
-- The @Something@ matcher can handle only a pattern variable and a wildcard.
data Something = Something
instance Matcher Something a

-- | Value patterns.
class ValuePat m a where
  valuePat :: (Matcher m a, Eq a) => (HList ctx -> a) -> Pattern a m ctx '[]

-- | A matcher for data types that are instances of @Eq@.
-- The @Eql@ matcher can handle a pattern variable, a wildcard, and a value pattern.
data Eql = Eql
instance (Eq a) => Matcher Eql a

instance Eq a => ValuePat Eql a where
  valuePat f = Pattern (\ctx _ tgt -> [MNil | f ctx == tgt])

-- | A matcher for integers.
-- The @Integer@ matcher can handle a pattern variable, a wildcard, and a value pattern.
data Integer = Integer
instance Integral a => Matcher Integer a

instance Integral a => ValuePat Integer a where
  valuePat f = Pattern (\ctx _ tgt -> [MNil | f ctx == tgt])


-- | A pattern constructor for pairs.
class PairPat m a where
  pair :: (Matcher m a , a ~ (b1, b2), m ~ (Pair m1 m2))
       => Pattern b1 m1 ctx xs
       -> Pattern b2 m2 (ctx :++: xs) ys
       -> Pattern a m ctx (xs :++: ys)

-- | A matcher for a pair of data.
data Pair m1 m2 = Pair m1 m2
instance (Matcher m1 a1, Matcher m2 a2) => Matcher (Pair m1 m2) (a1, a2)

instance (Matcher m1 a1, Matcher m2 a2) => PairPat (Pair m1 m2) (a1, a2) where
  pair p1 p2 = Pattern (\_ (Pair m1 m2) (t1, t2) -> [twoMAtoms (MAtom p1 m1 t1) (MAtom p2 m2 t2)])


-- | Patterns for collections.
class CollectionPat m a where
  -- | The @nil@ pattern matches an empty collection.
  nil  :: (Matcher m a) => Pattern a m ctx '[]
  -- | The @cons@ pattern decomposes a collection into an element and the rest elements.
  cons :: (Matcher m a, a ~ [a'], m ~ (f m'))
       => Pattern a' m' ctx xs
       -> Pattern a m (ctx :++: xs) ys
       -> Pattern a m ctx (xs :++: ys)
  -- | The @join@ pattern decomposes a collection into two collections.
  join :: (Matcher m a)
       => Pattern a m ctx xs
       -> Pattern a m (ctx :++: xs) ys
       -> Pattern a m ctx (xs :++: ys)

-- | A matcher for a list.
newtype List m = List m
instance (Matcher m a) => Matcher (List m) [a]

instance (Matcher m a, Eq a, ValuePat m a) => ValuePat (List m) [a] where
  valuePat f = Pattern (\ctx (List m) tgt ->
                            match (f ctx, tgt) (Pair (List m) (List m)) $
                              [[mc| ([], []) => [MNil] |],
                               [mc| (($x : $xs), (#x : #xs)) => [MNil] |],
                               [mc| _ => [] |]])

instance Matcher m a => CollectionPat (List m) [a] where
  nil = Pattern (\_ _ t -> [MNil | null t])
  cons p1 p2 = Pattern (\_ (List m) tgt ->
                              case tgt of
                                [] -> []
                                x:xs -> [twoMAtoms (MAtom p1 m x) (MAtom p2 (List m) xs)])
  join Wildcard p2 = Pattern (\_ m tgt -> map (\ts -> oneMAtom (MAtom p2 m ts)) (tails tgt))
  join p1 p2 = Pattern (\_ m tgt -> map (\(hs, ts) -> twoMAtoms (MAtom p1 m hs) (MAtom p2 m ts)) (splits tgt))

splits :: [a] -> [([a], [a])]
splits []     = [([], [])]
splits (x:xs) = ([], x:xs) : [(x:ys, zs) | (ys, zs) <- splits xs]

-- | A matcher for a multiset.
-- When we regard a collection as a multiset, the order of elements is ignored but the number of times an element appears in the collection is counted.
newtype Multiset m = Multiset m
instance (Matcher m a) => Matcher (Multiset m) [a]

instance (Matcher m a, Eq a, ValuePat m a) => ValuePat (Multiset m) [a] where
  valuePat f = Pattern (\ctx (Multiset m) tgt ->
                            match (f ctx, tgt) (Pair (List m) (Multiset m)) $
                              [[mc| ([], []) => [MNil] |],
                               [mc| (($x : $xs), (#x : #xs)) => [MNil] |],
                               [mc| _ => [] |]])

instance (Matcher m a) => CollectionPat (Multiset m) [a] where
  nil = Pattern (\_ _ tgt -> [MNil | null tgt])
  -- | The @cons@ pattern for a multiset decomposes a collection into an arbitrary element and the rest elements.
  cons p Wildcard = Pattern (\_ (Multiset m) tgt -> map (\x -> oneMAtom (MAtom p m x)) tgt)
  cons p1 p2 = Pattern (\_ (Multiset m) tgt -> map (\(x, xs) -> twoMAtoms (MAtom p1 m x) (MAtom p2 (Multiset m) xs))
                                                   (matchAll tgt (List m) [[mc| $hs ++ $x : $ts => (x, hs ++ ts) |]]))
  join p1 p2 = undefined

-- | A matcher for a set. Both the order and the repetition of elements are ignored.
newtype Set m = Set m
instance (Matcher m a) => Matcher (Set m) [a]

instance (Matcher m a, Eq a,  Ord a, ValuePat m a) => ValuePat (Set m) [a] where
  valuePat f = undefined

instance Matcher m a => CollectionPat (Set m) [a] where
  nil = Pattern (\_ _ tgt -> [MNil | null tgt])
  cons p1 p2 = Pattern (\_ (Set m) tgt ->
                  map (\x -> twoMAtoms (MAtom p1 m x) (MAtom p2 (Set m) tgt))
                      (matchAll tgt (List m) [[mc| _ ++ $x : _ => x |]]))
  join p1 p2 = undefined
