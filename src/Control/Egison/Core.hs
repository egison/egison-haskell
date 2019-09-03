{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Control.Egison.Core (
  MState(..),
  MAtom(..),
  Matcher(..),
  HList(..),
  MList(..),
  happend,
  Unit(..),
  Pattern(..),
  BasePat(..),
  CollectionPat(..),
  ) where

import           Data.Maybe

--
-- Matching states
--

data MState vs where
  MState :: vs ~ (xs :++: ys) => HList xs -> MList ys -> MState vs

data MAtom vs = forall a m ctx. MAtom (Pattern a m ctx vs) a
data Matcher a = Matcher a

data HList xs where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

data MList vs where
  MNil :: MList '[]
  MSingle :: MAtom xs -> MList xs
  MCons :: MAtom xs -> MList ys -> MList (xs :++: ys)
  MJoin :: MList xs -> MList ys -> MList (xs :++: ys)

happend :: HList as -> HList bs -> HList (as :++: bs)
happend (HCons x xs) ys = HCons x $ happend xs ys
happend HNil ys         = ys

type family as :++: bs :: [*] where
  (a ': as') :++: bs = a ': (as' :++: bs)
  '[] :++: bs = bs

--
-- Patterns
--

data Unit = Unit

data Pattern a m ctx vs where
  Pattern :: vs ~ (x ': xs) => (a -> HList ctx -> ([MList xs], x)) -> Pattern a m ctx vs

class BasePat m a where
  wildcard :: Pattern a m ctx '[Unit]
  patVar :: String -> Pattern a m ctx '[a]
  valuePat :: Eq a => (HList ctx -> a) -> Pattern a m ctx '[Unit]

class CollectionPat m a where
  nilPat       :: a ~ [b] => Pattern a m ctx '[Unit]
  consPat      :: a ~ [b] => Pattern b m ctx xs -> Pattern a m (ctx :++: xs) ys -> Pattern a m (ctx :++: xs :++: ys) (Unit ': (xs :++: ys))
  -- joinPat      :: a ~ [b] => Pattern a m xs -> Pattern a m ys -> Pattern a m (xs :++: ys)

-- data Pattern a where
--   AndPat       :: Pattern a -> Pattern a -> Pattern a
--   OrPat        :: Pattern a -> Pattern a -> Pattern a
--   NotPat       :: Pattern a -> Pattern a
--   LaterPat     :: Pattern a -> Pattern a
--   PredicatePat :: (a -> Bool) -> Pattern a
--
--   UserPat      :: String -> [Pat] -> Pattern a
