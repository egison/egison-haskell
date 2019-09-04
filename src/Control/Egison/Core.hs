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
  List(..),
  CollectionPat(..),
  ) where

import           Data.Maybe

--
-- Matching states
--

data MState vs where
  MState :: vs ~ (xs :++: ys) => HList xs -> MList xs ys -> MState vs

data MAtom ctx vs = forall a m. MAtom (Pattern a m ctx vs) a
data Matcher a = Matcher a

data HList xs where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

data MList ctx vs where
  MNil :: MList ctx '[]
  MSingle :: MAtom ctx xs -> MList ctx xs
  MCons :: MAtom ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)
  MJoin :: MList ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)

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
  Pattern :: (a -> HList ctx -> ([MList ctx vs], Unit)) -> Pattern a m ctx vs
  Pattern' :: vs ~ (x ': xs) => (a -> HList ctx -> ([MList (ctx :++: '[x]) xs], x)) -> Pattern a m ctx vs

class BasePat m a where
  wildcard :: Pattern a m ctx '[]
  patVar :: String -> Pattern a m ctx '[a]
  valuePat :: Eq a => (HList ctx -> a) -> Pattern a m ctx '[]

data List a = List a

class CollectionPat m a where
  nilPat       :: a ~ [b] => Pattern a m ctx '[]
  consPat      :: a ~ [b] => m ~ (Matcher (List m')) => Pattern b (Matcher m') ctx xs -> Pattern a m (ctx :++: xs) ys -> Pattern a m ctx (xs :++: ys)
  -- joinPat      :: a ~ [b] => Pattern a m xs -> Pattern a m ys -> Pattern a m (xs :++: ys)

-- data Pattern a where
--   AndPat       :: Pattern a -> Pattern a -> Pattern a
--   OrPat        :: Pattern a -> Pattern a -> Pattern a
--   NotPat       :: Pattern a -> Pattern a
--   LaterPat     :: Pattern a -> Pattern a
--   PredicatePat :: (a -> Bool) -> Pattern a
--
--   UserPat      :: String -> [Pat] -> Pattern a
