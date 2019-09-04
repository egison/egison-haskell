{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
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
  List(..),
  Multiset(..),
  CollectionPatL(..),
  CollectionPatM(..),
  ) where

import           Data.Maybe
import           Unsafe.Coerce

--
-- Matching states
--

data MState vs where
  MState :: vs ~ (xs :++: ys) => HList xs -> MList xs ys -> MState vs

data MAtom ctx vs = forall a m. MAtom (Pattern a (Matcher m) ctx vs) a m
data Matcher a = Matcher a

data HList xs where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

data MList ctx vs where
  MNil :: MList ctx '[]
  MCons :: MAtom ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)
  MJoin :: MList ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)

happend :: HList as -> HList bs -> HList (as :++: bs)
happend (HCons x xs) ys = unsafeCoerce $ HCons x $ happend xs ys
happend HNil ys         = ys

type family as :++: bs :: [*] where
  bs :++: '[] = bs
  (a ': as') :++: bs = a ': (as' :++: bs)
  '[] :++: bs = bs

--
-- Patterns
--

data Unit = Unit

data Pattern a mt ctx vs where
  Pattern :: mt ~ Matcher m => (a -> HList ctx -> m -> [MList ctx vs]) -> Pattern a mt ctx vs

  Wildcard :: Pattern a mt ctx '[]
  PatVar :: String -> Pattern a mt ctx '[a]
  ValuePat :: Eq a => (HList ctx -> a) -> Pattern a mt ctx '[]
  AndPat :: Pattern a mt ctx vs -> Pattern a mt (ctx :++: vs) vs' -> Pattern a mt ctx (vs :++: vs')
  OrPat :: Pattern a mt ctx vs -> Pattern a mt ctx vs -> Pattern a mt ctx vs
  NotPat :: Pattern a mt ctx '[] -> Pattern a mt ctx '[]
  PredicatePat :: (a -> HList ctx -> Bool) -> Pattern a mt ctx '[]

data List a = List a
data Multiset a = Multiset a

class CollectionPatL mt a where
  nilPatL       :: a ~ [b] => Pattern a mt ctx '[]
  consPatL      :: a ~ [b] => mt ~ Matcher (List m) => Pattern b (Matcher m) ctx xs -> Pattern a mt (ctx :++: xs) ys -> Pattern a mt ctx (xs :++: ys)
  joinPatL      :: a ~ [b] => Pattern a mt ctx xs -> Pattern a mt (ctx :++: xs) ys -> Pattern a mt ctx (xs :++: ys)

class CollectionPatM mt a where
  nilPatM       :: a ~ [b] => Pattern a mt ctx '[]
  consPatM      :: a ~ [b] => mt ~ Matcher (Multiset m) => Pattern b (Matcher m) ctx xs -> Pattern a mt (ctx :++: xs) ys -> Pattern a mt ctx (xs :++: ys)
