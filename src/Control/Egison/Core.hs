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
  PList(..),
  ps,
  happend,
  (:++:),
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

data MAtom ctx vs = forall a m. MAtom (Pattern a ctx (Matcher m) vs) a m
data Matcher a = Matcher a

data HList xs where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

data MList ctx vs where
  MNil :: MList ctx '[]
  MCons :: MAtom ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)
  MJoin :: MList ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)

data PList a m b where
  PNil :: PList a m b
  PCons :: (Pattern a '[] (Matcher m) vs, (HList vs) -> b) -> PList a m b -> PList a m b

ps :: (Pattern a '[] (Matcher m) vs, (HList vs) -> b) -> PList a m b
ps x = PCons x PNil

happend :: HList as -> HList bs -> HList (as :++: bs)
happend (HCons x xs) ys = unsafeCoerce $ HCons x $ happend xs ys
happend HNil ys         = ys

type family as :++: bs :: [*] where
  bs :++: '[] = bs
  '[] :++: bs = bs
  (a ': as') :++: bs = a ': (as' :++: bs)

--
-- Patterns
--

data Unit = Unit

data Pattern a ctx mt vs where
  Pattern :: mt ~ Matcher m => (a -> HList ctx -> m -> [MList ctx vs]) -> Pattern a ctx mt vs

  Wildcard :: Pattern a ctx mt '[]
  PatVar :: String -> Pattern a ctx mt '[a]
  ValuePat :: Eq a => (HList ctx -> a) -> Pattern a ctx mt '[]
  AndPat :: Pattern a ctx mt vs -> Pattern a (ctx :++: vs) mt vs' -> Pattern a ctx mt (vs :++: vs')
  OrPat :: Pattern a ctx mt vs -> Pattern a ctx mt vs -> Pattern a ctx mt vs
  NotPat :: Pattern a ctx mt '[] -> Pattern a ctx mt '[]
  PredicatePat :: (HList ctx -> a -> Bool) -> Pattern a ctx mt '[]

data List a = List a
data Multiset a = Multiset a

class CollectionPatL mt a where
  nilPatL       :: a ~ [b] => Pattern a ctx mt '[]
  consPatL      :: a ~ [b] => mt ~ Matcher (List m) => Pattern b ctx (Matcher m) xs -> Pattern a (ctx :++: xs) mt ys -> Pattern a ctx mt (xs :++: ys)
  joinPatL      :: a ~ [b] => Pattern a ctx mt xs -> Pattern a (ctx :++: xs) mt ys -> Pattern a ctx mt (xs :++: ys)

class CollectionPatM mt a where
  nilPatM       :: a ~ [b] => Pattern a ctx mt '[]
  consPatM      :: a ~ [b] => mt ~ Matcher (Multiset m) => Pattern b ctx (Matcher m) xs -> Pattern a (ctx :++: xs) mt ys -> Pattern a ctx mt (xs :++: ys)
