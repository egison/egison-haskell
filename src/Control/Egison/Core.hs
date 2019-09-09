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
  (.*.),
  happend,
  (:++:),
  Pattern(..),
  ValuePat(..),
  Pair(..),
  PairPat(..),
  CollectionPat(..),
  ) where

import           Data.Maybe
import           Unsafe.Coerce

--
-- Matching states
--

data MState vs where
  MState :: vs ~ (xs :++: ys) => HList xs -> MList xs ys -> MState vs

data MAtom ctx vs = forall a m. MAtom (Pattern a ctx (Matcher m) vs) a m
newtype Matcher a = Matcher a

data HList xs where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

data MList ctx vs where
  MNil :: MList ctx '[]
  MCons :: MAtom ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)
  MJoin :: MList ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)

data PList a m b where
  PNil :: PList a m b
  PCons :: (Pattern a '[] (Matcher m) vs, HList vs -> b) -> PList a m b -> PList a m b

(.*.) :: (Pattern a '[] (Matcher m) vs, HList vs -> b) -> PList a m b -> PList a m b
x .*. xs = PCons x xs

infixr 0 .*.

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

data Pattern a ctx mt vs where
  Pattern :: mt ~ Matcher m => (a -> HList ctx -> m -> [MList ctx vs]) -> Pattern a ctx mt vs

  Wildcard :: Pattern a ctx mt '[]
  PatVar :: String -> Pattern a ctx mt '[a]
  AndPat :: Pattern a ctx mt vs -> Pattern a (ctx :++: vs) mt vs' -> Pattern a ctx mt (vs :++: vs')
  OrPat :: Pattern a ctx mt vs -> Pattern a ctx mt vs -> Pattern a ctx mt vs
  NotPat :: Pattern a ctx mt '[] -> Pattern a ctx mt '[]
  PredicatePat :: (HList ctx -> a -> Bool) -> Pattern a ctx mt '[]

class ValuePat mt a where
  valuePat :: Eq a => (HList ctx -> a) -> Pattern a ctx mt '[]

data Pair a b = Pair a b

class PairPat mt a where
  pairPat :: a ~ (b1, b2) => mt ~ Matcher (Pair m1 m2) => Pattern b1 ctx (Matcher m1) xs -> Pattern b2 (ctx :++: xs) (Matcher m2) ys -> Pattern a ctx mt (xs :++: ys)

class CollectionPat mt a where
  nilPat       :: a ~ [b] => Pattern a ctx mt '[]
  consPat      :: a ~ [b] => mt ~ Matcher (f m) => Pattern b ctx (Matcher m) xs -> Pattern a (ctx :++: xs) mt ys -> Pattern a ctx mt (xs :++: ys)
  joinPat      :: a ~ [b] => Pattern a ctx mt xs -> Pattern a (ctx :++: xs) mt ys -> Pattern a ctx mt (xs :++: ys)
