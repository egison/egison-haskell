{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
-- {-# LANGUAGE RankNTypes #-}

module Control.Egison.Core (
  MState(..),
  MAtom(..),
  Matcher(..),
  HList(..),
  MList(..),
  MatchClause(..),
  happend,
  (:++:),
  Pattern(..),
  ) where

import           Data.Maybe

--
-- Matching states
--

data MState vs where
  MState :: vs ~ (xs :++: ys) => HList xs -> MList xs ys -> MState vs

-- ctx: intermediate pattern-matching results, vs: list of types bound to the pattern variables in the pattern.
data MAtom ctx vs = forall a m. MAtom (Pattern a ctx (Matcher m) vs) a m

newtype Matcher a = Matcher a

data HList xs where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

-- List of matching atoms
data MList ctx vs where
  MNil :: MList ctx '[]
  MCons :: MAtom ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)
  MJoin :: MList ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)

data MatchClause a m b = forall vs. MatchClause (Pattern a '[] (Matcher m) vs) (HList vs -> b)

happend :: HList as -> HList bs -> HList (as :++: bs)
happend (HCons x xs) ys = case proof x xs ys of Refl -> HCons x $ happend xs ys
happend HNil ys         = ys

type family as :++: bs :: [*] where
  bs :++: '[] = bs
  '[] :++: bs = bs
  (a ': as) :++: bs = a ': (as :++: bs)

data (a :: [*]) :~: (b :: [*]) where
  Refl :: a :~: a

proof :: a -> HList as -> HList bs -> ((a ': as) :++: bs) :~: (a ': (as :++: bs))
proof _ _ HNil = Refl
proof x xs (HCons y ys) = Refl

--
-- Pattern
--

data Pattern a ctx mt vs where
  Pattern :: mt ~ Matcher m => (a -> HList ctx -> m -> [MList ctx vs]) -> Pattern a ctx mt vs

  Wildcard :: Pattern a ctx mt '[]
  PatVar :: String -> Pattern a ctx mt '[a]
  AndPat :: Pattern a ctx mt vs -> Pattern a (ctx :++: vs) mt vs' -> Pattern a ctx mt (vs :++: vs')
  OrPat  :: Pattern a ctx mt vs -> Pattern a ctx mt vs -> Pattern a ctx mt vs
  NotPat :: Pattern a ctx mt '[] -> Pattern a ctx mt '[]
  PredicatePat :: (HList ctx -> a -> Bool) -> Pattern a ctx mt '[]
