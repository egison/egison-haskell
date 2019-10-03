{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Control.Egison.Core (
  -- Pattern
  Pattern(..),
  Matcher(..),
  MatchClause(..),
  -- Matching state
  MState(..),
  MAtom(..),
  MList(..),
  -- Heterogeneous list
  HList(..),
  happend,
  (:++:),
  ) where

import           Data.Maybe
import           Data.Type.Equality
import           Unsafe.Coerce

---
--- Pattern
---

-- a: the type of the target
-- m: a matcher passed to the pattern
-- ctx: the intermediate pattern-matching result
-- vs: the list of types bound to the pattern variables in the pattern.
data Pattern a m ctx vs where
  Wildcard :: Pattern a m ctx '[]
  PatVar :: String -> Pattern a m ctx '[a]
  AndPat :: Pattern a m ctx vs -> Pattern a m (ctx :++: vs) vs' -> Pattern a m ctx (vs :++: vs')
  OrPat  :: Pattern a m ctx vs -> Pattern a m ctx vs -> Pattern a m ctx vs
  NotPat :: Pattern a m ctx '[] -> Pattern a m ctx '[]
  PredicatePat :: (HList ctx -> a -> Bool) -> Pattern a m ctx '[]
  -- User-defined pattern; pattern is a function that takes a target, an intermediate pattern-matching result, and a matcher and returns a list of lists of matching atoms.
  Pattern :: Matcher m a => (HList ctx -> m -> a -> [MList ctx vs]) -> Pattern a m ctx vs

class Matcher m a

data MatchClause a m b = forall vs. (Matcher m a) => MatchClause (Pattern a m '[] vs) (HList vs -> b)

---
--- Matching state
---

data MState vs where
  MState :: vs ~ (xs :++: ys) => HList xs -> MList xs ys -> MState vs

-- matching atom
-- ctx: intermediate pattern-matching results
-- vs: list of types bound to the pattern variables in the pattern.
data MAtom ctx vs = forall a m. (Matcher m a) => MAtom (Pattern a m ctx vs) m a

-- stack of matching atoms
data MList ctx vs where
  MNil :: MList ctx '[]
  MCons :: MAtom ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)
  MJoin :: MList ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)

---
--- Heterogeneous list
---

data HList xs where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

type family (as ::[*]) :++: (bs :: [*]) :: [*] where
  as :++: '[] = as
  '[] :++: bs = bs
  (a ': as) :++: bs = a ': (as :++: bs)

happend :: HList as -> HList bs -> HList (as :++: bs)
happend HNil ys         = ys
happend xs@(HCons x xs') ys = case consAssoc x xs' ys of
                                Refl -> HCons x $ happend xs' ys

consAssoc :: a -> HList as -> HList bs -> ((a ': as) :++: bs) :~: (a ': (as :++: bs))
consAssoc _ _ HNil = Refl
consAssoc x xs (HCons y ys) = Refl

appendAssoc :: HList as -> HList bs -> HList cs -> ((as :++: bs) :++: cs) :~: (as :++: (bs :++: cs))
appendAssoc HNil _ _ = Refl
appendAssoc (HCons _ xs) ys zs = case appendAssoc xs ys zs of
                                   Refl -> unsafeCoerce Refl -- TODO: ?
