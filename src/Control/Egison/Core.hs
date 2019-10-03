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
  mappend,
  -- Heterogeneous list
  HList(..),
  happend,
  (:++:),
  ) where

import           Prelude hiding (mappend)
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

mappend :: MList ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)
mappend MNil atoms = atoms
mappend (MCons atom atoms1) atoms2 =
  case mconsAssocProof atom atoms1 of
    Refl -> case mappendAssocProof atom atoms1 atoms2 of
      Refl -> MCons atom (mappend atoms1 atoms2)

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
happend xs@(HCons x xs') ys = case hconsAssocProof x xs' ys of
                                Refl -> HCons x $ happend xs' ys

hconsAssocProof :: a -> HList as -> HList bs -> ((a ': as) :++: bs) :~: (a ': (as :++: bs))
hconsAssocProof _ _ HNil = Refl
hconsAssocProof x xs (HCons y ys) = Refl

mconsAssocProof :: MAtom ctx vs -> MList (ctx :++: vs) vs' -> (ctx :++: (vs :++: vs')) :~: ((ctx :++: vs) :++: vs')
mconsAssocProof _ _ = unsafeCoerce Refl -- Todo: Write proof.

mappendAssocProof :: MAtom ctx xs -> MList (ctx :++: xs) ys ->  MList (ctx :++: xs :++: ys) zs -> (xs :++: (ys :++: zs)) :~: ((xs :++: ys) :++: zs)
mappendAssocProof _ _ = unsafeCoerce Refl -- Todo: Write proof.
