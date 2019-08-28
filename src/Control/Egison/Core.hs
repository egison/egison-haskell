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
  happend,
  Pattern(..),
  BasePat(..),
  CollectionPat(..),
  ) where

import           Data.Maybe

--
-- Matching states
--

data MState = forall xs. MState [MAtom] (HList xs)
-- data MState vs = forall vs' vs''. vs = vs' ++ vs'' => MState (MAtoms vs') (HList vs'')
data MAtom = forall a m vs. MAtom (Pattern a m vs) a
data Matcher a = Matcher a

data HList xs where
    HNil :: HList '[]
    HCons :: a -> HList as -> HList (a ': as)

happend :: HList as -> HList bs -> HList (as :++: bs)
happend (HCons x xs) ys = HCons x $ happend xs ys
happend HNil ys         = ys

type family as :++: bs :: [*] where
  (a ': as') :++: bs = a ': (as' :++: bs)
  '[] :++: bs = bs

--
-- Patterns
--

data Pattern a m vs = forall x. Pattern (a -> ([[MAtom]], Maybe x))

class BasePat a m where
  wildcard :: Pattern a m '[]
  patVar :: String -> Pattern a m '[a]
  -- valuePat :: forall vs. Eq a => (HList vs -> a) -> Pattern a m '[]
  valuePat' :: Eq a => a -> Pattern a m '[]

class CollectionPat a m where
  nilPat       :: a ~ [b] => Pattern a m '[]
  consPat      :: a ~ [b] => Pattern b m xs -> Pattern a m ys -> Pattern a m (xs :++: ys)
  -- joinPat      :: a ~ [b] => Pattern a m xs -> Pattern a m ys -> Pattern a m (xs :++: ys)

-- data Pattern a where
--   AndPat       :: Pattern a -> Pattern a -> Pattern a
--   OrPat        :: Pattern a -> Pattern a -> Pattern a
--   NotPat       :: Pattern a -> Pattern a
--   LaterPat     :: Pattern a -> Pattern a
--   PredicatePat :: (a -> Bool) -> Pattern a
--
--   UserPat      :: String -> [Pat] -> Pattern a
