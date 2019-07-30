{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeOperators             #-}

module Control.Egison.Core (
  MState(..),
  MAtom(..),
  Matcher(..),
  Pattern(..),
  HList(..),
  ) where

--
-- Matching states
--

data MState a m vs = forall vs' vs''. MState [MAtom a m vs'] vs''
data MAtom a m vs = MAtom (Pattern a m vs) a
-- data Matcher a = Something | Matcher (Pattern a -> a -> [[MAtom]])
data Matcher a = Something | Matcher a

something :: Matcher a
something = Something

data HList xs where
    HNil :: HList '[]
    (:::) :: a -> HList as -> HList (a ': as)

infixr 6 :::
--
-- Patterns
--

newtype Pattern a m vars = Pattern { runPattern :: a -> Maybe vars }

class BasePat a m where
  wildcard :: Pattern a m (HList '[])
  -- patVar :: String -> Pattern a '[a]
  patVar :: String -> Pattern a m (HList '[a])
  valuePat :: forall vs. Eq a => (HList vs -> a) -> Pattern a m (HList '[])
  valuePat' :: Eq a => a -> Pattern a m (HList '[])

-- data Pat = forall a. Pat (Pattern a)
--
-- data Pattern a where
--   Wildcard     :: Pattern a
--   PatVar       :: String -> Pattern a
--   ValuePat     :: Eq a => ([Result] -> a) -> Pattern a
--   ValuePat'    :: Eq a => a -> Pattern a
--
--   AndPat       :: Pattern a -> Pattern a -> Pattern a
--   OrPat        :: Pattern a -> Pattern a -> Pattern a
--   NotPat       :: Pattern a -> Pattern a
--   LaterPat     :: Pattern a -> Pattern a
--   PredicatePat :: (a -> Bool) -> Pattern a
--
--   NilPat       :: a ~ [b] => Pattern a
--   ConsPat      :: a ~ [b] => Pattern b -> Pattern a -> Pattern a
--   JoinPat      :: a ~ [b] => Pattern a -> Pattern a -> Pattern a
--
--   UserPat      :: String -> [Pat] -> Pattern a
