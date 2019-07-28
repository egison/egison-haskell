{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module Control.Egison.Core (
  MState(..),
  MAtom(..),
  Result(..),
  Matcher(..),
  something,
  Pat(..),
  Pattern(..),
  ) where

--
-- Matching states
--

data MState = MState [MAtom] [Result]
data MAtom = forall a. MAtom (Pattern a) (Matcher a) a
data Result = forall a. Result a
data Matcher a = Something | Matcher (Pattern a -> a -> [[MAtom]])

something :: Matcher a
something = Something

--
-- Patterns
--

data Pat = forall a. Pat (Pattern a)

data Pattern a where
  Wildcard     :: Pattern a
  PatVar       :: String -> Pattern a
  ValuePat     :: Eq a => ([Result] -> a) -> Pattern a
  ValuePat'    :: Eq a => a -> Pattern a

  AndPat       :: Pattern a -> Pattern a -> Pattern a
  OrPat        :: Pattern a -> Pattern a -> Pattern a
  NotPat       :: Pattern a -> Pattern a
  LaterPat     :: Pattern a -> Pattern a
  PredicatePat :: (a -> Bool) -> Pattern a

  NilPat       :: a ~ [b] => Pattern a
  ConsPat      :: a ~ [b] => Pattern b -> Pattern a -> Pattern a
  JoinPat      :: a ~ [b] => Pattern a -> Pattern a -> Pattern a

  UserPat      :: String -> [Pat] -> Pattern a
