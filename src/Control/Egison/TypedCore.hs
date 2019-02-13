{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}


module Control.Egison.TypedCore (
--  Pattern(..),
             ) where

import           Data.HList.HList
import           Data.List
import           Data.Maybe
import           Prelude

--
-- Matching states
--

--data MState l = MState [MAtom] (HList l)
--data MAtom = forall a. MAtom (Pattern a) (Matcher a) a
--data Matcher a = Something | Matcher (Pattern a (HList l1) (HList l2) -> a -> [[MAtom]])

--something :: Matcher a
--something = Something

--
-- Patterns
--

data Pattern a l1 l2 where
  Wildcard :: l1 ~ l2 => Pattern a (HList l1) (HList l2)
  PatVar   :: (a ': l1) ~ l2 => String -> Pattern a (HList l1) (HList l2)
  ValuePat :: (l1 ~ l2, Eq a) => (HList l1 -> a) -> Pattern a (HList l1) (HList l2)

  NilPat :: (a ~ [b], l1 ~ l2) => Pattern a (HList l1) (HList l2)
  ConsPat :: a ~ [b] => Pattern b (HList l1) (HList l3) -> Pattern a (HList l3) (HList l2) -> Pattern a (HList l1) (HList l2)
  JoinPat :: a ~ [b] => Pattern a (HList l1) (HList l3) -> Pattern a (HList l3) (HList l2) -> Pattern a (HList l1) (HList l2)
