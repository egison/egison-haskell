{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeFamilies              #-}


module Control.Egison.Core (
  MState(..),
  MAtom(..),
  Result(..),
  Matcher(..),
  Pattern(..),
  Pattern'(..),
  something,
  processMStatesAll,
             ) where

import           Data.List
import           Data.Maybe
import           Prelude

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

data Pattern' = forall a. Pattern' (Pattern a)

data Pattern a where
  Wildcard  :: Pattern a
  PatVar    :: String -> Pattern a
  ValuePat :: Eq a => ([Result] -> a) -> Pattern a
  ValuePat'  :: Eq a => a -> Pattern a

  AndPat :: Pattern a -> Pattern a -> Pattern a
  OrPat :: Pattern a -> Pattern a -> Pattern a
  NotPat :: Pattern a -> Pattern a
  LaterPat :: Pattern a -> Pattern a
  PredicatePat :: (a -> Bool) -> Pattern a

  NilPat :: a ~ [b] => Pattern a
  ConsPat :: a ~ [b] => Pattern b -> Pattern a -> Pattern a
  JoinPat :: a ~ [b] => Pattern a -> Pattern a -> Pattern a

  UserPat :: String -> [Pattern'] -> Pattern a

--
-- Pattern-matching algorithm
--

processMStatesAll :: [[MState]] -> [[Result]]
processMStatesAll [] = []
processMStatesAll streams = let (results, streams') = extractResults $ concatMap processMStates streams in results ++ processMStatesAll streams'

extractResults :: [[MState]] -> ([[Result]], [[MState]])
extractResults = foldr extractResults' ([], [])
 where
   extractResults' :: [MState] -> ([[Result]], [[MState]]) -> ([[Result]], [[MState]])
   extractResults' [] (rss, mss) = (rss, mss)
   extractResults' (MState [] rs:ms) (rss, mss) = extractResults' ms (rs:rss, mss)
   extractResults' ms (rss, mss) = (rss, ms:mss)

processMStates :: [MState] -> [[MState]]
processMStates []          = []
processMStates (mstate:ms) = [processMState mstate, ms]

processMState :: MState -> [MState]
processMState (MState (MAtom Wildcard something t:atoms) rs) = [MState atoms rs]
processMState (MState (MAtom (PatVar _) something t:atoms) rs) = [MState atoms (rs ++ [Result t])]
processMState (MState (MAtom (ValuePat f) (Matcher m) t:atoms) rs) =
  let next = m (ValuePat' $ f rs) t in
      map (\nt -> MState (nt ++ atoms) rs) next
processMState (MState (MAtom (AndPat p1 p2) m t:atoms) rs) = [MState (MAtom p1 m t:MAtom p2 m t:atoms) rs]
processMState (MState (MAtom (OrPat p1 p2) m t:atoms) rs) = [MState (MAtom p1 m t:atoms) rs, MState (MAtom p2 m t:atoms) rs]
processMState (MState (MAtom (NotPat p) m t:atoms) rs) = [MState atoms rs | null $ processMStatesAll [[MState [MAtom p m t] rs]]]
processMState (MState (MAtom (LaterPat p) m t:atoms) rs) = [MState (atoms ++ [MAtom p m t]) rs]
processMState (MState (MAtom (PredicatePat f) _ t:atoms) rs) = [MState atoms rs | f t]
processMState (MState (MAtom p (Matcher m) t:atoms) rs) =
  map (\newAtoms -> MState (newAtoms ++ atoms) rs) (m p t)
