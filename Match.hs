{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeFamilies              #-}


module Match (
  MState(..),
  MAtom(..),
  Result(..),
  Matcher(..),
  Pattern(..),
  something,
  eql,
  list,
  multiset,
  integer,
  matchAll,
  match,
             ) where

import           Data.List
import           Data.Maybe
import           Prelude
import           Unsafe.Coerce

data MState = MState [MAtom] [Result]
data MAtom = forall a. MAtom (Pattern a) (Matcher a) a
data Result = forall a. Result a
data Matcher a = Something | Matcher (Pattern a -> a -> [[MAtom]])

--
-- Patterns
--

data family Pattern a

data instance Pattern a :: * where
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

--
-- Matchers
--

something :: Matcher a
something = Something

eql :: Eq a => Matcher a
eql = Matcher eql'

eql' :: Eq a => Pattern a -> a -> [[MAtom]]
eql' p@Wildcard t    = [[MAtom p something t]]
eql' p@(PatVar _) t  = [[MAtom p something t]]
eql' (ValuePat' v) t = [[] | v == t]

integer :: Eq a => Matcher a
integer = eql

list :: Matcher a -> Matcher [a]
list m = Matcher (list' m)

list' :: Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
list' _ p@Wildcard t = [[MAtom p something t]]
list' _ p@(PatVar _) t = [[MAtom p something t]]
list' _ NilPat t = [[] | null t]
list' _ (ConsPat _ _) [] = []
list' m (ConsPat p1 p2) (t:ts) = [[MAtom p1 m t, MAtom p2 (list m) ts]]
list' m (JoinPat p1 p2) t = map (\(hs, ts) -> [MAtom p1 (list m) hs, MAtom p2 (list m) ts]) (unjoin t)

multiset :: Matcher a -> Matcher [a]
multiset m = Matcher (multiset' m)

multiset' :: Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
multiset' _ p@Wildcard t = [[MAtom p something t]]
multiset' _ p@(PatVar _) t = [[MAtom p something t]]
multiset' _ NilPat t = [[] | null t]
multiset' m (ConsPat p1 p2) t =
  map (\(x, xs) -> [MAtom p1 m x, MAtom p2 (multiset m) xs])
    (matchAll t (list m)
      [(JoinPat (PatVar "hs") (ConsPat (PatVar "x") (PatVar "ts")), \[Result hs, Result x, Result ts] -> let x' = unsafeCoerce x in let hs' = unsafeCoerce hs in let ts' = unsafeCoerce ts in (x', hs' ++ ts'))])

unjoin :: [a] -> [([a], [a])]
unjoin []     = [([], [])]
unjoin (x:xs) = ([], x:xs) : map (\(hs,ts) -> (x:hs, ts)) (unjoin xs)

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

matchAll :: a -> Matcher a -> [(Pattern a, [Result] -> b)] -> [b]
matchAll tgt matcher =
  foldr (\(pat, f) matches ->
    map f (processMStatesAll [[MState [MAtom pat matcher tgt] []]]) ++ matches) []

match :: a -> Matcher a -> [(Pattern a, [Result] -> b)] -> b
match t m xs = head $ matchAll t m xs
