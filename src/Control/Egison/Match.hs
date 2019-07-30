module Control.Egison.Match (
  matchAll,
  match,
  processMStatesAll,
  ) where

import           Control.Egison.Core

--
-- Pattern-matching algorithm
--

matchAll :: a -> Matcher a -> [(Pattern a, [Result] -> b)] -> [b]
matchAll tgt matcher =
  foldr (\(pat, f) matches ->
    map f (processMStatesAll [[MState [MAtom pat matcher tgt] []]]) ++ matches) []

match :: a -> Matcher a -> [(Pattern a, [Result] -> b)] -> b
match t m xs = head $ matchAll t m xs

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
