{-# LANGUAGE GADTs                     #-}
module Control.Egison.Match (
  matchAll,
  -- match,
  processMStatesAll,
  ) where

import           Control.Egison.Core
import           Unsafe.Coerce

--
-- Pattern-matching algorithm
--

matchAll :: a -> Matcher m -> [(Pattern a m vs, (HList vs) -> b)] -> [b]
matchAll tgt _ =
  foldr (\(pat, f) matches ->
    map f (processMStatesAll [[MState HNil (MSingle (MAtom pat tgt))]]) ++ matches) []
    -- map f (processMStatesAll [[MState [MAtom pat tgt] HNil]]) ++ matches) []

-- match :: a -> Matcher a -> [(Pattern a, HList vs -> b)] -> b
-- match t m xs = head $ matchAll t m xs
--
processMStatesAll :: [[MState vs]] -> [HList vs]
processMStatesAll [] = []
processMStatesAll streams =
  let (results, streams') = extractResults $ concatMap processMStates streams
   in results ++ processMStatesAll streams'

extractResults :: [[MState vs]] -> ([HList vs], [[MState vs]])
extractResults = foldr extractResults' ([], [])
 where
   extractResults' :: [MState vs] -> ([HList vs], [[MState vs]]) -> ([HList vs], [[MState vs]])
   extractResults' [] (rss, mss) = (rss, mss)
   extractResults' (MState rs MNil:ms) (rss, mss) =
     let rs' = unsafeCoerce rs in extractResults' ms (rs':rss, mss)
   extractResults' ms (rss, mss) = (rss, ms:mss)

processMStates :: [MState vs] -> [[MState vs]]
processMStates []          = []
processMStates (mstate:ms) = [processMState mstate, ms]

processMState :: MState vs -> [MState vs]
processMState (MState rs (MSingle (MAtom (Pattern f) tgt))) =
  let (matomss, mb) = f tgt in
  case mb of
    -- Unit -> mmap (\newAtoms -> MState rs newAtoms) matomss
    Unit -> g1 matomss
    x -> map (\newAtoms -> MState (happend rs (HCons x HNil)) newAtoms) matomss
processMState (MState rs (MCons (MAtom (Pattern f) tgt) atoms)) =
  let (matomss, mb) = f tgt in
  case mb of
    Unit -> mmap (\newAtoms -> MState rs (newAtoms ++ atoms)) matomss
    x -> map (\newAtoms -> MState (happend rs (HCons x HNil)) (newAtoms ++ atoms)) matomss
 where
   g1 ::
  mmap f MNil = MNil
  mmap f (MSingle matom) = MSingle $ f matom
  mmap f (MCons matom ms) = MCons (f matom) ms

-- processMState (MState (MAtom (valuePat f) (Matcher m) t:atoms) rs) =
--   let next = m (ValuePat' $ f rs) t in
--       map (\nt -> MState (nt ++ atoms) rs) next
-- processMState (MState (MAtom (AndPat p1 p2) m t:atoms) rs) = [MState (MAtom p1 m t:MAtom p2 m t:atoms) rs]
-- processMState (MState (MAtom (OrPat p1 p2) m t:atoms) rs) = [MState (MAtom p1 m t:atoms) rs, MState (MAtom p2 m t:atoms) rs]
-- processMState (MState (MAtom (NotPat p) m t:atoms) rs) = [MState atoms rs | null $ processMStatesAll [[MState [MAtom p m t] rs]]]
-- processMState (MState (MAtom (LaterPat p) m t:atoms) rs) = [MState (atoms ++ [MAtom p m t]) rs]
-- processMState (MState (MAtom (PredicatePat f) _ t:atoms) rs) = [MState atoms rs | f t]
