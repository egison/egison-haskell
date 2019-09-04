{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
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

matchAll :: a -> Matcher m -> [(Pattern a (Matcher m) '[] vs, (HList vs) -> b)] -> [b]
matchAll tgt _ =
  foldr (\(pat, f) matches ->
    let results = processMStatesAll [[MState HNil (MSingle (MAtom pat tgt))]] in
    map f results ++ matches) []

match :: a -> Matcher m -> [(Pattern a (Matcher m) '[] vs, HList vs -> b)] -> b
match tgt m xs = head $ matchAll tgt m xs

processMStatesAll :: [[MState vs]] -> [HList vs]
processMStatesAll [] = []
processMStatesAll streams =
  let (results, streams') = extractMatches $ concatMap processMStates streams
   in results ++ processMStatesAll streams'

extractMatches :: [[MState vs]] -> ([HList vs], [[MState vs]])
extractMatches = extractMatches' ([], [])
 where
   extractMatches' :: ([HList vs], [[MState vs]]) -> [[MState vs]] -> ([HList vs], [[MState vs]])
   extractMatches' (xs, ys) [] = (xs, ys)
   extractMatches' (xs, ys) ((MState rs MNil:states):rest) =
     let rs' = unsafeCoerce rs in -- ok
     extractMatches' (xs ++ [rs'], ys ++ [states]) rest
   extractMatches' (xs, ys) (stream:rest) = extractMatches' (xs, ys ++ [stream]) rest

processMStates :: [MState vs] -> [[MState vs]]
processMStates []          = []
processMStates (mstate:ms) = [processMState mstate, ms]

processMState :: MState vs -> [MState vs]
processMState (MState rs MNil) = [MState rs MNil]
processMState (MState rs (MSingle (MAtom (Pattern f) tgt))) =
  let (matomss, _) = f tgt rs in
  map (\newAtoms -> MState rs newAtoms) matomss
processMState (MState rs (MSingle (MAtom (Pattern' f) tgt))) =
  let (matomss, x) = f tgt rs in
  map (\newAtoms -> unsafeCoerce $ MState (happend rs (HCons x HNil)) newAtoms) matomss
processMState (MState rs (MCons (MAtom (Pattern f) tgt) atoms)) =
  let (matomss, _) = f tgt rs in
  map (\newAtoms -> MState rs (MJoin newAtoms atoms)) matomss
processMState (MState rs (MCons (MAtom (Pattern' f) tgt) atoms)) =
  let (matomss, x) = f tgt rs in
  let atoms' = unsafeCoerce atoms in
  map (\newAtoms -> unsafeCoerce $ MState (happend rs (HCons x HNil)) (MJoin newAtoms atoms')) matomss
processMState (MState rs (MJoin MNil matoms2)) =
  let matoms2' = unsafeCoerce matoms2 in -- ok
  processMState (unsafeCoerce $ MState rs matoms2')
processMState (MState rs (MJoin matoms1 matoms2)) =
  let mstates = processMState (MState rs matoms1) in
  map (\(MState rs' ms) -> unsafeCoerce $ MState rs' $ MJoin ms matoms2) mstates

-- processMState (MState (MAtom (valuePat f) (Matcher m) t:atoms) rs) =
--   let next = m (ValuePat' $ f rs) t in
--       map (\nt -> MState (nt ++ atoms) rs) next
-- processMState (MState (MAtom (AndPat p1 p2) m t:atoms) rs) = [MState (MAtom p1 m t:MAtom p2 m t:atoms) rs]
-- processMState (MState (MAtom (OrPat p1 p2) m t:atoms) rs) = [MState (MAtom p1 m t:atoms) rs, MState (MAtom p2 m t:atoms) rs]
-- processMState (MState (MAtom (NotPat p) m t:atoms) rs) = [MState atoms rs | null $ processMStatesAll [[MState [MAtom p m t] rs]]]
-- processMState (MState (MAtom (LaterPat p) m t:atoms) rs) = [MState (atoms ++ [MAtom p m t]) rs]
-- processMState (MState (MAtom (PredicatePat f) _ t:atoms) rs) = [MState atoms rs | f t]
