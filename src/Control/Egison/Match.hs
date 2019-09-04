{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Control.Egison.Match (
  matchAll,
  match,
  processMStatesAll,
  ) where

import           Control.Egison.Core
import           Unsafe.Coerce

--
-- Pattern-matching algorithm
--

matchAll :: a -> Matcher m -> PList a m b -> [b]
matchAll tgt (Matcher m) PNil = []
matchAll tgt (Matcher m) (PCons (pat, f) ps) =
  let results = processMStatesAll [[MState HNil (MCons (MAtom pat tgt m) MNil)]] in
  map f results ++ (matchAll tgt (Matcher m) ps)

match :: a -> Matcher m -> PList a m b -> b
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
     extractMatches' (xs ++ [rs], ys ++ [states]) rest
   extractMatches' (xs, ys) (stream:rest) = extractMatches' (xs, ys ++ [stream]) rest

processMStates :: [MState vs] -> [[MState vs]]
processMStates []          = []
processMStates (mstate:ms) = [processMState mstate, ms]

processMState :: MState vs -> [MState vs]
processMState (MState rs MNil) = [MState rs MNil]
processMState (MState rs (MCons (MAtom pat tgt m) atoms)) =
  case pat of
    Pattern f ->
      let matomss = f tgt rs m in
      map (\newAtoms -> MState rs (MJoin newAtoms atoms)) matomss

    Wildcard -> [MState rs atoms]
    PatVar _ -> [unsafeCoerce $ MState (happend rs (HCons tgt HNil)) atoms]
    ValuePat f -> if f rs == tgt then [MState rs atoms] else []
    AndPat p1 p2 ->
      [unsafeCoerce $ MState rs (MCons (MAtom p1 tgt m) (MCons (MAtom p2 tgt m) $ unsafeCoerce atoms))]
    OrPat p1 p2 ->
      [MState rs (MCons (MAtom p1 tgt m) atoms), MState rs (MCons (MAtom p2 tgt m) atoms)]
    NotPat p ->
      [MState rs atoms | null $ processMStatesAll [[MState rs $ MCons (MAtom p tgt m) MNil]]]
    PredicatePat f -> [MState rs atoms | f rs tgt]
processMState (MState rs (MJoin MNil matoms2)) = processMState (MState rs matoms2)
processMState (MState rs (MJoin matoms1 matoms2)) =
  let mstates = processMState (MState rs matoms1) in
  map (\(MState rs' ms) -> unsafeCoerce $ MState rs' $ MJoin ms matoms2) mstates
