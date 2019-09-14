{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Control.Egison.Match (
  matchAll,
  match,
  ) where

import           Control.Egison.Core

--
-- Pattern-matching algorithm
--

matchAll :: a -> Matcher m -> PList a m b -> [b]
matchAll tgt (Matcher m) PNil = []
matchAll tgt (Matcher m) (PCons (pat, f) ps) =
  let results = processMStatesAll [[MState (MCons (MAtom pat tgt m) MNil) HNil]] in
  map f results ++ matchAll tgt (Matcher m) ps

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
   extractMatches' (xs, ys) ((MState MNil rs:states):rest) =
     extractMatches' (xs ++ [rs], ys ++ [states]) rest
   extractMatches' (xs, ys) (stream:rest) = extractMatches' (xs, ys ++ [stream]) rest

processMStates :: [MState vs] -> [[MState vs]]
processMStates []          = []
processMStates (mstate:ms) = [processMState mstate, ms]

processMState :: MState vs -> [MState vs]
processMState (MState MNil rs) = [MState MNil rs]
processMState (MState (MCons (MAtom pat tgt m) atoms) rs) =
  case pat of
    Pattern f ->
      let matomss = f tgt rs m in
      map (\newAtoms -> MState (MJoin newAtoms atoms) rs) matomss
    Wildcard -> [MState atoms rs]
    PatVar _ -> case proof2 rs (hsingle tgt) (mlToHList atoms) of Refl -> [MState atoms (happend rs (HCons tgt HNil))]
    AndPat p1 p2 ->
      case proof2 rs (maToHList $ MAtom p1 tgt m) (maToHList $ MAtom p2 tgt m) of
        Refl -> case proof2 (maToHList $ MAtom p1 tgt m) (maToHList $ MAtom p2 tgt m) (mlToHList atoms) of
                  Refl -> [MState (MCons (MAtom p1 tgt m) (MCons (MAtom p2 tgt m) atoms)) rs]
    OrPat p1 p2 ->
      [MState (MCons (MAtom p1 tgt m) atoms) rs, MState (MCons (MAtom p2 tgt m) atoms) rs]
    NotPat p ->
      [MState atoms rs | null $ processMStatesAll [[MState (MCons (MAtom p tgt m) MNil) rs]]]
    PredicatePat f -> [MState atoms rs | f rs tgt]
processMState (MState (MJoin MNil matoms2) rs) = processMState (MState matoms2 rs)
processMState (MState (MJoin matoms1 matoms2) rs) =
  let mstates = processMState (MState matoms1 rs) in
  map (\(MState ms rs') -> case proof2 rs (mlToHList matoms1) (mlToHList matoms2) of
                             Refl -> case proof2 rs' (mlToHList ms) (mlToHList matoms2)  of
                                       Refl -> MState (MJoin ms matoms2) rs') mstates
