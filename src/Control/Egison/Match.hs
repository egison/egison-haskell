{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Control.Egison.Match (
  matchAll,
  match,
  matchAllDFS,
  matchDFS,
  ) where

import           Control.Egison.Core
import           Unsafe.Coerce

matchAll :: (Matcher m a) => a -> m -> [MatchClause a m b] -> [b]
matchAll tgt m [] = []
matchAll tgt m ((MatchClause pat f):cs) =
  let results = processMStatesAll [[MState HNil (MCons (MAtom pat m tgt) MNil)]] in
  map f results ++ matchAll tgt m cs

match :: (Matcher m a) => a -> m -> [MatchClause a m b] -> b
match tgt m cs = head $ matchAll tgt m cs

matchAllDFS :: (Matcher m a) => a -> m -> [MatchClause a m b] -> [b]
matchAllDFS tgt m [] = []
matchAllDFS tgt m ((MatchClause pat f):cs) =
  let results = processMStatesAllDFS [MState HNil (MCons (MAtom pat m tgt) MNil)] in
  map f results ++ matchAllDFS tgt m cs

matchDFS :: (Matcher m a) => a -> m -> [MatchClause a m b] -> b
matchDFS tgt m cs = head $ matchAllDFS tgt m cs

--
-- Pattern-matching algorithm
--

processMStatesAllDFS :: [MState vs] -> [HList vs]
processMStatesAllDFS [] = []
processMStatesAllDFS (MState rs MNil:ms) = rs:(processMStatesAllDFS ms)
processMStatesAllDFS (mstate:ms) = processMStatesAllDFS $ (processMState mstate) ++ ms

processMStatesAll :: [[MState vs]] -> [HList vs]
processMStatesAll [] = []
processMStatesAll streams =
  case extractMatches $ concatMap processMStates streams of
    ([], streams') -> processMStatesAll streams'
    (results, streams') -> results ++ processMStatesAll streams'

extractMatches :: [[MState vs]] -> ([HList vs], [[MState vs]])
extractMatches = extractMatches' ([], [])
 where
   extractMatches' :: ([HList vs], [[MState vs]]) -> [[MState vs]] -> ([HList vs], [[MState vs]])
   extractMatches' (xs, ys) [] = (reverse xs,  reverse ys) -- These calls of the reverse function are very important for performance.
   extractMatches' (xs, ys) ((MState rs MNil:[]):rest) = extractMatches' (rs:xs, ys) rest
   extractMatches' (xs, ys) (stream:rest) = extractMatches' (xs, stream:ys) rest

processMStates :: [MState vs] -> [[MState vs]]
processMStates []          = []
processMStates (mstate:ms) = [processMState mstate, ms]

processMState :: MState vs -> [MState vs]
processMState (MState rs (MCons (MAtom pat m tgt) atoms)) =
  case pat of
    Pattern f ->
      let matomss = f rs m tgt in
      map (\newAtoms -> MState rs (MJoin newAtoms atoms)) matomss
    Wildcard -> [MState rs atoms]
    PatVar _ -> [unsafeCoerce $ MState (happend rs (HCons tgt HNil)) atoms]
    AndPat p1 p2 ->
      [unsafeCoerce $ MState rs (MCons (MAtom p1 m tgt) (MCons (MAtom p2 m tgt) $ unsafeCoerce atoms))]
    OrPat p1 p2 ->
      [MState rs (MCons (MAtom p1 m tgt) atoms), MState rs (MCons (MAtom p2 m tgt) atoms)]
    NotPat p ->
      [MState rs atoms | null $ processMStatesAll [[MState rs $ MCons (MAtom p m tgt) MNil]]]
    PredicatePat f -> [MState rs atoms | f rs tgt]
processMState (MState rs (MJoin MNil matoms2)) = processMState (MState rs matoms2)
processMState (MState rs (MJoin matoms1 matoms2)) =
  let mstates = processMState (MState rs matoms1) in
  map (\(MState rs' ms) -> unsafeCoerce $ MState rs' $ MJoin ms matoms2) mstates
processMState (MState rs MNil) = [MState rs MNil] -- TODO: shold not reach here but reaches here.
