{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}

import           Control.Exception.Assert
import           Data.Dynamic
import           Data.Maybe
import           Data.Numbers.Primes
import           Prelude

data MState = MState [MAtom] [Result]
data MAtom = forall a. (Typeable a) => MAtom (Pattern a) (Matcher a) a
type Result = Dynamic
data Matcher a = Something | Matcher (Pattern a -> a -> [[MAtom]])

--
-- pattern
--

data family Pattern a

data instance Pattern a :: * where
  Wildcard  :: Pattern a
  PatVar    :: String -> Pattern a
  LambdaPat :: Eq a => ([Result] -> a) -> Pattern a
  ValuePat  :: Eq a => a -> Pattern a
  AndPat :: Pattern a -> Pattern a -> Pattern a
  OrPat :: Pattern a -> Pattern a -> Pattern a
  NotPat :: Pattern a -> Pattern a
  LaterPat :: Pattern a -> Pattern a

  NilPat :: a ~ [b] => Pattern a
  ConsPat :: a ~ [b] => Pattern b -> Pattern a -> Pattern a
  JoinPat :: a ~ [b] => Pattern a -> Pattern a -> Pattern a

--
-- matcher
--

something :: Matcher a
something = Something

eqM :: (Typeable a, Eq a) => Matcher a
eqM = Matcher eqM'

eqM' :: (Typeable a, Eq a) => Pattern a -> a -> [[MAtom]]
eqM' p@Wildcard t = [[MAtom p something t]]
eqM' p@(PatVar _) t   = [[MAtom p something t]]
eqM' (ValuePat v) t   = [[] | v == t]

integer = eqM

list :: (Typeable a, Eq a) => Matcher a -> Matcher [a]
list m = Matcher (list' m)

list' :: (Typeable a, Eq a) => Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
list' _ p@Wildcard t = [[MAtom p something t]]
list' _ p@(PatVar _) t = [[MAtom p something t]]
list' _ (ValuePat v) t = [[] | v == t]
list' _ NilPat t = [[] | null t]
list' _ (ConsPat _ _) [] = []
list' m (ConsPat p1 p2) (t:ts) = [[MAtom p1 m t, MAtom p2 (list m) ts]]
list' m (JoinPat p1 p2) t = map (\(hs, ts) -> [MAtom p1 (list m) hs, MAtom p2 (list m) ts]) (unjoin t)

multiset :: (Typeable a, Eq a) => Matcher a -> Matcher [a]
multiset m = Matcher (multiset' m)

multiset' :: (Typeable a, Eq a) => Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
multiset' _ p@Wildcard t = [[MAtom p something t]]
multiset' _ p@(PatVar _) t = [[MAtom p something t]]
multiset' _ (ValuePat v) t = [[] | v == t]
multiset' _ NilPat t = [[] | null t]
multiset' m (ConsPat p1 p2) t =
  map (\(x, xs) -> [MAtom p1 m x, MAtom p2 (multiset m) xs])
    (matchAll t (list m)
      [(JoinPat (PatVar "hs") (ConsPat (PatVar "x") (PatVar "ts")), \[hs, x, ts] -> let x' = fromJust $ fromDynamic x in let hs' = fromJust $ fromDynamic hs in let ts' = fromJust $ fromDynamic ts in (x', hs' ++ ts'))])

unjoin :: [a] -> [([a], [a])]
unjoin []     = [([], [])]
unjoin (x:xs) = ([], x:xs) : map (\(hs,ts) -> (x:hs, ts)) (unjoin xs)

--
-- match
--

processMStates :: [MState] -> [[Result]]
processMStates [] = []
processMStates (MState [] results:rs) = results:processMStates rs
processMStates (mstate:rs) = processMStates (processMState mstate ++ rs)

processMState :: MState -> [MState]
processMState (MState (MAtom Wildcard something t:atoms) rs) = [MState atoms rs]
processMState (MState (MAtom (PatVar _) something t:atoms) rs) = [MState atoms (rs ++ [toDyn t])]
processMState (MState (MAtom (LambdaPat f) (Matcher m) t:atoms) rs) =
  let next = m (ValuePat $ f rs) t in
      map (\nt -> MState (nt ++ atoms) rs) next
processMState (MState (MAtom (AndPat p1 p2) m t:atoms) rs) = [MState (MAtom p1 m t:MAtom p2 m t:atoms) rs]
processMState (MState (MAtom (OrPat p1 p2) m t:atoms) rs) = [MState (MAtom p1 m t:atoms) rs, MState (MAtom p2 m t:atoms) rs]
processMState (MState (MAtom (NotPat p) m t:atoms) rs) = [MState atoms rs | null $ processMStates [MState [MAtom p m t] rs]]
processMState (MState (MAtom (LaterPat p) m t:atoms) rs) = [MState (atoms ++ [MAtom p m t]) rs]
processMState (MState (MAtom p (Matcher m) t:atoms) rs) =
  map (\newAtoms -> MState (newAtoms ++ atoms) rs) (m p t)

matchAll :: (Typeable a) => a -> Matcher a -> [(Pattern a, [Result] -> b)] -> [b]
matchAll tgt matcher =
  foldr (\(pat, f) matches ->
    let resultss = processMStates [MState [MAtom pat matcher tgt] []] in
        map f resultss ++ matches) []

match :: (Typeable a) => a -> Matcher a -> [(Pattern a, [Result] -> b)] -> b
match t m xs = head $ matchAll t m xs


main :: IO ()
main = do
  let pat1 = Wildcard :: Pattern Integer
  let pat2 = ConsPat Wildcard Wildcard :: Pattern [Integer]
  let pat3 = ConsPat (ConsPat Wildcard Wildcard) Wildcard :: Pattern [[Integer]]
  let patTwinPrimes = JoinPat Wildcard (ConsPat (PatVar "p") (ConsPat (LambdaPat (\rs -> case map fromDynamic rs of [Just p] -> p + 2)) Wildcard)) :: Pattern [Integer]

  -- list
  let xss0 = match [1,2,5,9,4] (list integer) [(ConsPat (PatVar "x") (PatVar "xs"), \[x, xs] -> (fromJust $ fromDynamic x :: Integer, fromJust $ fromDynamic xs :: [Integer]))]
  assert (xss0 == (1, [2,5,9,4])) $ print "ok 1"

  -- infinite target
  let twinprimes = matchAll primes (list integer) [(patTwinPrimes, \[p] -> let p' = fromJust $ fromDynamic p :: Integer in (p', p' + 2))]
  assert (take 10 twinprimes == [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]) $ print "ok 2"

  -- multiset
  let xss1 = matchAll [1,2,5,9,4] (multiset integer) [(ConsPat (PatVar "x") (PatVar "xs"), \[x, xs] -> (fromJust $ fromDynamic x :: Integer, fromJust $ fromDynamic xs :: [Integer]))]
  assert (xss1 == [(1,[2,5,9,4]),(2,[1,5,9,4]),(5,[1,2,9,4]),(9,[1,2,5,4]),(4,[1,2,5,9])]) $ print "ok 3"

  -- value, and, or, not pattern
  let xss2 = matchAll [1,2,5,9,4] (multiset integer) [(ConsPat (AndPat (NotPat (ValuePat 5)) (PatVar "x")) (ConsPat (AndPat (OrPat (ValuePat 1) (ValuePat 2)) (PatVar "y")) (PatVar "xs")), \[x, y, xs] -> (fromJust $ fromDynamic x :: Integer, fromJust $ fromDynamic y :: Integer, fromJust $ fromDynamic xs :: [Integer]))]
  assert (xss2 == [(1,2,[5,9,4]),(2,1,[5,9,4]),(9,1,[2,5,4]),(9,2,[1,5,4]),(4,1,[2,5,9]),(4,2,[1,5,9])]) $ print "ok 4"

  -- later pattern
  let xss3 = match [1..5] (list integer) [(ConsPat (LaterPat (LambdaPat (\rs -> case map fromDynamic rs of [Just p, _] -> p - 1))) (ConsPat (PatVar "x") (PatVar "xs")), \[x, xs] -> (fromJust $ fromDynamic x :: Integer, fromJust $ fromDynamic xs :: [Integer]))]
  assert (xss3 == (2,[3,4,5])) $ print "ok 5"

  return ()
