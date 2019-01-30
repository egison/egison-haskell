{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeFamilies              #-}

import           Control.Exception.Assert
import           Data.List
import           Data.Maybe
import           Data.Numbers.Primes
import           Prelude
import           Unsafe.Coerce

data MState = MState [MAtom] [Result]
data MAtom = forall a. MAtom (Pattern a) (Matcher a) a
data Result = forall a. Result a
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

eqM :: Eq a => Matcher a
eqM = Matcher eqM'

eqM' :: Eq a => Pattern a -> a -> [[MAtom]]
eqM' p@Wildcard t   = [[MAtom p something t]]
eqM' p@(PatVar _) t = [[MAtom p something t]]
eqM' (ValuePat v) t = [[] | v == t]

integer = eqM

list :: Eq a => Matcher a -> Matcher [a]
list m = Matcher (list' m)

list' :: Eq a => Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
list' _ p@Wildcard t = [[MAtom p something t]]
list' _ p@(PatVar _) t = [[MAtom p something t]]
list' _ (ValuePat v) t = [[] | v == t]
list' _ NilPat t = [[] | null t]
list' _ (ConsPat _ _) [] = []
list' m (ConsPat p1 p2) (t:ts) = [[MAtom p1 m t, MAtom p2 (list m) ts]]
list' m (JoinPat p1 p2) t = map (\(hs, ts) -> [MAtom p1 (list m) hs, MAtom p2 (list m) ts]) (unjoin t)

multiset :: Eq a => Matcher a -> Matcher [a]
multiset m = Matcher (multiset' m)

multiset' :: Eq a => Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
multiset' _ p@Wildcard t = [[MAtom p something t]]
multiset' _ p@(PatVar _) t = [[MAtom p something t]]
multiset' _ (ValuePat v) t = [[] | v == t]
multiset' _ NilPat t = [[] | null t]
multiset' m (ConsPat p1 p2) t =
  map (\(x, xs) -> [MAtom p1 m x, MAtom p2 (multiset m) xs])
    (matchAll t (list m)
      [(JoinPat (PatVar "hs") (ConsPat (PatVar "x") (PatVar "ts")), \[Result hs, Result x, Result ts] -> let x' = unsafeCoerce x in let hs' = unsafeCoerce hs in let ts' = unsafeCoerce ts in (x', hs' ++ ts'))])

unjoin :: [a] -> [([a], [a])]
unjoin []     = [([], [])]
unjoin (x:xs) = ([], x:xs) : map (\(hs,ts) -> (x:hs, ts)) (unjoin xs)

--
-- match
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
processMState (MState (MAtom (LambdaPat f) (Matcher m) t:atoms) rs) =
  let next = m (ValuePat $ f rs) t in
      map (\nt -> MState (nt ++ atoms) rs) next
processMState (MState (MAtom (AndPat p1 p2) m t:atoms) rs) = [MState (MAtom p1 m t:MAtom p2 m t:atoms) rs]
processMState (MState (MAtom (OrPat p1 p2) m t:atoms) rs) = [MState (MAtom p1 m t:atoms) rs, MState (MAtom p2 m t:atoms) rs]
processMState (MState (MAtom (NotPat p) m t:atoms) rs) = [MState atoms rs | null $ processMStates [MState [MAtom p m t] rs]]
processMState (MState (MAtom (LaterPat p) m t:atoms) rs) = [MState (atoms ++ [MAtom p m t]) rs]
processMState (MState (MAtom p (Matcher m) t:atoms) rs) =
  map (\newAtoms -> MState (newAtoms ++ atoms) rs) (m p t)

matchAll :: a -> Matcher a -> [(Pattern a, [Result] -> b)] -> [b]
matchAll tgt matcher =
  foldr (\(pat, f) matches ->
    let resultss = processMStatesAll [[MState [MAtom pat matcher tgt] []]] in
        map f resultss ++ matches) []

match :: a -> Matcher a -> [(Pattern a, [Result] -> b)] -> b
match t m xs = head $ matchAll t m xs

--
-- my functions
--

mymap :: Eq a => (a -> b) -> [a] -> [b]
mymap f xs = matchAll xs (list something) [(JoinPat Wildcard (ConsPat (PatVar "x") Wildcard), \[Result x] -> let x' = unsafeCoerce x in f x')]

myconcat :: Eq a => [[a]] -> [a]
myconcat xs = matchAll xs (multiset (multiset something)) [(ConsPat (ConsPat (PatVar "x") Wildcard) Wildcard, \[Result x] -> let x' = unsafeCoerce x in x')]


main :: IO ()
main = do
  let pat1 = Wildcard :: Pattern Integer
  let pat2 = ConsPat Wildcard Wildcard :: Pattern [Integer]
  let pat3 = ConsPat (ConsPat Wildcard Wildcard) Wildcard :: Pattern [[Integer]]
  let patTwinPrimes = JoinPat Wildcard (ConsPat (PatVar "p") (ConsPat (LambdaPat (\[Result p] ->let p = unsafeCoerce p in p + 2)) Wildcard)) :: Pattern [Integer]

  -- list
  let xss0 = match [1,2,5,9,4] (list integer) [(ConsPat (PatVar "x") (PatVar "xs"), \[Result x, Result xs] -> let x = unsafeCoerce x in let xs = unsafeCoerce xs in (x, xs))]
  assert (xss0 == (1, [2,5,9,4])) $ print "ok 1"

  -- infinite target
  let twinprimes = matchAll primes (list integer) [(patTwinPrimes, \[Result p] -> let p = unsafeCoerce p in (p, p + 2))]
  assert (take 10 twinprimes == [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]) $ print "ok 2"

  -- multiset
  let xss1 = matchAll [1,2,5,9,4] (multiset integer) [(ConsPat (PatVar "x") (PatVar "xs"), \[Result x, Result xs] -> let x' = unsafeCoerce x in let xs' = unsafeCoerce xs in (x', xs'))]
  assert (xss1 == [(1,[2,5,9,4]),(2,[1,5,9,4]),(5,[1,2,9,4]),(9,[1,2,5,4]),(4,[1,2,5,9])]) $ print "ok 3"

  -- value, and, or, not pattern
  let xss2 = matchAll [1,2,5,9,4] (multiset integer) [(ConsPat (AndPat (NotPat (ValuePat 5)) (PatVar "x")) (ConsPat (AndPat (OrPat (ValuePat 1) (ValuePat 2)) (PatVar "y")) (PatVar "xs")), \[Result x, Result y, Result xs] -> let x = unsafeCoerce x in let y = unsafeCoerce y in let xs = unsafeCoerce xs in (x, y, xs))]
  assert (xss2 == [(1,2,[5,9,4]),(2,1,[5,9,4]),(9,1,[2,5,4]),(9,2,[1,5,4]),(4,1,[2,5,9]),(4,2,[1,5,9])]) $ print "ok 4"

  -- later pattern
  let xss3 = match [1..5] (list integer) [(ConsPat (LaterPat (LambdaPat (\[Result p, _] -> let p = unsafeCoerce p in p - 1))) (ConsPat (PatVar "x") (PatVar "xs")), \[Result x, Result xs] -> let x = unsafeCoerce x in let xs = unsafeCoerce xs in (x, xs))]
  assert (xss3 == (2,[3,4,5])) $ print "ok 5"

  -- check order
  let xss4 = matchAll [1..] (multiset integer) [(ConsPat (PatVar "x") (ConsPat (PatVar "y") Wildcard), \[Result x, Result y] -> let x' = unsafeCoerce x in let y' = unsafeCoerce y in (x', y') :: (Integer, Integer))]
  assert (take 10 xss4 == [(1,2),(1,3),(2,1),(1,4),(2,3),(3,1),(1,5),(2,4),(3,2),(4,1)]) $ print "ok 6"

  -- map
  assert (mymap (+ 10) [1,2,3] == [11, 12, 13]) $ print "ok mymap"

  -- concat
  assert (myconcat [[1,2], [3], [4, 5]] == [1..5]) $ print "ok myconcat"

  return ()
