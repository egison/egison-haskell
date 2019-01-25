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
data MAtom = forall a. (Typeable a, Eq a, P a) => MAtom (Pattern a) (Matcher a) a
type Result = Dynamic
data Matcher a = Something | Matcher (Pattern a -> a -> [[MAtom]])

--
-- pattern
--

data family Pattern a

data instance Pattern Integer :: * where
  IWildcard  :: Pattern Integer
  IPatVar    :: String -> Pattern Integer
  ILambdaPat :: ([Result] -> Integer) -> Pattern Integer
  IValuePat  :: Integer -> Pattern Integer
  IAndPat :: Pattern Integer -> Pattern Integer -> Pattern Integer
  IOrPat :: Pattern Integer -> Pattern Integer -> Pattern Integer
  INotPat :: Pattern Integer -> Pattern Integer
  ILaterPat :: Pattern Integer -> Pattern Integer

data instance Pattern [a] :: * where
  LWildcard  :: Pattern [a]
  LPatVar    :: String -> Pattern [a]
  LLambdaPat :: ([Result] -> [a]) -> Pattern [a]
  LValuePat  :: [a] -> Pattern [a]
  LAndPat :: Pattern [a] -> Pattern [a] -> Pattern [a]
  LOrPat :: Pattern [a] -> Pattern [a] -> Pattern [a]
  LNotPat :: Pattern [a] -> Pattern [a]
  LLaterPat :: Pattern [a] -> Pattern [a]

  LNilPat    :: Pattern [a]
  LConsPat   :: Pattern a -> Pattern [a] -> Pattern [a]
  LJoinPat   :: Pattern [a] -> Pattern [a] -> Pattern [a]

class P a where
  wildcard :: Pattern a
  patVar :: String -> Pattern a
  lambdaPat :: ([Result] -> a) -> Pattern a
  valuePat :: a -> Pattern a
  andPat :: Pattern a -> Pattern a -> Pattern a
  orPat :: Pattern a -> Pattern a -> Pattern a
  notPat :: Pattern a -> Pattern a
  laterPat :: Pattern a -> Pattern a

  isWildcard :: Pattern a -> Maybe ()
  isPatVar :: Pattern a -> Maybe String
  isLambdaPat :: Pattern a -> Maybe ([Result] -> a)
  isValuePat :: Pattern a -> Maybe a
  isAndPat :: Pattern a -> Maybe (Pattern a, Pattern a)
  isOrPat :: Pattern a -> Maybe (Pattern a, Pattern a)
  isNotPat :: Pattern a -> Maybe (Pattern a)
  isLaterPat :: Pattern a -> Maybe (Pattern a)

instance P Integer where
  wildcard = IWildcard
  patVar = IPatVar
  lambdaPat = ILambdaPat
  valuePat = IValuePat
  andPat = IAndPat
  orPat = IOrPat
  notPat = INotPat
  laterPat = ILaterPat

  isWildcard IWildcard = Just ()
  isWildcard _         = Nothing
  isPatVar (IPatVar s) = Just s
  isPatVar _           = Nothing
  isLambdaPat (ILambdaPat f) = Just f
  isLambdaPat _              = Nothing
  isValuePat (IValuePat v) = Just v
  isValuePat _             = Nothing
  isAndPat (IAndPat p1 p2) = Just (p1, p2)
  isAndPat _               = Nothing
  isOrPat (IOrPat p1 p2) = Just (p1, p2)
  isOrPat _              = Nothing
  isNotPat (INotPat p) = Just p
  isNotPat _           = Nothing
  isLaterPat (ILaterPat p) = Just p
  isLaterPat _             = Nothing

instance P [a] where
  wildcard = LWildcard
  patVar = LPatVar
  lambdaPat = LLambdaPat
  valuePat = LValuePat
  andPat = LAndPat
  orPat = LOrPat
  notPat = LNotPat
  laterPat = LLaterPat

  isWildcard LWildcard = Just ()
  isWildcard _         = Nothing
  isPatVar (LPatVar s) = Just s
  isPatVar _           = Nothing
  isLambdaPat (LLambdaPat f) = Just f
  isLambdaPat _              = Nothing
  isValuePat (LValuePat v) = Just v
  isValuePat _             = Nothing
  isAndPat (LAndPat p1 p2) = Just (p1, p2)
  isAndPat _               = Nothing
  isOrPat (LOrPat p1 p2) = Just (p1, p2)
  isOrPat _              = Nothing
  isNotPat (LNotPat p) = Just p
  isNotPat _           = Nothing
  isLaterPat (LLaterPat p) = Just p
  isLaterPat _             = Nothing

--
-- matcher
--

eqM :: (Typeable a, Eq a, P a) => Matcher a
eqM = Matcher eqM'

eqM' :: (Typeable a, Eq a, P a) => Pattern a -> a -> [[MAtom]]
eqM' p@(isWildcard -> Just _) t = [[MAtom p Something t]]
eqM' p@(isPatVar -> Just _) t   = [[MAtom p Something t]]
eqM' (isValuePat -> Just v) t   = [[] | v == t]

integer = eqM

list :: (Typeable a, Eq a, P a) => Matcher a -> Matcher [a]
list m = Matcher (list' m)

list' :: (Typeable a, Eq a, P a) => Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
list' _ p@(isWildcard -> Just _) t = [[MAtom p Something t]]
list' _ p@(isPatVar -> Just _) t = [[MAtom p Something t]]
list' _ (isValuePat -> Just v) t = [[] | v == t]
list' _ LNilPat t = [[] | null t]
list' _ (LConsPat _ _) [] = []
list' m (LConsPat p1 p2) (t:ts) = [[MAtom p1 m t, MAtom p2 (list m) ts]]
list' m (LJoinPat p1 p2) t = map (\(hs, ts) -> [MAtom p1 (list m) hs, MAtom p2 (list m) ts]) (unjoin t)

multiset :: (Typeable a, Eq a, P a) => Matcher a -> Matcher [a]
multiset m = Matcher (multiset' m)

multiset' :: (Typeable a, Eq a, P a) => Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
multiset' _ p@(isWildcard -> Just _) t = [[MAtom p Something t]]
multiset' _ p@(isPatVar -> Just _) t = [[MAtom p Something t]]
multiset' _ (isValuePat -> Just v) t = [[] | v == t]
multiset' _ LNilPat t = [[] | null t]
multiset' m (LConsPat p1 p2) t =
  map (\(x, xs) -> [MAtom p1 m x, MAtom p2 (multiset m) xs])
    (matchAll t (list m)
      [(LJoinPat (patVar "hs") (LConsPat (patVar "x") (patVar "ts")), \[hs, x, ts] -> let x' = fromJust $ fromDynamic x in let hs' = fromJust $ fromDynamic hs in let ts' = fromJust $ fromDynamic ts in (x', hs' ++ ts'))])

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
processMState (MState (MAtom (isWildcard -> Just _) Something t:atoms) rs) = [MState atoms rs]
processMState (MState (MAtom (isPatVar -> Just _) Something t:atoms) rs) = [MState atoms (rs ++ [toDyn t])]
processMState (MState (MAtom (isLambdaPat -> Just f) (Matcher m) t:atoms) rs) =
  let next = m (valuePat $ f rs) t in
      map (\nt -> MState (nt ++ atoms) rs) next
processMState (MState (MAtom (isAndPat -> Just (p1, p2)) m t:atoms) rs) = [MState (MAtom p1 m t:MAtom p2 m t:atoms) rs]
processMState (MState (MAtom (isOrPat -> Just (p1, p2)) m t:atoms) rs) = [MState (MAtom p1 m t:atoms) rs, MState (MAtom p2 m t:atoms) rs]
processMState (MState (MAtom (isNotPat -> Just p) m t:atoms) rs) = [MState atoms rs | null $ processMStates [MState [MAtom p m t] rs]]
processMState (MState (MAtom (isLaterPat -> Just p) m t:atoms) rs) = [MState (atoms ++ [MAtom p m t]) rs]
processMState (MState (MAtom p (Matcher m) t:atoms) rs) =
  map (\newAtoms -> MState (newAtoms ++ atoms) rs) (m p t)

matchAll :: (Typeable a, Eq a, P a) => a -> Matcher a -> [(Pattern a, [Result] -> b)] -> [b]
matchAll tgt matcher =
  foldr (\(pat, f) matches ->
    let resultss = processMStates [MState [MAtom pat matcher tgt] []] in
        map f resultss ++ matches) []

match :: (Typeable a, Eq a, P a) => a -> Matcher a -> [(Pattern a, [Result] -> b)] -> b
match t m xs = head $ matchAll t m xs


main :: IO ()
main = do
  let pat1 = wildcard :: Pattern Integer
  let pat2 = LConsPat wildcard wildcard :: Pattern [Integer]
  let pat3 = LConsPat (LConsPat wildcard wildcard) wildcard :: Pattern [[Integer]]
  let patTwinPrimes = LJoinPat wildcard (LConsPat (patVar "p") (LConsPat (lambdaPat (\rs -> case map fromDynamic rs of [Just p] -> p + 2)) wildcard)) :: Pattern [Integer]

  -- list
  let [(x, xs)] = matchAll [1,2,5,9,4] (list integer) [(LConsPat (patVar "x") (patVar "xs"), \[x, xs] -> (fromJust $ fromDynamic x :: Integer, fromJust $ fromDynamic xs :: [Integer]))]
  assert ((x, xs) == (1, [2,5,9,4])) $ print "ok 1"

  -- infinite target
  let twinprimes = matchAll primes (list integer) [(patTwinPrimes, \[p] -> let p' = fromJust $ fromDynamic p :: Integer in (p', p' + 2))]
  assert (take 10 twinprimes == [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]) $ print "ok 2"

  -- multiset
  let xss = matchAll [1,2,5,9,4] (multiset integer) [(LConsPat (patVar "x") (patVar "xs"), \[x, xs] -> (fromJust $ fromDynamic x :: Integer, fromJust $ fromDynamic xs :: [Integer]))]
  assert (xss == [(1,[2,5,9,4]),(2,[1,5,9,4]),(5,[1,2,9,4]),(9,[1,2,5,4]),(4,[1,2,5,9])]) $ print "ok 3"

  -- value, and pattern
  let xss2 = matchAll [1,2,5,9,4] (multiset integer) [(LConsPat (patVar "x") (LConsPat (andPat (valuePat 2) (patVar "y")) (patVar "xs")), \[x, y, xs] -> (fromJust $ fromDynamic x :: Integer, fromJust $ fromDynamic y :: Integer, fromJust $ fromDynamic xs :: [Integer]))]
  assert (xss2 == [(1,2,[5,9,4]),(5,2,[1,9,4]),(9,2,[1,5,4]),(4,2,[1,5,9])]) $ print "ok 4"

  -- later pattern
  let xss3 = match [1..5] (list integer) [(LConsPat (laterPat (lambdaPat (\rs -> case map fromDynamic rs of [Just p, _] -> p - 1))) (LConsPat (patVar "x") (patVar "xs")), \[x, xs] -> (fromJust $ fromDynamic x :: Integer, fromJust $ fromDynamic xs :: [Integer]))]
  assert (xss3 == (2,[3,4,5])) $ print "ok 5"

  return ()
