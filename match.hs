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

data instance Pattern [a] :: * where
  LWildcard  :: Pattern [a]
  LPatVar    :: String -> Pattern [a]
  LLambdaPat :: ([Result] -> [a]) -> Pattern [a]
  LValuePat  :: [a] -> Pattern [a]
  LNilPat    :: Pattern [a]
  LConsPat   :: Pattern a -> Pattern [a] -> Pattern [a]
  LJoinPat   :: Pattern [a] -> Pattern [a] -> Pattern [a]

class P a where
  wildcard :: Pattern a
  patVar :: String -> Pattern a
  lambdaPat :: ([Result] -> a) -> Pattern a
  valuePat :: a -> Pattern a

  isWildcard :: Pattern a -> Maybe ()
  isPatVar :: Pattern a -> Maybe String
  isLambdaPat :: Pattern a -> Maybe ([Result] -> a)
  isValuePat :: Pattern a -> Maybe a

instance P Integer where
  wildcard = IWildcard
  patVar = IPatVar
  lambdaPat = ILambdaPat
  valuePat = IValuePat

  isWildcard IWildcard = Just ()
  isWildcard _         = Nothing
  isPatVar (IPatVar s) = Just s
  isPatVar _           = Nothing
  isLambdaPat (ILambdaPat f) = Just f
  isLambdaPat _              = Nothing
  isValuePat (IValuePat v) = Just v
  isValuePat _             = Nothing

instance P [a] where
  wildcard = LWildcard
  patVar = LPatVar
  lambdaPat = LLambdaPat
  valuePat = LValuePat

  isWildcard LWildcard = Just ()
  isWildcard _         = Nothing
  isPatVar (LPatVar s) = Just s
  isPatVar _           = Nothing
  isLambdaPat (LLambdaPat f) = Just f
  isLambdaPat _              = Nothing
  isValuePat (LValuePat v) = Just v
  isValuePat _             = Nothing

--
-- matcher
--

eqM :: (Typeable a, Eq a, P a) => Matcher a
eqM = Matcher eqM'

eqM' :: (Typeable a, Eq a, P a) => Pattern a -> a -> [[MAtom]]
eqM' p@(isWildcard -> Just _) t = [[MAtom p Something t]]
eqM' p@(isPatVar -> Just _) t   = [[MAtom p Something t]]
eqM' (isValuePat -> Just v) t   = [[] | v == t]

integerM = eqM

listM :: (Typeable a, Eq a, P a) => Matcher a -> Matcher [a]
listM m = Matcher (listM' m)

listM' :: (Typeable a, Eq a, P a) => Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
listM' _ p@(isWildcard -> Just _) t = [[MAtom p Something t]]
listM' _ p@(LPatVar _) t = [[MAtom p Something t]]
listM' _ (LValuePat v) t = [[] | v == t]
listM' _ LNilPat t = [[] | null t]
listM' _ (LConsPat _ _) [] = []
listM' m (LConsPat p1 p2) (t:ts) = [[MAtom p1 m t, MAtom p2 (listM m) ts]]
listM' m (LJoinPat p1 p2) t = map (\(hs, ts) -> [MAtom p1 (listM m) hs, MAtom p2 (listM m) ts]) (unjoin t)

multisetM :: (Typeable a, Eq a, P a) => Matcher a -> Matcher [a]
multisetM m = Matcher (multisetM' m)

multisetM' :: (Typeable a, Eq a, P a) => Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
multisetM' _ p@(isWildcard -> Just _) t = [[MAtom p Something t]]
multisetM' _ p@(LPatVar _) t = [[MAtom p Something t]]
multisetM' _ (LValuePat v) t = [[] | v == t]
multisetM' _ LNilPat t = [[] | null t]
multisetM' m (LConsPat p1 p2) t =
  map (\(x, xs) -> [MAtom p1 m x, MAtom p2 (multisetM m) xs])
    (matchAll t (listM m)
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

  let [(x, xs)] = matchAll [1,2,5,9,4] (listM integerM) [(LConsPat (patVar "x") (patVar "xs"), \[x, xs] -> (fromJust $ fromDynamic x :: Integer, fromJust $ fromDynamic xs :: [Integer]))]
  assert ((x, xs) == (1, [2,5,9,4])) $ print "ok 1"

  let twinprimes = matchAll primes (listM integerM) [(patTwinPrimes, \[p] -> let p' = fromJust $ fromDynamic p :: Integer in (p', p' + 2))]
  assert (take 10 twinprimes == [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]) $ print "ok 2"

  let xss = matchAll [1,2,5,9,4] (multisetM integerM) [(LConsPat (patVar "x") (patVar "xs"), \[x, xs] -> (fromJust $ fromDynamic x :: Integer, fromJust $ fromDynamic xs :: [Integer]))]
  assert (xss == [(1,[2,5,9,4]),(2,[1,5,9,4]),(5,[1,2,9,4]),(9,[1,2,5,4]),(4,[1,2,5,9])]) $ print "ok 3"

  let xss2 = matchAll [1,2,5,9,4] (multisetM integerM) [(LConsPat (patVar "xs") (LConsPat (valuePat 2) (patVar "ys")), \[xs, ys] -> (fromJust $ fromDynamic xs :: Integer, fromJust $ fromDynamic ys :: [Integer]))]
  assert (xss2 == [(1,[5,9,4]),(5,[1,9,4]),(9,[1,5,4]),(4,[1,5,9])]) $ print "ok 4"

  return ()
