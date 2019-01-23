{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}

import           Data.Dynamic
import           Data.Maybe
import           Prelude

data MState = MState [MAtom] [Result]
data MAtom = forall a. (Typeable a, Eq a, P a) => MAtom (Pattern a) (Matcher a) a
type Result = Dynamic
data Matcher a = Something | Matcher (Pattern a -> a -> [[MAtom]])

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

integerM :: Matcher Integer
integerM = Matcher integerM'

integerM' :: Pattern Integer -> Integer -> [[MAtom]]
integerM' p@(isWildcard -> Just _) t = [[MAtom p Something t]]
integerM' p@(isPatVar -> Just _) t   = [[MAtom p Something t]]
integerM' (isValuePat -> Just v) t   = [[] | v == t]

listM :: (Typeable a, Eq a, P a) => Matcher a -> Matcher [a]
listM m = Matcher (listM' m)

listM' :: (Typeable a, Eq a, P a) => Matcher a -> Pattern [a] -> [a] -> [[MAtom]]
listM' _ p@(isWildcard -> Just _) t = [[MAtom p Something t]]
listM' _ p@(LPatVar _) t = [[MAtom p Something t]]
listM' _ (LValuePat v) t = [[] | v == t]
listM' _ (LConsPat _ _) [] = []
listM' m (LConsPat p1 p2) (t:ts) = [[MAtom p1 m t, MAtom p2 (listM m) ts]]
listM' m (LJoinPat p1 p2) t = map (\(hs, ts) -> [MAtom p1 (listM m) hs, MAtom p2 (listM m) ts]) (unjoin t)

unjoin :: [a] -> [([a], [a])]
unjoin []     = [([], [])]
unjoin (x:xs) = ([], xs) : map (\(hs,ts) -> (x:hs, ts)) (unjoin xs)

matchAll :: (Typeable a, Eq a, P a) => a -> Matcher a -> [(Pattern a, [Result] -> b)] -> [b]
matchAll tgt matcher =
  foldr (\(pat, f) matches ->
    let resultss = processMStates [MState [MAtom pat matcher tgt] []] in
        map f resultss ++ matches) []

main :: IO ()
main = do
  let pat1 = wildcard :: Pattern Integer
  let pat2 = LConsPat wildcard wildcard :: Pattern [Integer]
  let pat3 = LConsPat (LConsPat wildcard wildcard) wildcard :: Pattern [[Integer]]
  let patTwinPrimes = LJoinPat wildcard (LConsPat (patVar "p") (LConsPat (lambdaPat (\rs -> case map fromDynamic rs of [Just p] -> p + 2)) wildcard)) :: Pattern [Integer]

  let [(x, xs)] = matchAll [1,2,5,9,4] (listM integerM) [(LConsPat (patVar "x") (patVar "xs"), \[x, xs] -> (fromJust $ fromDynamic x :: Integer, fromJust $ fromDynamic xs :: [Integer]))]
  print (x, xs)

  return ()
