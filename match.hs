{-# LANGUAGE ExistentialQuantification #-}
import           Prelude

data MState = MState [MAtom] [Result]
data MAtom = forall a. Show a => MAtom (Pattern a) (Matcher a) a
data Result = forall a. Show a => Result a
data Matcher a = Something | Matcher (Pattern a -> a -> [[MAtom]])

data CollectionPattern a = Wildcard | PatVar String | ValuePat a | LambdaPat ([Result] -> a) | Nil | Cons a (CollectionPattern [a]) | Join (CollectionPattern [a]) (CollectionPattern [a])
type Pattern a = CollectionPattern a


eq :: (Eq a, Show a) => Matcher a
eq = Matcher eq'

eq' Wildcard _       = [[]]
eq' (PatVar s) tgt   = [[MAtom (PatVar s) Something tgt]]
eq' (ValuePat v) tgt = [[] | v == tgt]

something :: Matcher a
something = Something

-- list :: (Eq a, Show a) => Matcher a -> Matcher [a]
-- list m = Matcher (\pat tgt ->
--   case pat of
--     Nil ->
--       case tgt of
--         [] -> [[]]
--         _ -> []
--     Cons a b ->
--       case tgt of
--         (x:xs) -> [[MAtom (ValuePat a) m x, MAtom b (list m) xs]]
--         _ -> []
--     Wildcard -> [[]]
--     PatVar s -> [[MAtom (PatVar s) Something tgt]]
--     ValuePat v -> if v == tgt then [[]] else [])
--
--
processMStates :: [MState] -> [[Result]]
processMStates [] = []
processMStates (MState [] results:rs) = results:processMStates rs
processMStates (mstate:rs) = processMStates (processMState mstate ++ rs)

processMState :: MState -> [MState]
processMState (MState (MAtom (LambdaPat f) (Matcher matcher) tgt:mstack) results) =
  let nextmatomss = matcher (ValuePat $ f results) tgt in
      map (\nextmatoms -> MState (nextmatoms ++ mstack) results) nextmatomss
processMState (MState (MAtom Wildcard Something _:mstack) results) = [MState mstack results]
processMState (MState (MAtom (PatVar _) Something tgt:mstack) results) = [MState mstack (results ++ [Result tgt])]
processMState (MState (MAtom pat (Matcher matcher) tgt:mstack) results) =
  let nextmatomss = matcher pat tgt in
      map (\nextmatoms -> MState (nextmatoms ++ mstack) results) nextmatomss

matchAll :: Show a => a -> Matcher a -> [(Pattern a, [Result] -> b)] -> [b]
matchAll tgt matcher =
  foldr (\(pat, f) matches ->
    let resultss = processMStates [MState [MAtom pat matcher tgt] []] in
        map f resultss ++ matches) []

-- main = print $ matchAll [1,2,5,9,4] (list eq) [(Cons 1 (Cons (PatVar "x") Wildcard), \[Result x] -> x + 2)] -- -> 4
main = print $ matchAll [1,2,5,9,4] eq [(PatVar "x", \[Result x] -> show x)] -- ok
