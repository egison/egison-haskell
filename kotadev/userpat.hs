{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
import           Control.Egison

data Tree a = Leaf | Node (Tree a) a (Tree a)

tree :: Matcher a -> Matcher (Tree a)
tree m = Matcher (tree' m)

tree' :: Matcher a -> Pattern (Tree a) -> Tree a -> [[MAtom]]
tree' _ p@Wildcard t             = [[MAtom p something t]]
tree' _ p@(PatVar _) t           = [[MAtom p something t]]
tree' m (ValuePat' v) t          = [[] | v == t]
tree' m (UserPat "Leaf" []) Leaf = [[]]
tree' m (UserPat "Leaf" []) _    = []
tree' m (UserPat "Node" [p1, p2, p3]) (Node tree1 v tree2) = [[MAtom p1 (tree m) tree1], [MAtom p2 m v], [MAtom p3 (tree m) tree2]]
tree' _ _ _ = []


main :: IO ()
main = do
  let ans = matchAll (Node (Node Leaf 1 Leaf) 3 (Node Leaf 2 Leaf)) (tree integer)
              [ [mc| (UserPat "Node" [pack Wildcard, pack $x, pack Wildcard])  => x |]
                [mc| (UserPat "Leaf" []) => 0 |] ] :: [Int]
  putStrLn $ show ans -- [3]
