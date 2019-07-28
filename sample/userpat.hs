{-# LANGUAGE QuasiQuotes     #-}

import           Control.Egison
import           Unsafe.Coerce

data Tree a = Leaf | Node (Tree a) a (Tree a)

tree :: Matcher a -> Matcher (Tree a)
tree m = Matcher (tree' m)

tree' :: Matcher a -> Pattern (Tree a) -> Tree a -> [[MAtom]]
tree' _ p@Wildcard t             = [[MAtom p something t]]
tree' _ p@(PatVar _) t           = [[MAtom p something t]]
tree' m (ValuePat' v) t          = [[] | v == t]
tree' m (UserPat "Leaf" []) Leaf = [[]]
tree' m (UserPat "Leaf" []) _    = []
tree' m (UserPat "Node" [Pat p1, Pat p2, Pat p3]) (Node tree1 v tree2) =
  let p1' = unsafeCoerce p1 in
  let p2' = unsafeCoerce p2 in
  let p3' = unsafeCoerce p3 in
  [[MAtom p1' (tree m) tree1, MAtom p2' m v, MAtom p3' (tree m) tree2]]
tree' _ _ _                      = []

main :: IO ()
main = do
  let t1 = Node (Node Leaf 1 Leaf) 3 (Node Leaf 2 Leaf)
  let t2 = Leaf :: Tree Integer
  putStrLn $ show $ f t1 -- [3]
  putStrLn $ show $ f t2 -- [0]
 where
   f t = matchAll t (tree integer)
       [ [mc| (UserPat "Node" [Pat Wildcard, Pat $x, Pat Wildcard]) => x |],
         [mc| (UserPat "Leaf" []) => 0 |] ]
