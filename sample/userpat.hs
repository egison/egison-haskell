{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}

import           Control.Egison
import           Unsafe.Coerce

data Tree a = Leaf | Node (Tree a) a (Tree a)

data TreeM a = TreeM a

tree :: Matcher a -> Matcher (TreeM a)
tree (Matcher m) = Matcher (TreeM m)

class TreePat mt a where
  leafPat :: Pattern a ctx mt '[]
  nodePat :: a ~ (Tree b) => mt ~ Matcher (f m) => Pattern a ctx mt xs -> Pattern b (ctx :++: xs) (Matcher m) ys -> Pattern a (ctx :++: xs :++: ys) mt zs -> Pattern a ctx mt (xs :++: (ys :++: zs))

instance TreePat (Matcher (TreeM m)) (Tree a) where
  leafPat =
    Pattern (\t ctx _ ->
      case t of
        Leaf -> [MNil]
        _    -> [])
  nodePat p1 p2 p3 =
    Pattern (\t ctx (TreeM m) ->
      case t of
        Node t1 v t2 -> [MCons (MAtom p1 t1 (TreeM m)) $ MCons (MAtom p2 v m) $ MCons (MAtom p3 t2 (TreeM m)) MNil]
        _ -> [])

main :: IO ()
main = do
  let t1 = Node (Node Leaf 1 Leaf) 3 (Node Leaf 2 Leaf)
  let t2 = Leaf
  putStrLn $ show $ f t1 -- [3]
  putStrLn $ show $ f t2 -- [0]
 where
   f t = matchAll t (tree integer)
         $ [mc| nodePat Wildcard $x Wildcard => x |] .*.
           [mc| leafPat => 0 |] .*. PNil
