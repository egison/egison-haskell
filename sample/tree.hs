{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

import           Control.Egison

data Tree a = Leaf | Node a (Tree a) (Tree a)

data TreeM m = TreeM m
instance (Matcher m a) => Matcher (TreeM m) (Tree a)

class TreePat m a where
  leafPat :: Pattern a m ctx '[]
  nodePat :: (a ~ (Tree a'), m ~ (f m'))
          => Pattern a' m' ctx xs
          -> Pattern a m (ctx :++: xs) ys
          -> Pattern a m (ctx :++: xs :++: ys) zs
          -> Pattern a m ctx (xs :++: ys :++: zs)

instance (Matcher m a) => TreePat (TreeM m) (Tree a) where
  leafPat =
    Pattern (\ctx _ t -> case t of
                           Leaf -> [MNil]
                           _    -> [])
  nodePat p1 p2 p3 =
    Pattern (\ctx (TreeM m) t -> case t of
                                   Node v t1 t2 -> [threeMAtoms (MAtom p1 m v) (MAtom p2 (TreeM m) t1) (MAtom p3 (TreeM m) t2)]
                                   _ -> [])

main :: IO ()
main = do
  let t1 = Node 3 (Node 1 Leaf Leaf) (Node 2 Leaf Leaf)
  let t2 = Leaf
  putStrLn $ show $ f t1 -- [3]
  putStrLn $ show $ f t2 -- [0]
 where
   f [view| nodePat $x _ _ as TreeM Eql |] = x
   f [view| leafPat        as TreeM Eql |] = 0
