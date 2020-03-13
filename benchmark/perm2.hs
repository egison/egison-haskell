import           Control.Egison

import           Criterion.Main


perm2 :: Int -> [(Int, Int)]
perm2 n =
  matchAllDFS [1 .. n] (Multiset Something) [[mc| $x : $y : _ -> (x, y) |]]

perm2Native :: Int -> [(Int, Int)]
perm2Native n = go [1 .. n] [] []
 where
  go [] _ acc = acc
  go (x : xs) rest acc =
    [ (x, y) | y <- rest ++ xs ] ++ go xs (rest ++ [x]) acc

main :: IO ()
main = defaultMain
  [ bgroup
      "perm2"
      [ bgroup
        "1600"
        [ bench "native" $ nf perm2Native 1600
        , bench "miniEgison" $ nf perm2 1600
        ]
      , bgroup
        "3200"
        [ bench "native" $ nf perm2Native 3200
        , bench "miniEgison" $ nf perm2 3200
        ]
      , bgroup
        "6400"
        [ bench "native" $ nf perm2Native 6400
        , bench "miniEgison" $ nf perm2 6400
        ]
      ]
  ]
