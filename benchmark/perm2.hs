import           Control.Egison

import           Language.Egison                ( initialEnv
                                                , runEgisonExpr
                                                , EgisonValue
                                                )
import           Language.Egison.CmdOptions     ( defaultOption
                                                , EgisonOpts(..)
                                                )
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

perm2Egison :: Int -> IO EgisonValue
perm2Egison n = do
  egisonEnv   <- initialEnv option
  Right value <- runEgisonExpr option egisonEnv expr
  pure value
 where
  option = defaultOption { optSExpr = False }
  expr =
    "matchAllDFS [1 .. "
      ++ show n
      ++ "] as multiset something with $x :: $y :: _ -> (x, y)"

main :: IO ()
main = defaultMain
  [ bgroup
      "perm2"
      [ bgroup
        "50"
        [ bench "native" $ nf perm2Native 50
        , bench "egison" $ whnfIO $ perm2Egison 50
        , bench "miniEgison" $ nf perm2 50
        ]
      , bgroup
        "100"
        [ bench "native" $ nf perm2Native 100
        , bench "egison" $ whnfIO $ perm2Egison 100
        , bench "miniEgison" $ nf perm2 100
        ]
      , bgroup
        "200"
        [ bench "native" $ nf perm2Native 200
        , bench "egison" $ whnfIO $ perm2Egison 200
        , bench "miniEgison" $ nf perm2 200
        ]
      , bgroup
        "400"
        [ bench "native" $ nf perm2Native 400
        , bench "egison" $ whnfIO $ perm2Egison 400
        , bench "miniEgison" $ nf perm2 400
        ]
      , bgroup
        "800"
        [ bench "native" $ nf perm2Native 800
        , bench "egison" $ whnfIO $ perm2Egison 800
        , bench "miniEgison" $ nf perm2 800
        ]
      , bgroup
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
