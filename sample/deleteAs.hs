{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs           #-}

import Control.Egison

deleteWith m x xs =
  match xs (List m)
    [[mc| $hs ++ #x : $ts -> hs ++ ts |],
     [mc| _ -> xs |]]

main = do
  putStrLn $ show $ deleteWith (List Eql) [1, 2] [[2, 3], [2, 1], [2, 4]] -- [[2, 3], [2, 1], [2, 4]]
  putStrLn $ show $ deleteWith (Multiset Eql) [1, 2] [[2, 3], [2, 1], [2, 4]] -- [[2, 3], [2, 4]]
