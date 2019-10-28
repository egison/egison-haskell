splits [] = []
splits (x:xs) = ([],x:xs) : [ (x:ys,zs) | (ys,zs) <- splits xs ]

mapWithBothSides :: ([Integer] -> Integer -> [Integer] -> a) -> [Integer] -> [a]
mapWithBothSides f xs = [ f hs (head ts) (tail ts) | (hs,ts) <- splits xs, not (null ts) ]

main = do
  putStrLn $ show $ mapWithBothSides (\hs x ts -> (hs,x,ts)) [1..3]
