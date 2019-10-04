{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}

import           Control.Egison hiding (Integer)
import qualified Control.Egison as M

--
-- UnorderedIntegerPair
--

data UnorderedIntegerPair = UnorderedIntegerPair
instance Matcher UnorderedIntegerPair (Integer, Integer)

uipair :: Pattern Integer M.Integer ctx xs
       -> Pattern Integer M.Integer (ctx :++: xs) ys
       -> Pattern (Integer, Integer) UnorderedIntegerPair ctx (xs :++: ys)
uipair p1 p2 = Pattern (\_ UnorderedIntegerPair (t1, t2) -> [twoMAtoms (MAtom p1 M.Integer t1) (MAtom p2 M.Integer t2)
                                                            ,twoMAtoms (MAtom p1 M.Integer t2) (MAtom p2 M.Integer t1)])

--
-- UnorderedPair (parameterized)
--

data UnorderedPair m = UnorderedPair m
instance Matcher m a => Matcher (UnorderedPair m) (a, a)

upair :: (Matcher m a , a ~ (b, b), m ~ (UnorderedPair m'), Matcher m' b)
      => Pattern b m' ctx xs
      -> Pattern b m' (ctx :++: xs) ys
      -> Pattern a m ctx (xs :++: ys)
upair p1 p2 = Pattern (\_ (UnorderedPair m') (t1, t2) -> [twoMAtoms (MAtom p1 m' t1) (MAtom p2 m' t2)
                                                         ,twoMAtoms (MAtom p1 m' t2) (MAtom p2 m' t1)])

--
-- Main
--

main :: IO ()
main = do
  let t1 = (1,2)
  putStrLn $ show $ matchAll t1 UnorderedIntegerPair [[mc| uipair #2 $x => x |]]
  putStrLn $ show $ matchAll t1 (UnorderedPair Eql) [[mc| upair #2 $x => x |]]
