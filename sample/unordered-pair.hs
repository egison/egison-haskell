{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}

import           Control.Egison hiding (Integer)
import qualified Control.Egison as M

--
-- UnorderedEqlPair
--

data UnorderedEqlPair = UnorderedEqlPair
instance (Eq a) => Matcher UnorderedEqlPair (a, a)

uepair :: (Eq a)
       => Pattern a Eql ctx xs
       -> Pattern a Eql (ctx :++: xs) ys
       -> Pattern (a, a) UnorderedEqlPair ctx (xs :++: ys)
uepair p1 p2 = Pattern (\_ UnorderedEqlPair (t1, t2) ->
                          [twoMAtoms (MAtom p1 Eql t1) (MAtom p2 Eql t2)
                          ,twoMAtoms (MAtom p1 Eql t2) (MAtom p2 Eql t1)])

--
-- UnorderedPair (parameterized)
--

data UnorderedPair m = UnorderedPair m
instance Matcher m a => Matcher (UnorderedPair m) (a, a)

upair :: (Matcher m (a, a), m ~ (UnorderedPair m'), Matcher m' a)
      => Pattern a m' ctx xs
      -> Pattern a m' (ctx :++: xs) ys
      -> Pattern (a, a) m ctx (xs :++: ys)
upair p1 p2 = Pattern (\_ (UnorderedPair m') (t1, t2) ->
                         [twoMAtoms (MAtom p1 m' t1) (MAtom p2 m' t2)
                         ,twoMAtoms (MAtom p1 m' t2) (MAtom p2 m' t1)])

--
-- Main
--

main :: IO ()
main = do
  let t1 = (1,2)
  putStrLn $ show $ matchAll t1 UnorderedEqlPair [[mc| uepair #2 $x -> x |]]
  putStrLn $ show $ matchAll t1 (UnorderedPair Eql) [[mc| upair #2 $x -> x |]]
