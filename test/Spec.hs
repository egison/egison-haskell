{-# LANGUAGE GADTs       #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec (spec) where

import           Control.Egison
import           Data.Numbers.Primes
import           Test.Hspec

--
-- Basic list processing functions in pattern-matching-oriented programming style
--

pmap :: (a -> b) -> [a] -> [b]
pmap f xs = matchAll xs (list something)
             [[mc| join _ (cons $x _) => f x |]]

pmConcat :: [[a]] -> [a]
pmConcat xss = matchAll xss (multiset (multiset something))
                 [[mc| cons (cons $x _) _ => x |]]

spec :: Spec
spec = do
  describe "list and multiset matchers" $ do
    it "cons pattern for list" $
      matchAll [1,2,3] (list integer) [[mc| cons $x $xs => (x, xs) |]]
      `shouldBe` [(1, [2,3])]

    it "multiset cons pattern" $
      matchAll [1,2,3] (multiset integer) [[mc| cons $x $xs => (x, xs) |]]
      `shouldBe` [(1,[2,3]),(2,[1,3]),(3,[1,2])]

    it "join pattern for multiset matcher" $ length (
      matchAll [1..5] (multiset integer)
        [[mc| join $xs $ys => (xs, ys) |]])
      `shouldBe` 32

    it "value pattern for multiset matcher" $
      match [1,2,3] (multiset integer)
        [[mc| #[2,1,3] => "Matched" |],
         [mc| _ => "Not matched" |]]
      `shouldBe` "Matched"

  describe "match-all with infinitely many results" $ do
    it "Check the order of pattern-matching results" $
      take 10 (matchAll [1..] (multiset integer)
                 [[mc| cons $x (cons $y _) => (x, y) |]])
      `shouldBe` [(1,2),(1,3),(2,1),(1,4),(2,3),(3,1),(1,5),(2,4),(3,2),(4,1)]

  describe "built-in pattern constructs" $ do
    it "Predicate patterns" $
      matchAll [1..10] (multiset integer)
        [[mc| cons (& (PredicatePat (\x -> mod x 2 == 0)) $x) _ => x |]]
      `shouldBe` [2,4,6,8,10]

  describe "patterns for prime numbers" $ do
    it "twin primes (p, p+2)" $
      take 10 (matchAll primes (list integer)
                 [[mc| join _ (cons $p (cons #(p+2) _)) => (p, p+2) |]])
      `shouldBe` [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]

    it "prime pairs whose form is (p, p+6) -- pattern matching with infinitely many results" $
      take 10 (matchAll primes (list integer)
                 [[mc| join _ (cons $p (join _ (cons #(p+6) _))) => (p, p+6) |]])
      `shouldBe` [(5,11),(7,13),(11,17),(13,19),(17,23),(23,29),(31,37),(37,43),(41,47),(47,53)]

    it "prime triplets -- and-patterns, or-patterns, and not-patterns" $
      take 10 (matchAll primes (list integer)
                 [[mc| join _ (cons $p (cons (& (| #(p+2) #(p+4)) $m) (cons #(p+6) _))) => (p, m, p+6) |]])
      `shouldBe` [(5,7,11),(7,11,13),(11,13,17),(13,17,19),(17,19,23),(37,41,43),(41,43,47),(67,71,73),(97,101,103),(101,103,107)]

  describe "Basic list processing functions" $ do
    it "map" $
      pmap (+ 10) [1,2,3] `shouldBe` [11, 12, 13]
    it "concat" $
      pmConcat [[1,2], [3], [4, 5]] `shouldBe` [1..5]
    -- it "uniq" $
    --   pmUniq [1,1,2,3,2] `shouldBe` [1,2,3]
