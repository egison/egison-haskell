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
             $ [mc| joinPat Wildcard (consPat $x Wildcard) => f x |] .*. PNil

pmConcat :: [[a]] -> [a]
pmConcat xss = matchAll xss (multiset (multiset something))
               $ [mc| consPat (consPat $x Wildcard) Wildcard => x |] .*. PNil
                 -- [ [mc| ConsPat (ConsPat $x Wildcard) Wildcard => x |] ]

-- pmUniq :: Eq a => [a] -> [a]
-- pmUniq xs = matchAll xs (list eql)
--               [ [mc| JoinPat (LaterPat (NotPat (JoinPat Wildcard (ConsPat #x Wildcard)))) (ConsPat $x Wildcard) => x|] ]


spec :: Spec
spec = do
  describe "match and matchAll" $ do
    it "list cons pattern" $
      match [1,2,5,9,4] (list integer) ([mc| consPat $x $xs => (x, xs) |] .*. PNil)
      `shouldBe` (1, [2,5,9,4])

    it "multiset cons pattern" $
      matchAll [1,2,5,9,4] (multiset integer) ([mc| consPat $x $xs => (x, xs) |] .*. PNil)
      `shouldBe` [(1,[2,5,9,4]),(2,[1,5,9,4]),(5,[1,2,9,4]),(9,[1,2,5,4]),(4,[1,2,5,9])]

    it "pattern matching with infinitely many results" $
      take 10 (matchAll primes (list integer)
                ([mc| joinPat Wildcard (consPat $p (joinPat Wildcard (consPat #(p+6) Wildcard))) => (p, p+6) |] .*. PNil))
      `shouldBe` [(5,11),(7,13),(11,17),(13,19),(17,23),(23,29),(31,37),(37,43),(41,47),(47,53)]

    it "Value patterns, and-patterns, or-patterns, and not-patterns" $
      matchAll [1,2,5,9,4] (multiset integer)
        ([mc| consPat (AndPat (NotPat #5) $x) (consPat (AndPat (OrPat #5 #2) $y) $xs) => (x, y, xs) |] .*. PNil)
      `shouldBe` [(1,2,[5,9,4]),(1,5,[2,9,4]),(2,5,[1,9,4]),(9,2,[1,5,4]),(9,5,[1,2,4]),(4,2,[1,5,9]),(4,5,[1,2,9])]

    it "joinpat in multiset matcher" $ length (
      matchAll [1..5] (multiset integer)
        ([mc| joinPat $xs $ys => (xs, ys) |] .*. PNil))
      `shouldBe` 32

    -- it "Later pattern" $
    --   match [1..5] (list integer)
    --     [ [mc| ConsPat (LaterPat #(x - 1)) (ConsPat $x $xs) => (x, xs) |] ]
    --   `shouldBe` (2,[3,4,5])

    it "value pattern in multiset matcher" $
      matchAll [1,2,3] (multiset integer)
        ([mc| #[2,1,3] => "Matched" |] .*. PNil)
      `shouldBe` ["Matched"]

    it "Check the order of pattern-matching results" $
      take 10 (matchAll [1..] (multiset integer)
                ([mc| consPat $x (consPat $y Wildcard) => (x, y) |] .*. PNil))
      `shouldBe` [(1,2),(1,3),(2,1),(1,4),(2,3),(3,1),(1,5),(2,4),(3,2),(4,1)]

    it "Predicate patterns" $
      matchAll [1..10] (multiset integer)
        ([mc| consPat (AndPat (PredicatePat (\x -> mod x 2 == 0)) $x) Wildcard => x |] .*. PNil)
      `shouldBe` [2,4,6,8,10]

  describe "Basic list processing functions" $ do
    it "map" $
      pmap (+ 10) [1,2,3] `shouldBe` [11, 12, 13]
    it "concat" $
      pmConcat [[1,2], [3], [4, 5]] `shouldBe` [1..5]
    -- it "uniq" $
    --   pmUniq [1,1,2,3,2] `shouldBe` [1,2,3]
