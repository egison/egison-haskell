{-# LANGUAGE QuasiQuotes #-}

module Spec (spec) where

import           Control.Egison
import           Data.Numbers.Primes
import           Test.Hspec

--
-- Basic list processing functions in pattern-matching-oriented programming style
--

pmMap :: (a -> b) -> [a] -> [b]
pmMap f xs = matchAll xs (list something)
               [ [mc| UserPat "Join" [Pat Wildcard, Pat (ConsPat $x Wildcard)] => f x|] ]

pmConcat :: [[a]] -> [a]
pmConcat xss = matchAll xss (multiset (multiset something))
                 [ [mc| ConsPat (ConsPat $x Wildcard) Wildcard => x |] ]

pmUniq :: Eq a => [a] -> [a]
pmUniq xs = matchAll xs (list eql)
              [ [mc| JoinPat (LaterPat (NotPat (JoinPat Wildcard (ConsPat #x Wildcard)))) (ConsPat $x Wildcard) => x|] ]


spec :: Spec
spec = do
  describe "match and matchAll" $ do
    it "List cons pattern" $
      match [1,2,5,9,4] (list integer) [ [mc| ConsPat $x $xs => (x, xs) |] ]
      `shouldBe` (1, [2,5,9,4])

    it "Multiset cons pattern" $
      matchAll [1,2,5,9,4] (multiset integer) [ [mc| ConsPat $x $xs => (x, xs) |] ]
      `shouldBe` [(1,[2,5,9,4]),(2,[1,5,9,4]),(5,[1,2,9,4]),(9,[1,2,5,4]),(4,[1,2,5,9])]

    it "Twin primes (pattern matching with infinitely many results)" $
      take 10 (matchAll primes (list integer)
                [ [mc| UserPat "Join" [Pat Wildcard, Pat (ConsPat $p (ConsPat #(p+2) Wildcard))] => (p, p+2) |] ])
      `shouldBe` [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]

    it "Value patterns, and-patterns, or-patterns, and not-patterns" $
      matchAll [1,2,5,9,4] (multiset integer)
        [ [mc| ConsPat (AndPat (NotPat #5) $x) (ConsPat (AndPat (OrPat #5 #2) $y) $xs) => (x, y, xs) |] ]
      `shouldBe` [(1,2,[5,9,4]),(1,5,[2,9,4]),(2,5,[1,9,4]),(9,2,[1,5,4]),(9,5,[1,2,4]),(4,2,[1,5,9]),(4,5,[1,2,9])]

    it "Later pattern" $
      match [1..5] (list integer)
        [ [mc| ConsPat (LaterPat #(x - 1)) (ConsPat $x $xs) => (x, xs) |] ]
      `shouldBe` (2,[3,4,5])

    it "Check the order of pattern-matching results" $
      take 10 (matchAll [1..] (multiset integer)
                [ [mc| ConsPat $x (ConsPat $y Wildcard) => (x, y) |] ])
      `shouldBe` [(1,2),(1,3),(2,1),(1,4),(2,3),(3,1),(1,5),(2,4),(3,2),(4,1)]

    it "Predicate patterns" $
      matchAll [1..10] (multiset integer)
        [ [mc| ConsPat (AndPat (PredicatePat (\x -> mod x 2 == 0)) $x) Wildcard => x |] ]
      `shouldBe` [2,4,6,8,10]

  describe "Basic list processing functions" $ do
    it "map" $
      pmMap (+ 10) [1,2,3] `shouldBe` [11, 12, 13]
    it "concat" $
      pmConcat [[1,2], [3], [4, 5]] `shouldBe` [1..5]
    it "uniq" $
      pmUniq [1,1,2,3,2] `shouldBe` [1,2,3]
