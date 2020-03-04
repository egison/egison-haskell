# miniEgison: Template Haskell Implementation of Egison Pattern Matching
[![Build Status](https://travis-ci.org/egison/egison-haskell.svg?branch=master)](https://travis-ci.org/egison/egison-haskell)

This Haskell library provides the users with the pattern-matching facility against non-free data types.
Non-free data types are data types whose data have no standard forms.
For example, multisets are non-free data types because the multiset {a,b,b} has two other equivalent but literally different forms {b,a,b} and {b,b,a}.
This library provides the pattern-matching facility that fulfills the following three criteria for practical pattern matching for non-free data types: (i) non-linear pattern matching with backtracking; (ii) extensibility of pattern-matching algorithms; (iii) ad-hoc polymorphism of patterns.

The design of the pattern-matching facility is originally proposed in [this paper](https://arxiv.org/abs/1808.10603) and implemented in [the Egison programming language](http://github.com/egison/egison/).

## Grammar

This library provides two syntax constructs, `matchAll`, `match`, `matchAllDFS`, and `matchDFS` for advanced pattern matching for non-free data types.

```
e = hs-expr                    -- arbitrary Haskell expression
  | matchAll e e [C, ...]      -- match-all expression
  | match e e [C, ...]         -- match expression
  | matchAllDFS e e [C, ...]   -- match-all expression
  | matchDFS e e [C, ...]      -- match expression
  | Something                  -- Something built-in matcher

C = [mc| p -> e |]             -- match clause

p ::= _                     -- wildcard pattern
    | $v                    -- pattern variable
    | #e                    -- value pattern
    | ?e                    -- predicate pattern
    | (p_1, p_2, ..., p_n)  -- tuple pattern
    | [p_1, p_2, ..., p_n]  -- collection pattern
    | p & p                 -- and-pattern
    | p | p                 -- or-pattern
    | !p                    -- not-pattern
    | c p_1 p_2 ... p_n     -- constructor pattern
```

## Usage

### The `matchAll` expression and matchers

The `matchAll` expression evaluates the body of the match clause for all the pattern-matching results.
The expression below pattern-matches a target `[1,2,3]` as a list of integers with a pattern `cons $x $xs`.
This expression returns a list of a single element because there is only one decomposition.

```
matchAll [1,2,3] (List Integer) [[mc| $x : $xs -> (x, xs)|]]
-- [(1,[2,3])]
```

The other characteristic of `matchAll` is its additional argument matcher.
A matcher is a special object that retains the pattern-matching algorithms for each data type.
`matchAll` takes a matcher as its second argument.
We can change a way to interpret a pattern by changing a matcher.

For example, by changing the matcher of the above `matchAll` from `List Integer` to `Multiset Integer`, the evaluation result changes as follows:

```
matchAll [1,2,3] (Multiset Integer) [[mc| $x : $xs -> (x, xs)|]]
-- [(1,[2,3]),(2,[1,3]),(3,[1,2])]
```

When the `Multiset` matcher is used, `:` (the cons pattern) decomposes a target list into an element and the rest elements.

The pattern-matching algorithms for each matcher can be defined by users.
For example, the matchers such as `List` and `Multiset` can be defined by users.
The `Something` matcher is the only built-in matcher.
`something` can be used for pattern-matching arbitrary objects but can handle only pattern variables and wildcards.
The definitions of `List` and `Multiset` are found [here](https://github.com/egison/egison-haskell/blob/master/src/Control/Egison/Matcher.hs).
We will write an explanation of this definition in future.

### Non-linear pattern

Non-linear pattern matching is another important feature of Egison pattern matching.
Non-linear patterns are patterns that allow multiple occurrences of the same pattern variables in a pattern.
For example, the program below pattern-matches a list `[1,2,5,9,4]` as a multiset and extracts pairs of sequential elements.
A non-linear pattern is effectively used for expressing the pattern.

```
matchAll [1,2,5,9,4] (Multiset Integer) [[mc| $x :#(x+1) : _ -> x|]]
-- [1,4]
```

### The `match` expression

The `match` expression takes a target, a matcher, and match-clauses as the `matchAll` expression.
The `match` expression returns only the evaluation result of the first pattern-matching result.

```
match [1,2,5,9,4] (Multiset Integer) [[mc| $x :#(x+1) : _ -> x|]]
-- 1
```

The `match` expression is simply implemented using `matchAll` as follows:

```
match tgt m cs = head $ matchAll tgt m cs
```

### `matchAllDFS` and `matchDFS`

The `matchAll` and `match` expressions traverse a search tree for pattern matching in breadth-first order.
The reason of the default breadth-first traversal is because to enumerate all the successful pattern-matching results even when they are infinitely many.
For example, all the pairs of natural numbers can be enumerated by the following `matchAll` expression:

```
take 10 (matchAll [1..] (Set Integer)
           [[mc| $x : $y : _ -> (x, y) |]])
-- [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),(4,1)]
```

If we change the above `matchAll` to `matchAllDFS`, the order of the pattern-matching results changes as follows:

```
take 10 (matchAllDFS [1..] (Set Integer)
           [[mc| $x : $y : _ -> (x, y) |]])
-- [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10)]
```

There are cases where depth-first traversal is suitable because the depth-first order of pattern-matching results is preferable.
Furthermore, `matchAllDFS` is more efficient than `matchAll`.
It would be better to use `matchAllDFS` instead of `matchAll` when the both expressions can be used.

### Matcher definitions

The users can define pattern-matching algorithms for each pattern by themselves.

preparing...

```
matchAll (1,2) UnorderedEqlPair [[mc| uepair $x $y -> (x,y) |]]
-- [(1,2),(2,1)]

matchAll (1,2) UnorderedEqlPair [[mc| uepair #2 $x -> x |]]
-- [1]
```

A matcher is represented as a data type whose name and constructor's name is identical.

preparing...

```
data UnorderedEqlPair = UnorderedEqlPair
instance (Eq a) -> Matcher UnorderedEqlPair (a, a)

uepair :: (Eq a)
       => Pattern a Eql ctx xs
       -> Pattern a Eql (ctx :++: xs) ys
       -> Pattern (a, a) UnorderedEqlPair ctx (xs :++: ys)
uepair p1 p2 = Pattern (\_ UnorderedEqlPair (t1, t2) ->
                          [twoMAtoms (MAtom p1 Eql t1) (MAtom p2 Eql t2)
                          ,twoMAtoms (MAtom p1 Eql t2) (MAtom p2 Eql t1)])
```

```
matchAll (1,2) (UnorderedPair Eql) [[mc| uepair $x $y => (x,y) |]]
-- [(1,2),(2,1)]

matchAll (1,2) (UnorderedPair Eql) [[mc| upair #2 $x => x |]]
-- [1]
```

```
data UnorderedPair m = UnorderedPair m
instance Matcher m a => Matcher (UnorderedPair m) (a, a)

upair :: (Matcher m a , a ~ (b, b), m ~ (UnorderedPair m'), Matcher m' b)
      => Pattern b m' ctx xs
      -> Pattern b m' (ctx :++: xs) ys
      -> Pattern a m ctx (xs :++: ys)
upair p1 p2 = Pattern (\_ (UnorderedPair m') (t1, t2) ->
                         [twoMAtoms (MAtom p1 m' t1) (MAtom p2 m' t2)
                         ,twoMAtoms (MAtom p1 m' t2) (MAtom p2 m' t1)])
```

## Samples

### Twin primes

We can extract all twin primes from the list of prime numbers by pattern matching:

```
take 10 (matchAll primes (List Integer)
           [[mc| _ ++ $p : #(p+2) : _ -> (p, p+2) |]])
-- [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]
```

It is also possible to enumerate all the pairs of prime numbers whose form is (p, p+6):

```
take 10 (matchAll primes (List Integer)
           [[mc| _ ++ $p : _ ++ #(p+6) : _ -> (p, p+6) |]])
-- [(5,11),(7,13),(11,17),(13,19),(17,23),(23,29),(31,37),(37,43),(41,47),(47,53)]
```

### Poker hand

```
poker cs =
  match cs (Multiset CardM)
    [[mc| card $s $n :
           card #s #(n-1) :
            card #s #(n-2) :
             card #s #(n-3) :
              card #s #(n-4) :
               [] -> "Straight flush" |],
     [mc| card _ $n :
           card _ #n :
            card _ #n :
             card _ #n :
              _ :
               [] -> "Four of a kind" |],
     [mc| card _ $m :
           card _ #m :
            card _ #m :
             card _ $n :
              card _ #n :
               [] -> "Full house" |],
     [mc| card $s _ :
           card #s _ :
            card #s _ :
             card #s _ :
              card #s _ :
               [] -> "Flush" |],
     [mc| card _ $n :
           card _ #(n-1) :
            card _ #(n-2) :
             card _ #(n-3) :
              card _ #(n-4) :
               [] -> "Straight" |],
     [mc| card _ $n :
           card _ #n :
            card _ #n :
             _ :
              _ :
               [] -> "Three of a kind" |],
     [mc| card _ $m :
           card _ #m :
            card _ $n :
             card _ #n :
              _ :
               [] -> "Two pair" |],
     [mc| card _ $n :
           card _ #n :
            _ :
             _ :
              _ :
               [] -> "One pair" |],
     [mc| _ -> "Nothing" |]]
```

## Benchmark

We benchmarked this library using the program that enumerates the first 100 twin primes.
This Haskell library is faster (more than 20 times in this case) than the original Egison interpreter!

```
$ cat benchmark/prime-pairs-2.hs
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs           #-}

import Control.Egison
import Data.Numbers.Primes

main :: IO ()
main = do
  let n = 100
  let ans = take n (matchAll primes (List Integer)
                     [[mc| _ ++ $p : #(p+2) : _ -> (p, p+2) |]])
  putStrLn $ show ans
$ stack ghc -- benchmark/prime-pairs-2.hs
$ time ./benchmark/prime-pairs-2
[(3,5),(5,7),(11,13), ..., (3671,3673),(3767,3769),(3821,3823)]
./benchmark/prime-pairs-2  0.01s user 0.01s system 64% cpu 0.024 total
```

```
$ cat benchmark/prime-pairs-2.egi
(define $n 100)
(define $primes {2 3 5 7 11 13 17 ... 4391 4397 4409})

(define $twin-primes
  (match-all primes (list integer)
    [<join _ <cons $p <cons ,(+ p 2) _>>>
     [p (+ p 2)]]))

(take n twin-primes)
$ time stack exec egison -- -t benchmark/prime-pairs-2.egi
{[3 5] [5 7] [11 13] ... [3671 3673] [3767 3769] [3821 3823]}
stack exec egison -- -t benchmark/prime-pairs-2.egi  0.54s user 0.04s system 97% cpu 0.593 total
```

## Sponsors

Egison is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
