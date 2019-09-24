# miniEgison: Template Haskell Implementation of Egison Pattern Matching

This Haskell library provides the users with the pattern-matching facility against non-free data types.
Non-free data types are data types whose data have no standard forms.
For example, multisets are non-free data types because the multiset {a,b,b} has two other equivalent but literally different forms {b,a,b} and {b,b,a}.
This library provides the pattern-matching facility that fulfills the following three criteria for practical pattern matching for non-free data types: (i) non-linear pattern matching with backtracking; (ii) extensibility of pattern-matching algorithms; (iii) ad-hoc polymorphism of patterns.

The design of the pattern-matching facility is originally proposed in [this paper](https://arxiv.org/abs/1808.10603) and implemented in [the Egison programming language](http://github.com/egison/egison/).

## Grammar

This library provides two syntax constructs, `matchAll`, `match`, `matchAllDFS`, and `matchDFS` for advanced pattern matching for non-free data types.

```
e = hs-expr                 -- arbitrary Haskell expression
  | matchAll e e [C, ...]   -- match-all expression
  | match e e [C, ...]      -- match expression
  | matchAllDFS e e [C, ...]   -- match-all expression
  | matchDFS e e [C, ...]      -- match expression
  | something               -- Something built-in matcher

C = [mc| p => e]            -- match clause

p = _                       -- wildcard
  | $x                      -- pattern variable
  | #e                      -- value pattern
  | (& p ...)               -- and-pattern
  | (| p ...)               -- or-pattern
  | (not p)                 -- not-pattern
```

## Usage

### The `matchAll` expression and matchers

The `matchAll` expression evaluates the body of the match clause for all the pattern-matching results.
The expression below pattern-matches a target `[1,2,3]` as a list of integers with a pattern `cons $x $xs`.
This expression returns a list of a single element because there is only one decomposition.

```
matchAll [1,2,3] (list integer) [[mc| cons $x $xs => (x, xs)]]
-- [(1,[2,3])]
```

The other characteristic of `matchAll` is its additional argument matcher.
A matcher is a special object that retains the pattern-matching algorithms for each data type.
`matchAll` takes a matcher as its second argument.
We can change a way to interpret a pattern by changing a matcher.

For example, by changing the matcher of the above `matchAll` from `list integer` to `multiset integer`, the evaluation result changes as follows:

```
matchAll [1,2,3] (multiset integer) [[mc| cons $x $xs => (x, xs)]]
-- [(1,[2,3]),(2,[1,3]),(3,[1,2])]
```

When the `multiset` matcher is used, the `cons` pattern decomposes a target list into an element and the rest elements.

The pattern-matching algorithms for each matcher can be defined by users.
For example, the matchers such as `list` and `multiset` can be defined by users.
The `something` matcher is the only built-in matcher.
`something` can be used for pattern-matching arbitrary objects but can handle only pattern variables and wildcards.
The definitions of `list` and `multiset` are found [here](https://github.com/egison/egison-haskell/blob/master/src/Control/Egison/Matcher.hs).
We will write an explanation of this definition in future.

### Non-linear pattern

Non-linear pattern matching is another important feature of Egison pattern matching.
Non-linear patterns are patterns that allow multiple occurrences of the same pattern variables in a pattern.
For example, the program below pattern-matches a list `[1,2,5,9,4]` as a multiset and extracts pairs of sequential elements.
A non-linear pattern is effectively used for expressing the pattern.

```
matchAll [1,2,5,9,4] (multiset integer) [[mc| cons $x (cons #(x+1) _) => x]]
-- [1,4]
```

### The `match` expression

preparing...

### `matchAllDFS` and `matchDFS`

preparing...

## Samples

### Twin primes

We can extract all twin primes from the list of prime numbers by pattern matching:

```
take 10 (matchAll primes (list integer)
           [[mc| join _ (cons $p (cons #(p+2) _)) => (p, p+2) |]])
-- [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]
```

It is also possible to enumerate all the pairs of prime numbers whose form is (p, p+6):

```
take 10 (matchAll primes (list integer)
           [[mc| join _ (cons $p (join _ (cons #(p+6) _))) => (p, p+6) |]])
-- [(5,11),(7,13),(11,17),(13,19),(17,23),(23,29),(31,37),(37,43),(41,47),(47,53)]
```

### Poker hand

preparing...


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
  let ans = take n (matchAll primes (list integer)
                     [[mc| join _ (cons $p (cons #(p+2) _)) => (p, p+2) |]])
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
