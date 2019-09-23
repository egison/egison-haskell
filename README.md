# miniEgison: Template Haskell Implementation of Egison Pattern Matching

This Haskell library provides the users with the pattern-matching facility against non-free data types.
This pattern-matching facility is originally proposed in [this paper](https://arxiv.org/abs/1808.10603) and implemented in [the Egison programming language](http://github.com/egison/egison/).

## Grammar

This library provides two syntax constructs, `matchAll` and `match` for advanced pattern matching for non-free data types.

```
e = hs-expr                 -- arbitrary Haskell expression
  | matchAll e e [C, ...]   -- match-all expression
  | match e e [C, ...]      -- match expression
  | something               -- Something built-in matcher

C = [mc| p => e]            -- match clause

p = _                       -- wildcard
  | $x                      -- pattern variable
  | #e                      -- value pattern
  | c p ...                 -- constructor pattern
```

## Usage

Non-free data types are data types whose data have no standard forms.
For example, multisets are non-free data types because the multiset {a,b,b} has two other equivalent but literally different forms {b,a,b} and {b,b,a}.
This library provides the users with a pattern-matching facility for these non-free data types.

For example, the following program pattern-matches a list `[1,2,5,9,4]` as a multiset.
This pattern matches if the target collection contains pairs of elements in sequence.
A non-linear pattern is effectively used for expressing the pattern.
`matchAll` returns a list of all the results.

```
matchAll [1,2,5,9,4] (multiset integer) [[mc| cons $x (cons #(x+1) _) => x]]
-- [1,4]
```

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

## Sponsors

Egison is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
