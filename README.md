# Template Haskell Implementation of Egison Pattern Matching

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
