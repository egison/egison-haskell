# Template Haskell Implementation of Egison Pattern Matching

## Usage

`matchAll <target> <matcher> [ <match clause> ]`

`<matcher> ::= (list <matcher>) | (multiset <matcher>) | integer | eql | something`
`<match clause> ::= [mc| <pattern> => <expr> |]`

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.Numbers.Primes
import Control.Egison

twinPrimes = matchAll primes (list integer)
               [ [mc| JoinPat Wildcard (ConsPat $p (ConsPat #(p + 2) Wildcard))
                         => (p, p + 2) |] ]

take 10 twinPrimes
-- [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]
```
