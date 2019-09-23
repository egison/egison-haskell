# Template Haskell Implementation of Egison Pattern Matching

### Twin primes

```
take 10 (matchAll primes (list integer)
           [[mc| join _ (cons $p (cons #(p+2) _)) => (p, p+2) |]])
-- [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43),(59,61),(71,73),(101,103),(107,109)]
```

## Sponsors

Egison is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
