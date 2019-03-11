# Template Haskell Implementation of Egison Pattern Matching

## matchAll and match

- match \<target\> \<matcher\> [ [mc| \<pattern\> => \<expr\> |] ]

    targetをpatternでパターンマッチし，最初にマッチしたものについてexprを返す。

    matcherによってパターンマッチの仕方を変えることができる。

- matchAll \<target\> \<matcher\> [ [mc| \<pattern\> => \<expr\> |] ]

    targetをpatternでパターンマッチし，マッチしたものすべてについてexprを返す。(exprのリストが返り値。)

    matcherによってパターンマッチの仕方を変えることができる。

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

## Patterns

- Wildcard

    任意のオブジェクトがマッチする。

- PatVar    ($\<identifier\>)

    パターン内で変数に束縛することができる。
    ここで束縛した変数は同一のmatch clause内で使える。

    ```
    match [1,2,3] (list integer) [ [mc| ConsPat $x Wildcard => x |] ]
    -- 1
    ```

- ValuePat  (#\<expr\>)

    パターンの中身とオブジェクトが等しいときにマッチする。

    ターゲットがEqのときのみ使うことができる。

    ```
    match [1,2,3] (list integer) [ [mc| ConsPat #1 Wildcard => "ok" |] ]
    -- "ok"
    ```

- AndPat    (AndPat \<pat1\> \<pat2\>)

    2つのパターンを引数に取り，両方にマッチするときに成功する。

    ```
    match [1,2,3] (list integer) [ [mc| ConsPat (AndPat #1 $x) Wildcard => x |] ]
    -- 1
    ```

- OrPat     (OrPat \<pat1\> \<pat2\>)

    2つのパターンを引数に取り，どちらか一方もしくは両方にマッチするときに成功する。

    ```
    matchAll [1,2,3] (multiset integer) [ [mc| ConsPat (OrPat #1 #2) $xs => xs |] ]
    -- [[2,3], [1,3]]
    ```

- NotPat    (NotPat \<pat\>)

    1つのパターンを引数に取り，そのパターンにマッチしないとき成功する。

    ```
    matchAll [1,2,3] (multiset integer) [ [mc| ConsPat (NotPat #1) $xs => xs |] ]
    -- [[1,3], [1,2]]
    ```

- LaterPat  (LaterPat \<pat\>)

    パターンマッチの順番を変えるためのパターン。
    通常左から右に向かって進むが，LaterPatで囲ったパターンは後回しになる。

    下の例のように，LaterPatを使うとPatVarで束縛した変数をPatVarより左のパターンでも用いることができる。

    ```
    match [1,1,2,3] (list integer) [ [mc| ConsPat (LaterPat #x) (ConsPat $x Wildcard)  => x |] ]
    -- 1
    ```

- PredicatePat (PredicatePat \<function\>)

    オブジェクトを受け取って真偽値を返す関数を引数に取り，関数を適用してTrueのときマッチする。

    ```
    matchAll [1..10] (multiset integer) [ [mc| ConsPat (AndPat (PredicatePat (\x -> mod x 2 == 0)) $x) Wildcard => x |] ]
    -- [2,4,6,8,10]
    ```
- NilPat

    collection pattern (matcherがlistやmultisetを用いているときに使うことができる)。
    空のリストにマッチする。

- ConsPat   (ConsPat \<pat1\> \<pat2\>)

    collection pattern。
    オブジェクトを`x:xs`に分け，xがpat1にマッチし，xsがpat2にマッチするとき成功する。

    ```
    match [1,2,3] (list integer) [ [mc| ConsPat $x $xs => (x, xs) |] ]
    -- [(1, [2, 3])]
    ```

- JoinPat   (JoinPat \<pat1\> \<pat2\>)

    collection pattern。
    オブジェクトを`xs++ys`に分け，xsがpat1にマッチし，ysがpat2にマッチするとき成功する。

    ```
    matchAll [1,2,3] (list integer) [ [mc| JoinPat $xs $ys => (xs, ys) |] ]
    -- [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]
    ```

## matcher

- eql

    ValuePatを使うことができる。

- integer

    eqlと同じ。

- list  (list \<matcher\>)

    1つのmatcherを引数に取り，それがリストの要素に対応するmatcherになる。

    ```
    matchAll [1,2,3] (list integer) [ [mc| ConsPat $x Wildcard => x |] ]
    -- [1]
    ```

- multiset  (multiset \<matcher\>)

    1つのmatcherを引数に取り，それがリストの要素に対応するmatcherになる。

    リスト内の要素の順番を無視してパターンマッチを行う点がlistと異なる。

    ```
    matchAll [1,2,3] (multiset integer) [ [mc| ConsPat $x Wildcard => x |] ]
    -- [1,2,3]
    ```
