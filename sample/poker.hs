{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}

import Control.Egison

data Card = Card Suit Integer
data Suit = Spade | Heart | Club | Diamond

poker cs =
  match cs (multiset eql)
    [[mc| cons (card $s $n)
           (cons (card #s #(n-1))
            (cons (card #s #(n-2))
             (cons (card #s #(n-3))
              (cons (card #s #(n-4))
               _)))) => "Straight flush"]
     [mc| _ => "Nothing"]]


