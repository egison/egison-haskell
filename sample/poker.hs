{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}

import Control.Egison

data Card = Card Suit Integer deriving (Show, Eq)
data Suit = Spade | Heart | Club | Diamond deriving (Show, Eq)

class CardPat mt a where
  card :: Pattern 

poker cs =
  match cs (multiset eql)
    [[mc| cons (card $s $n)
           (cons (card #s #(n-1))
            (cons (card #s #(n-2))
             (cons (card #s #(n-3))
              (cons (card #s #(n-4))
               _)))) => "Straight flush"]
     [mc| _ => "Nothing"]]


main :: IO ()
main = do
  let cs1 = [Card Spade 5, Card Spade 6, Card Spade 7, Card Spade 8, Card Spade 9]
  let cs2 = [Card Spade 5, Card Spade 6, Card Spade 7, Card Spade 8, Card Diamond 11]
  putStrLn $ poker cs1
  putStrLn $ poke cs2
