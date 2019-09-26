{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TypeOperators         #-}

import Control.Egison hiding (Integer)
import qualified Control.Egison as M

data Card = Card Suit Integer
data Suit = Spade | Heart | Club | Diamond deriving (Eq)

data CardM = CardM
instance Matcher CardM

card :: Pattern Suit Eql ctx xs -> Pattern Integer M.Integer (ctx :++: xs) ys -> Pattern Card CardM ctx (xs :++: ys)
card p1 p2 = Pattern (\_ _ (Card s n) -> [MCons (MAtom p1 Eql s) $ MCons (MAtom p2 M.Integer n) MNil])

poker cs =
  match cs (Multiset CardM)
    [[mc| cons (card $s $n)
           (cons (card #s #(n-1))
            (cons (card #s #(n-2))
             (cons (card #s #(n-3))
              (cons (card #s #(n-4))
               _)))) => "Straight flush" |],
     [mc| cons (card _ $n)
           (cons (card _ #n)
            (cons (card _ #n)
             (cons (card _ #n)
              (cons _
               _)))) => "Four of a kind" |],
     [mc| cons (card _ $m)
           (cons (card _ #m)
            (cons (card _ #m)
             (cons (card _ $n)
              (cons (card _ #n)
                _)))) => "Full house" |],
     [mc| cons (card $s _)
           (cons (card #s _)
            (cons (card #s _)
             (cons (card #s _)
              (cons (card #s _)
               _)))) => "Flush" |],
     [mc| cons (card _ $n)
           (cons (card _ #(n-1))
            (cons (card _ #(n-2))
             (cons (card _ #(n-3))
              (cons (card _ #(n-4))
               _)))) => "Straight" |],
     [mc| cons (card _ $n)
           (cons (card _ #n)
            (cons (card _ #n)
             (cons _
              (cons _
               _)))) => "Three of a kind" |],
     [mc| cons (card _ $m)
           (cons (card _ #m)
            (cons (card _ $n)
             (cons (card _ #n)
              (cons _
                _)))) => "Two pair" |],
     [mc| cons (card _ $n)
           (cons (card _ #n)
            (cons _
             (cons _
              (cons _
               _)))) => "One pair" |],
     [mc| _ => "Nothing" |]]

main :: IO ()
main = do
  let cs1 = [Card Spade 5, Card Spade 6, Card Spade 7, Card Spade 8, Card Spade 9]
  let cs2 = [Card Spade 5, Card Diamond 5, Card Spade 7, Card Club 5, Card Heart 5]
  let cs3 = [Card Spade 5, Card Diamond 5, Card Spade 7, Card Club 5, Card Heart 7]
  let cs4 = [Card Spade 5, Card Spade 6, Card Spade 7, Card Spade 13, Card Spade 9]
  let cs5 = [Card Spade 5, Card Club 6, Card Spade 7, Card Spade 8, Card Spade 9]
  let cs6 = [Card Spade 5, Card Diamond 5, Card Spade 7, Card Club 5, Card Heart 8]
  let cs7 = [Card Spade 5, Card Diamond 10, Card Spade 7, Card Club 5, Card Heart 10]
  let cs8 = [Card Spade 5, Card Diamond 10, Card Spade 7, Card Club 5, Card Heart 8]
  let cs9 = [Card Spade 5, Card Spade 6, Card Spade 7, Card Spade 8, Card Diamond 11]
  putStrLn $ poker cs1
  putStrLn $ poker cs2
  putStrLn $ poker cs3
  putStrLn $ poker cs4
  putStrLn $ poker cs5
  putStrLn $ poker cs6
  putStrLn $ poker cs7
  putStrLn $ poker cs8
  putStrLn $ poker cs9
