{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Egison hiding (Integer)
import qualified Control.Egison as M

data Card = Card Suit Integer
data Suit = Spade | Heart | Club | Diamond deriving (Eq)

data CardM = CardM
instance Matcher CardM Card

card :: Pattern Suit Eql ctx xs -> Pattern Integer M.Integer (ctx :++: xs) ys -> Pattern Card CardM ctx (xs :++: ys)
card p1 p2 = Pattern (\_ _ (Card s n) -> [twoMAtoms (MAtom p1 Eql s) (MAtom p2 M.Integer n)])

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
  putStrLn $ poker [Card Spade 5, Card Spade 6, Card Spade 7, Card Spade 8, Card Spade 9]    -- "Straight flush
  putStrLn $ poker [Card Spade 5, Card Diamond 5, Card Spade 7, Card Club 5, Card Heart 5]   -- "Four of a kind"
  putStrLn $ poker [Card Spade 5, Card Diamond 5, Card Spade 7, Card Club 5, Card Heart 7]   -- "Full house"
  putStrLn $ poker [Card Spade 5, Card Spade 6, Card Spade 7, Card Spade 13, Card Spade 9]   -- "Flush"
  putStrLn $ poker [Card Spade 5, Card Club 6, Card Spade 7, Card Spade 8, Card Spade 9]     -- "Straight"
  putStrLn $ poker [Card Spade 5, Card Diamond 5, Card Spade 7, Card Club 5, Card Heart 8]   -- "Three of a kind"
  putStrLn $ poker [Card Spade 5, Card Diamond 10, Card Spade 7, Card Club 5, Card Heart 10] -- "Two pair"
  putStrLn $ poker [Card Spade 5, Card Diamond 10, Card Spade 7, Card Club 5, Card Heart 8]  -- "One pair"
  putStrLn $ poker [Card Spade 5, Card Spade 6, Card Spade 7, Card Spade 8, Card Diamond 11] -- "Nothing"
