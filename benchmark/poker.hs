{-# OPTIONS_GHC -fno-full-laziness #-}

import           Control.Egison          hiding ( Integer )
import qualified Control.Egison                as M

import           Data.Maybe                     ( fromJust )
import           GHC.Generics                   ( Generic )
import           Control.DeepSeq                ( NFData )
import qualified Game.Implement.Card.Standard  as Game
                                                ( Suit
                                                , Rank
                                                , PlayingCard(..)
                                                )
import qualified Game.Game.Poker               as Game
                                                ( mkHand
                                                , typeOfPokerHand
                                                , PokerHandType(..)
                                                , allPossibleHands
                                                )

import           Criterion.Main

data CardM = CardM
instance Matcher CardM Game.PlayingCard

card
  :: Pattern Game.Suit Eql ctx xs
  -> Pattern Int M.Integer (ctx :++: xs) ys
  -> Pattern Game.PlayingCard CardM ctx (xs :++: ys)
card p1 p2 = Pattern
  (\_ _ (Game.PlayingCard rank suit) ->
    [twoMAtoms (MAtom p1 Eql suit) (MAtom p2 M.Integer (rankNumber rank))]
  )
  where rankNumber r = fromEnum r + 1

data PokerHand
  = StraightFlush
  | FourOfAKind
  | FullHouse
  | Flush
  | Straight
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
  deriving Generic

instance NFData PokerHand

pokerHand :: [Game.PlayingCard] -> PokerHand
pokerHand cs = match
  cs
  (Multiset CardM)
  [ [mc| [card $s $n, card #s #(n-1), card #s #(n-2), card #s #(n-3), card #s #(n-4)] ->
          StraightFlush |]
  , [mc| [card _ $n, card _ #n, card _ #n, card _ #n, _] ->
          FourOfAKind |]
  , [mc| [card _ $m, card _ #m, card _ #m, card _ $n, card _ #n] ->
          FullHouse |]
  , [mc| [card $s _, card #s _, card #s _, card #s _, card #s _] ->
          Flush |]
  , [mc| [card _ $n, card _ #(n-1), card _ #(n-2), card _ #(n-3), card _ #(n-4)] ->
          Straight |]
  , [mc| [card _ $n, card _ #n, card _ #n, _, _] ->
          ThreeOfAKind |]
  , [mc| [card _ $m, card _ #m, card _ $n, card _ #n, _] ->
          TwoPair |]
  , [mc| [card _ $n, card _ #n, _, _, _] ->
          OnePair |]
  , [mc| _ -> HighCard |]
  ]

pokerHandGames :: [Game.PlayingCard] -> PokerHand
pokerHandGames = toPokerHand . Game.typeOfPokerHand . fromJust . Game.mkHand
 where
  toPokerHand Game.HighCard          = HighCard
  toPokerHand Game.Pair              = OnePair
  toPokerHand Game.TwoPair           = TwoPair
  toPokerHand Game.ThreeOfAKind      = ThreeOfAKind
  toPokerHand (Game.Straight _)      = Straight
  toPokerHand Game.Flush             = Flush
  toPokerHand Game.FullHouse         = FullHouse
  toPokerHand Game.FourOfAKind       = FourOfAKind
  toPokerHand (Game.StraightFlush _) = StraightFlush
  toPokerHand Game.RoyalFlush        = StraightFlush

main :: IO ()
main = defaultMain
  [bgroup "poker hands" [makeGroup 128, makeGroup 256, makeGroup 512, makeGroup 1024]]
 where
  makeGroup n = bgroup
    (show n)
    [ bench "miniEgison" $ nf (map pokerHand) $ hands n
    , bench "general-games" $ nf (map pokerHandGames) $ hands n
    ]
  hands n = take n $ Game.allPossibleHands
