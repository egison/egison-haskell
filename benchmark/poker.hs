{-# OPTIONS_GHC -fno-full-laziness #-}

import           Control.Egison

import           Data.List                      ( intercalate )
import           Data.Maybe                     ( fromJust )
import           Data.Functor                   ( ($>) )
import           Control.Monad                  ( guard )
import           Language.Haskell.TH.Syntax    as TH
                                                ( lift
                                                , runIO
                                                )
import qualified Game.Implement.Card.Standard  as Game
                                                ( Suit(..)
                                                , Rank
                                                , PlayingCard(..)
                                                )
import qualified Game.Game.Poker               as Game
                                                ( mkHand
                                                , typeOfPokerHand
                                                , PokerHandType
                                                )

import           BenchImport
import           PokerBenchImport
import           Criterion.Main


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

pokerHandGames :: [Game.PlayingCard] -> Game.PokerHandType
pokerHandGames = Game.typeOfPokerHand . fromJust . Game.mkHand

pokerHandEgison :: String -> IO EgisonExpr
pokerHandEgison cards = parseEgison expr
 where
  expr = unwords
    [ "let suit :="
    , suitMatcher
    , "\nin let cardM :="
    , cardMatcher
    , "\nin map ("
    , matchExpr
    , ")"
    , cards
    ]
  suitMatcher = intercalate
    "\n  | "
    ["algebraicDataMatcher", "spade", "heart", "diamond", "club"]
  cardMatcher =
    intercalate "\n  | " ["algebraicDataMatcher", "card suit (mod 13)"]
  matchExpr = intercalate
    "\n  "
    [ "\\match as multiset cardM with"
    , "| card $s $n :: card #s #(n-1) :: card #s #(n-2) :: card #s #(n-3) :: card #s #(n-4) :: []"
    , "  -> \"Straight flush\""
    , "| card _ $n :: card _ #n :: card _ #n :: card _ #n :: _ :: []"
    , "  -> \"Four of a kind\""
    , "| card _ $m :: card _ #m :: card _ #m :: card _ $n :: card _ #n :: []"
    , "  -> \"Full house\""
    , "| card $s _ :: card #s _ :: card #s _ :: card #s _ :: card #s _ :: []"
    , "  -> \"Flush\""
    , "| card _ $n :: card _ #(n-2) :: card _ #(n-2) :: card _ #(n-3) :: card _ #(n-4) :: []"
    , "  -> \"Straight\""
    , "| card _ $n :: card _ #n :: card _ #n :: _ :: _ :: []"
    , "  -> \"Three of a kind\""
    , "| card _ $m :: card _ #m :: card _ $n :: card _ #n :: _ :: []"
    , "  -> \"Two pair\""
    , "| card _ $n :: card _ #n :: _ :: _ :: _ :: []"
    , "  -> \"One pair\""
    , "| _ :: _ :: _ :: _ :: _ :: [] -> \"Nothing\""
    ]

main :: IO ()
main = defaultMain
  [ bgroup
      "poker hands"
      [ makeGroup 50
      , makeGroup 100
      , makeGroup 200
      , makeGroup 400
      , makeGroup 800
      , makeGroup 1600
      ]
  ]
 where
  makeGroup n =
    bgroup (show n)
      $  [ bench "general-games" . nf (map pokerHandGames) $ hands n
         , bench "miniEgison" . nf (map pokerHand) $ hands n
         ]
      ++ (guard (n < 500) $> egisonBench n)
  egisonBench n =
    env (pokerHandEgison . renderHandsForEgison $ hands n)
      $ bench "egison"
      . whnfIO
      . evalEgison
  renderHandsForEgison = list . map (list . map renderCardForEgison)
  list xs = "[ " ++ intercalate ", " xs ++ " ]"
  renderCardForEgison (Game.PlayingCard rank suit) =
    unwords ["Card", renderSuitForEgison suit, renderRankForEgison rank]
  renderSuitForEgison Game.Spades   = "Spade"
  renderSuitForEgison Game.Hearts   = "Heart"
  renderSuitForEgison Game.Diamonds = "Diamond"
  renderSuitForEgison Game.Clubs    = "Club"
  renderRankForEgison r = show $ fromEnum r + 1
  hands n = take
    n
    $(TH.lift =<< read @[[Game.PlayingCard]] <$> TH.runIO (readFile "./benchmark/cards.txt"))
