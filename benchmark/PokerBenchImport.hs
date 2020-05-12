module PokerBenchImport where

import           Control.Egison          hiding ( Integer )
import qualified Control.Egison                as M

import           GHC.Generics                   ( Generic )
import           Control.DeepSeq                ( NFData )
import qualified Game.Implement.Card.Standard  as Game
                                                ( Suit(..)
                                                , Rank(..)
                                                , PlayingCard(..)
                                                )
import qualified Game.Game.Poker               as Game
                                                ( AceRank(..)
                                                , PokerHandType(..)
                                                )
import           Language.Haskell.TH.Syntax    as TH
                                                ( Lift(..) )

deriving instance TH.Lift Game.Rank
deriving instance TH.Lift Game.Suit
deriving instance TH.Lift Game.PlayingCard

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

deriving instance Generic Game.AceRank
instance NFData Game.AceRank

deriving instance Generic Game.PokerHandType
instance NFData Game.PokerHandType
