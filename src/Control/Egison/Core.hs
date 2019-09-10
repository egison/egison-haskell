{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Control.Egison.Core (
  MState(..),
  MAtom(..),
  Matcher(..),
  HList(..),
  MList(..),
  PList(..),
  (.*.),
  happend,
  (:++:),
  Pattern(..),
  ) where

import           Data.Maybe
import           Unsafe.Coerce

--
-- Matching states
--

data MState vs where
  MState :: vs ~ (xs :++: ys) => HList xs -> MList xs ys -> MState vs

data MAtom ctx vs = forall a m. MAtom (Pattern a ctx (Matcher m) vs) a m
newtype Matcher a = Matcher a

data HList xs where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

data MList ctx vs where
  MNil :: MList ctx '[]
  MCons :: MAtom ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)
  MJoin :: MList ctx xs -> MList (ctx :++: xs) ys -> MList ctx (xs :++: ys)

data PList a m b where
  PNil :: PList a m b
  PCons :: (Pattern a '[] (Matcher m) vs, HList vs -> b) -> PList a m b -> PList a m b

(.*.) :: (Pattern a '[] (Matcher m) vs, HList vs -> b) -> PList a m b -> PList a m b
x .*. xs = PCons x xs

infixr 0 .*.

happend :: HList as -> HList bs -> HList (as :++: bs)
happend (HCons x xs) ys = unsafeCoerce $ HCons x $ happend xs ys
happend HNil ys         = ys

type family as :++: bs :: [*] where
  bs :++: '[] = bs
  '[] :++: bs = bs
  (a ': as') :++: bs = a ': (as' :++: bs)

--
-- Pattern
--

data Pattern a ctx mt vs where
  Pattern :: mt ~ Matcher m => (a -> HList ctx -> m -> [MList ctx vs]) -> Pattern a ctx mt vs

  Wildcard :: Pattern a ctx mt '[]
  PatVar :: String -> Pattern a ctx mt '[a]
  AndPat :: Pattern a ctx mt vs -> Pattern a (ctx :++: vs) mt vs' -> Pattern a ctx mt (vs :++: vs')
  OrPat  :: Pattern a ctx mt vs -> Pattern a ctx mt vs -> Pattern a ctx mt vs
  NotPat :: Pattern a ctx mt '[] -> Pattern a ctx mt '[]
  PredicatePat :: (HList ctx -> a -> Bool) -> Pattern a ctx mt '[]
