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
  mlToHList,
  maToHList,
  proof2,
  hsingle,
  (:~:)(..),
  Pattern(..),
  ) where

import           Data.Maybe

--
-- Matching states
--

data MState vs where
  MState :: vs ~ (xs :++: ys) => MList xs ys -> HList xs -> MState vs

data MAtom ctx vs = forall a m. MAtom (Pattern a ctx (Matcher m) vs) a m
newtype Matcher a = Matcher a

data HList xs where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

  HPat :: Pattern a ctx mt vs -> HList vs
  HJoin :: HList as -> HList bs -> HList (as :++: bs)

data MList xs ys where
  MNil :: MList xs '[]
  MCons :: MAtom xs xs1 -> MList (xs :++: xs1) ys1 -> MList xs (xs1 :++: ys1)
  MJoin :: MList xs xs1 -> MList (xs :++: xs1) ys1 -> MList xs (xs1 :++: ys1)

data PList a m b where
  PNil :: PList a m b
  PCons :: (Pattern a '[] (Matcher m) vs, HList vs -> b) -> PList a m b -> PList a m b

(.*.) :: (Pattern a '[] (Matcher m) vs, HList vs -> b) -> PList a m b -> PList a m b
x .*. xs = PCons x xs

infixr 5 .*.
infixr 5 :++:

happend :: HList as -> HList bs -> HList (as :++: bs)
happend (HCons x xs) ys = case proof4 x xs ys of Refl -> HCons x $ happend xs ys
happend HNil ys         = ys
happend xs ys = HJoin xs ys

type family as :++: bs :: [*] where
  bs :++: '[] = bs
  '[] :++: bs = bs
  (a ': as) :++: bs = a ': (as :++: bs)

data (a :: [*]) :~: (b :: [*]) where
  Refl :: a :~: a

mlToHList :: MList xs ys -> HList ys
mlToHList MNil = HNil
mlToHList (MCons matom xs) = happend (maToHList matom) $ mlToHList xs

maToHList :: MAtom xs ys -> HList ys
maToHList (MAtom Wildcard tgt m) = HNil
maToHList (MAtom (PatVar _) tgt m) = hsingle tgt
maToHList (MAtom (AndPat p1 p2) tgt m) = happend (maToHList $ MAtom p1 tgt m) (maToHList $ MAtom p2 tgt m)
maToHList (MAtom (OrPat p1 p2) tgt m) = maToHList $ MAtom p1 tgt m
maToHList (MAtom (PredicatePat p) tgt m) = HNil
maToHList (MAtom pat tgt m) = HPat pat

hadd :: HList as -> a -> HList (as :++: '[a])
hadd HNil y = HCons y HNil
hadd (HCons x xs) y = HCons x $ hadd xs y

hsingle :: a -> HList '[a]
hsingle x = HCons x HNil

proof1 :: HList as -> (as :++: '[]) :~: as
proof1 (HCons x xs) = case proof1 xs of Refl -> Refl
proof1 _ = Refl

proof2 :: HList as -> HList bs -> HList cs -> (as :++: (bs :++: cs)) :~: ((as :++: bs) :++: cs)
proof2 l1 HNil l3 = case proof1 l1 of Refl -> Refl
proof2 l1 (HCons x xs) l3 = case proof4 x xs l3 of
                              Refl -> case proof3 l1 (HCons x (happend xs l3)) of
                                        Refl -> case proof2 (hadd l1 x) xs l3 of
                                                  Refl -> case proof3 l1 (HCons x xs) of
                                                            Refl -> Refl

proof3 :: HList as -> HList (b ': bs) -> (as :++: (b ': bs)) :~: ((as :++: '[b]) :++: bs)
proof3 HNil (HCons y ys) = case proof5 y ys of Refl -> Refl
proof3 (HCons x xs) l2@(HCons y ys) = case proof4 x xs l2 of
                           Refl -> case proof4 x xs (hsingle ys) of
                                     Refl -> case proof4 x (hadd xs y) ys of
                                               Refl -> case proof3 xs l2 of Refl -> Refl

proof4 :: a -> HList as -> HList bs -> ((a ': as) :++: bs) :~: (a ': (as :++: bs))
proof4 _ _ HNil = Refl
proof4 x xs (HCons y ys) = Refl

proof5 :: a -> HList as -> ('[a] :++: as) :~: (a ': as)
proof5 x HNil = Refl
proof5 x (HCons y ys) = Refl

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
