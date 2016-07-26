module Test.Main where

import CTPrelude

-- Prove that `Bool` is isomorphic to `One ⊕ One`.
data Bool = True | False

instance i0 :: Isomorphism1 Bool (One ⊕ One) where
  fwd1 True  = CoproductA One
  fwd1 False = CoproductB One

  bwd1 (CoproductA One) = True
  bwd1 (CoproductB One) = False

-- Prove that `a ⊕ a` is isomorphic to `Two ⊗ a`
newtype TwoTimes a = TwoTimes (Two ⊗ a)

instance i1 :: Isomorphism1 (a ⊕ a) (TwoTimes a) where
  fwd1 (CoproductA x) = TwoTimes $ TwoA ⊗ x
  fwd1 (CoproductB y) = TwoTimes $ TwoB ⊗ y

  bwd1 (TwoTimes (TwoA ⊗ x)) = CoproductA x
  bwd1 (TwoTimes (TwoB ⊗ y)) = CoproductB y

-- Prove that `Maybe` is isomorphic to `Const One ⊞ Identity`.
data Maybe a = Nothing | Just a

instance i2 :: Isomorphism2 Maybe (Const One ⊞ Identity) where
  fwd2 Nothing  = CoproductFA (Const One)
  fwd2 (Just x) = CoproductFB (Identity x)

  bwd2 (CoproductFA (Const One)) = Nothing
  bwd2 (CoproductFB (Identity x)) = Just x

-- Prove that `NonEmpty f` is isomorphic to `Identity ⊠ f`
data NonEmpty f a = NonEmpty a (f a)

infixr 5 NonEmpty as :|

instance functorNonEmpty :: Functor f ⇒ Functor (NonEmpty f) where
  map f (x :| xs) = f x :| map f xs

instance i3 :: Functor f ⇒ Isomorphism2 (NonEmpty f) (Identity ⊠ f) where
  fwd2 (x :| xs) = Identity x ⊠ xs
  bwd2 (Identity x ⊠ xs) = x :| xs

-- Prove that `Ann a f` is isomorphic to `Const a ⊠ f`
data Ann a f b = Ann a (f b)

instance functorAnn :: Functor f ⇒ Functor (Ann a f) where
  map f (Ann a fb) = Ann a (map f fb)

instance i4 :: Functor f ⇒ Isomorphism2 (Ann a f) (Const a ⊠ f) where
  fwd2 (Ann a fb) = Const a ⊠ fb
  bwd2 (Const a ⊠ fb) = (Ann a fb)

-- Dummy main function
main :: ∀ a. a → a
main = id
