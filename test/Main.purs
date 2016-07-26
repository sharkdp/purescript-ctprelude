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
  fwd2 Nothing = CoproductF $ CoproductA (Const One)
  fwd2 (Just x) = CoproductF $ CoproductB (Identity x)

  bwd2 (CoproductF (CoproductA (Const One))) = Nothing
  bwd2 (CoproductF (CoproductB (Identity x))) = Just x

-- Dummy main function
main :: ∀ a. a → a
main = id
