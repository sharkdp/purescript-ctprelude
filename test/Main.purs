module Test.Main where

import CTPrelude

-- | Prove that `Bool` is isomorphic to `One ⊕ One`.
data Bool = True | False

instance i0 :: Isomorphism0 Bool (One ⊕ One) where
  fwd0 True  = CoproductA One
  fwd0 False = CoproductB One

  bwd0 (CoproductA One) = True
  bwd0 (CoproductB One) = False

-- | Prove that `Maybe` is isomorphic to `Const One ⊕ Identity`.
data Maybe a = Nothing | Just a

instance i1 :: Isomorphism1 Maybe (Const One ⊞ Identity) where
  fwd1 Nothing = CoproductF $ CoproductA (Const One)
  fwd1 (Just x) = CoproductF $ CoproductB (Identity x)

  bwd1 (CoproductF (CoproductA (Const One))) = Nothing
  bwd1 (CoproductF (CoproductB (Identity x))) = Just x

-- Dummy main function
main :: ∀ a. a → a
main = id
