module Test.Main where

import CTPrelude

-- Prove that `Bool` is isomorphic to `One ⊕ One`.
data Bool = True | False

instance i0 :: Isomorphism1 Bool (One ⊕ One) where
  fwd1 True  = CoproductA One
  fwd1 False = CoproductB One

  bwd1 (CoproductA One) = True
  bwd1 (CoproductB One) = False

-------------------------------------------------------------------------------
-- Prove that `a ⊕ a` is isomorphic to `Two ⊗ a`.
-------------------------------------------------------------------------------

newtype TwoTimes a = TwoTimes (Two ⊗ a)

instance i1 :: Isomorphism1 (a ⊕ a) (TwoTimes a) where
  fwd1 (CoproductA x) = TwoTimes $ TwoA ⊗ x
  fwd1 (CoproductB y) = TwoTimes $ TwoB ⊗ y

  bwd1 (TwoTimes (TwoA ⊗ x)) = CoproductA x
  bwd1 (TwoTimes (TwoB ⊗ y)) = CoproductB y

-------------------------------------------------------------------------------
-- Prove that `Maybe` is isomorphic to `Const One ⊞ Identity`.
-------------------------------------------------------------------------------

data Maybe a = Nothing | Just a

instance i2 :: Isomorphism2 Maybe (Const One ⊞ Identity) where
  fwd2 Nothing  = CoproductFA (Const One)
  fwd2 (Just x) = CoproductFB (Identity x)

  bwd2 (CoproductFA (Const One)) = Nothing
  bwd2 (CoproductFB (Identity x)) = Just x

-------------------------------------------------------------------------------
-- Prove that `NonEmpty f` is isomorphic to `Identity ⊠ f`.
-------------------------------------------------------------------------------

data NonEmpty f a = NonEmpty a (f a)

infixr 5 NonEmpty as :|

instance functorNonEmpty :: Functor f ⇒ Functor (NonEmpty f) where
  map f (x :| xs) = f x :| map f xs

instance i3 :: Functor f ⇒ Isomorphism2 (NonEmpty f) (Identity ⊠ f) where
  fwd2 (x :| xs) = Identity x ⊠ xs
  bwd2 (Identity x ⊠ xs) = x :| xs

-------------------------------------------------------------------------------
-- Prove that `Ann a f` is isomorphic to `Const a ⊠ f`.
-------------------------------------------------------------------------------

data Ann a f b = Ann a (f b)

instance functorAnn :: Functor f ⇒ Functor (Ann a f) where
  map f (Ann a fb) = Ann a (map f fb)

instance i4 :: Functor f ⇒ Isomorphism2 (Ann a f) (Const a ⊠ f) where
  fwd2 (Ann a fb) = Const a ⊠ fb
  bwd2 (Const a ⊠ fb) = (Ann a fb)

-------------------------------------------------------------------------------
-- Prove that `Const One ⊞ f` is isomorphic to `Maybe ⊚ f`.
-------------------------------------------------------------------------------

data AddOne f a = AddOne ((Const One ⊞ f) a)

instance i5 :: Functor f ⇒ Isomorphism2 (AddOne f) (Maybe ⊚ f) where
  fwd2 (AddOne (CoproductFA (Const One))) = Compose Nothing
  fwd2 (AddOne (CoproductFB f))           = Compose (Just f)

  bwd2 (Compose Nothing)  = AddOne $ CoproductFA (Const One)
  bwd2 (Compose (Just f)) = AddOne $ CoproductFB f

-------------------------------------------------------------------------------
-- Natural numbers.
-------------------------------------------------------------------------------

-- | Natural numbers.
data Nat = Zero | Succ Nat

-- | Addition of natural numbers.
add :: Nat → Nat → Nat
add Zero n = n
add n Zero = n
add (Succ n) m = Succ (n `add` m)

infixl 5 add as +

-- | The natural number `0`.
zero :: Nat
zero = Zero

-- | The natural number `1`.
one :: Nat
one = Succ Zero

-- | The natural number `2`.
two :: Nat
two = one + one

-- | The natural number `3`.
three :: Nat
three = two + one

-------------------------------------------------------------------------------
-- Linked lists.
-------------------------------------------------------------------------------

-- | A linked list.
data List a = Nil | Cons a (List a)

infixr 6 Cons as :

instance functorList :: Functor List where
  map _ Nil = Nil
  map f (x : xs) = f x : map f xs

-- | The length of a list as a natural transformation from `List` to
-- `Const Nat`.
length :: List ↝ Const Nat
length Nil      = Const Zero
length (_ : xs) = Const $ runConst (length xs) + one

-------------------------------------------------------------------------------
-- Prove that `List` is isomorphic to `Const One ⊞ (Identity ⊠ List)`.
-------------------------------------------------------------------------------

instance i6 :: Isomorphism2 List (Const One ⊞ (Identity ⊠ List)) where
  fwd2 Nil         = CoproductFA (Const One)
  fwd2 (Cons x xs) = CoproductFB (Identity x ⊠ xs)

  bwd2 (CoproductFA (Const One))       = Nil
  bwd2 (CoproductFB (Identity x ⊠ xs)) = x : xs

-------------------------------------------------------------------------------
-- Prove that `These a b` is isomorphic to `a ⊕ b ⊕ (a ⊗ b)`
-------------------------------------------------------------------------------

data These a b = This a | That b | Both a b

instance i7 :: Isomorphism1 (These a b) (a ⊕ b ⊕ (a ⊗ b)) where
  fwd1 (This x)   = CoproductA x
  fwd1 (That y)   = CoproductB (CoproductA y)
  fwd1 (Both x y) = CoproductB (CoproductB (x ⊗ y))

  bwd1 (CoproductA x)                    = This x
  bwd1 (CoproductB (CoproductA y))       = That y
  bwd1 (CoproductB (CoproductB (x ⊗ y))) = Both x y

-------------------------------------------------------------------------------
-- Dummy main function
-------------------------------------------------------------------------------

main :: ∀ a. a → a
main = id
