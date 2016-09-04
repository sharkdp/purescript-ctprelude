module Test.Main where

import CTPrelude

-------------------------------------------------------------------------------
-- Prove that `Bool` is isomorphic to `One ⊕ One`.
-------------------------------------------------------------------------------
data Bool = True | False

i0 ∷ Bool ≅ One ⊕ One
i0 = Iso fwd bwd
  where
    fwd True  = CoproductA One
    fwd False = CoproductB One

    bwd (CoproductA One) = True
    bwd (CoproductB One) = False

-------------------------------------------------------------------------------
-- Prove that `a ⊕ a` is isomorphic to `Two ⊗ a`.
-------------------------------------------------------------------------------

i1 ∷ ∀ a. a ⊕ a ≅ Two ⊗ a
i1 = Iso fwd bwd
  where
    fwd (CoproductA x) = TwoA ⊗ x
    fwd (CoproductB y) = TwoB ⊗ y

    bwd (TwoA ⊗ x) = CoproductA x
    bwd (TwoB ⊗ y) = CoproductB y

-------------------------------------------------------------------------------
-- Prove that `Maybe` is isomorphic to `Const One ⊞ Identity`.
-------------------------------------------------------------------------------

data Maybe a = Nothing | Just a

i2 ∷ Maybe ≊ Const One ⊞ Identity
i2 = NaturalIso fwd bwd
  where
    fwd ∷ ∀ a. Maybe a → (Const One ⊞ Identity) a
    fwd Nothing  = CoproductFA (Const One)
    fwd (Just x) = CoproductFB (Identity x)

    bwd ∷ ∀ a. (Const One ⊞ Identity) a → Maybe a
    bwd (CoproductFA (Const One)) = Nothing
    bwd (CoproductFB (Identity x)) = Just x

-------------------------------------------------------------------------------
-- Prove that `NonEmpty f` is isomorphic to `Identity ⊠ f`.
-------------------------------------------------------------------------------

data NonEmpty f a = NonEmpty a (f a)

infixr 5 NonEmpty as :|

instance functorNonEmpty ∷ Functor f ⇒ Functor (NonEmpty f) where
  map f (x :| xs) = f x :| map f xs

i3 ∷ ∀ f. Functor f ⇒ NonEmpty f ≊ Identity ⊠ f
i3 = NaturalIso fwd bwd
  where
    fwd ∷ ∀ a. NonEmpty f a → (Identity ⊠ f) a
    fwd (x :| xs) = Identity x ⊠ xs

    bwd ∷ ∀ a. (Identity ⊠ f) a → NonEmpty f a
    bwd (Identity x ⊠ xs) = x :| xs

-------------------------------------------------------------------------------
-- Prove that `Ann a f` is isomorphic to `Const a ⊠ f`.
-------------------------------------------------------------------------------

data Ann a f b = Ann a (f b)

instance functorAnn ∷ Functor f ⇒ Functor (Ann a f) where
  map f (Ann a fb) = Ann a (map f fb)

i4 ∷ ∀ a f. Functor f ⇒ Ann a f ≊ Const a ⊠ f
i4 = NaturalIso fwd bwd
  where
    fwd ∷ ∀ b. Ann a f b → (Const a ⊠ f) b
    fwd (Ann a fb) = Const a ⊠ fb

    bwd ∷ ∀ b. (Const a ⊠ f) b → Ann a f b
    bwd (Const a ⊠ fb) = (Ann a fb)

-------------------------------------------------------------------------------
-- Prove that `Const One ⊞ f` is isomorphic to `Maybe ⊚ f`.
-------------------------------------------------------------------------------

data AddOne f a = AddOne ((Const One ⊞ f) a)

i5 ∷ ∀ f. Functor f ⇒ AddOne f ≊ Maybe ⊚ f
i5 = NaturalIso fwd bwd
  where
    fwd ∷ ∀ a. AddOne f a → (Maybe ⊚ f) a
    fwd (AddOne (CoproductFA (Const One))) = Compose Nothing
    fwd (AddOne (CoproductFB f))           = Compose (Just f)

    bwd ∷ ∀ a. (Maybe ⊚ f) a → AddOne f a
    bwd (Compose Nothing)  = AddOne (CoproductFA (Const One))
    bwd (Compose (Just f)) = AddOne (CoproductFB f)

-------------------------------------------------------------------------------
-- Natural numbers.
-------------------------------------------------------------------------------

-- | Natural numbers.
data Nat = Zero | Succ Nat

-- | Addition of natural numbers.
add ∷ Nat → Nat → Nat
add Zero n = n
add n Zero = n
add (Succ n) m = Succ (n `add` m)

infixl 5 add as +

-- | The natural number `0`.
zero ∷ Nat
zero = Zero

-- | The natural number `1`.
one ∷ Nat
one = Succ Zero

-- | The natural number `2`.
two ∷ Nat
two = one + one

-- | The natural number `3`.
three ∷ Nat
three = two + one

-------------------------------------------------------------------------------
-- Linked lists.
-------------------------------------------------------------------------------

-- | A linked list.
data List a = Nil | Cons a (List a)

infixr 6 Cons as :

instance functorList ∷ Functor List where
  map _ Nil = Nil
  map f (x : xs) = f x : map f xs

-- | The length of a list as a natural transformation from `List` to
-- `Const Nat`.
length ∷ List ↝ Const Nat
length Nil      = Const Zero
length (_ : xs) = Const (runConst (length xs) + one)

-------------------------------------------------------------------------------
-- Prove that `List` is isomorphic to `Const One ⊞ (Identity ⊠ List)`.
-------------------------------------------------------------------------------

i6 ∷ List ≊ Const One ⊞ (Identity ⊠ List)
i6 = NaturalIso fwd bwd
  where
    fwd ∷ ∀ a. List a → (Const One ⊞ (Identity ⊠ List)) a
    fwd Nil         = CoproductFA (Const One)
    fwd (Cons x xs) = CoproductFB (Identity x ⊠ xs)

    bwd ∷ ∀ a. (Const One ⊞ (Identity ⊠ List)) a → List a
    bwd (CoproductFA (Const One))       = Nil
    bwd (CoproductFB (Identity x ⊠ xs)) = x : xs

-------------------------------------------------------------------------------
-- Prove that `These a b` is isomorphic to `a ⊕ b ⊕ (a ⊗ b)`
-------------------------------------------------------------------------------

data These a b = This a | That b | Both a b

i7 ∷ ∀ a b. These a b ≅ a ⊕ b ⊕ (a ⊗ b)
i7 = Iso fwd bwd
  where
    fwd (This x)   = CoproductA x
    fwd (That y)   = CoproductB (CoproductA y)
    fwd (Both x y) = CoproductB (CoproductB (x ⊗ y))

    bwd (CoproductA x)                    = This x
    bwd (CoproductB (CoproductA y))       = That y
    bwd (CoproductB (CoproductB (x ⊗ y))) = Both x y

-------------------------------------------------------------------------------
-- Dummy main function
-------------------------------------------------------------------------------

main ∷ ∀ a. a → a
main = id
