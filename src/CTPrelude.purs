-- | An educational Prelude for PureScript with names from category theory.
module CTPrelude where

-------------------------------------------------------------------------------
-- Types (sets) with a finite number of 0, 1, .. 5 inhabitants (elements).
-------------------------------------------------------------------------------

-- | A type with no inhabitant (the empty set). Typically known as `Void`.
data Zero

-- | A type with a single inhabitant (a singleton set). Typically known as
-- | `Unit`.
data One = One

-- | A type with two inhabitants (a set with two elements). Typically known as
-- | `Bool`. Isomorphic to `One ⊕ One`.
data Two = TwoA | TwoB

-- | A type with three inhabitants. Isomorphic to `One ⊕ Two`.
data Three = ThreeA | ThreeB | ThreeC

-- | A type with four inhabitants. Isomorphic to `Three ⊕ One`,
-- | `Two ⊗ Two`, etc.
data Four = FourA | FourB | FourC | FourD

-- | A type with five inhabitants.
data Five = FiveA | FiveB | FiveC | FiveD | FiveE

-------------------------------------------------------------------------------
-- Some basic functions.
-------------------------------------------------------------------------------

-- | The identity morphism (function).
id :: ∀ a. a → a
id x = x

-- | Morphism (function) composition.
compose :: ∀ a b c. (b → c) → (a → b) → (a → c)
compose f g x = f (g x)

infixr 10 compose as ∘

-- | Helper-function to defined `$`.
apply :: ∀ a b. (a → b) → a → b
apply f x = f x

infixr 0 apply as $

-------------------------------------------------------------------------------
-- Initial and final object
-------------------------------------------------------------------------------

-- | In the category of PureScript types, `Zero` is the initial object.
type Initial = Zero

-- | In the category of PureScript types, `One` is the final object.
type Final = One

-- | `fromInitial` (also known as `absurd`) is the unique morphism from the
-- | inital object to anything else. Ex falso quodlibet.
foreign import
  fromInitial :: ∀ a. Initial → a

-- | `toFinal` (also known as `unit`) is the unique morphism from any object to
-- | the final object.
toFinal :: ∀ a. a → Final
toFinal _ = One

-------------------------------------------------------------------------------
-- Definition of the product and the coproduct.
-------------------------------------------------------------------------------

-- | The product of two types, also known as `Tuple`.
data Product a b = Product a b

infixl 6 type Product as ⊗
infixl 6 Product as ⊗

-- | The coproduct (sum) of two types, also known as `Either`.
data Coproduct a b = CoproductA a | CoproductB b

infixl 5 type Coproduct as ⊕

-------------------------------------------------------------------------------
-- The functor type class.
-------------------------------------------------------------------------------

-- | The Functor typeclass.
-- |
-- | Laws:
-- | - Identity: `map id = id`
-- | - Composition: `map (f ∘ g) = map f ∘ map g`
class Functor f where
  map :: ∀ a b. (a → b) → (f a → f b)

infixl 4 map as <$>

-------------------------------------------------------------------------------
-- Natural transformations.
-------------------------------------------------------------------------------

-- | A natural transformation between two functors.
type NaturalTransformation f g = ∀ a. f a → g a

infixr 6 type NaturalTransformation as ↝

-------------------------------------------------------------------------------
-- Type classes for isomorphisms.
-------------------------------------------------------------------------------

-- | A type class for isomorphisms between two types of kind `*`.
-- |
-- | Laws:
-- |    * `fwd0 ∘ bwd0 = id`
-- |    * `bwd0 ∘ fwd0 = id`
class Isomorphism0 a b where
  fwd0 :: a → b
  bwd0 :: b → a

-- | A type class for isomorphisms between two types of kind `* → *`.
-- |
-- | Laws:
-- |    * `fwd1 ∘ bwd1 = id`
-- |    * `bwd1 ∘ fwd1 = id`
class Isomorphism1 f g where
  fwd1 :: f ↝ g
  bwd1 :: g ↝ f

-------------------------------------------------------------------------------
-- Products and coproducts of functors.
-------------------------------------------------------------------------------

-- | The product of two functors.
newtype ProductF f g a = ProductF (f a ⊗ g a)

infixl 4 type ProductF as ⊠

instance functorProductF :: (Functor f, Functor g) ⇒ Functor (ProductF f g) where
  map f (ProductF (fa ⊗ ga)) = ProductF $ (f <$> fa) ⊗ (f <$> ga)

-- | The coproduct of two functors.
newtype CoproductF f g a = CoproductF (f a ⊕ g a)

infixl 3 type CoproductF as ⊞

instance functorFCoproduct :: (Functor f, Functor g) ⇒ Functor (CoproductF f g) where
  map f (CoproductF (CoproductA fa)) = CoproductF $ CoproductA (f <$> fa)
  map f (CoproductF (CoproductB fb)) = CoproductF $ CoproductB (f <$> fb)

-------------------------------------------------------------------------------
-- Identity and Const functor.
-------------------------------------------------------------------------------

-- | The Identity functor.
data Identity a = Identity a

instance functorIdentity :: Functor Identity where
  map f (Identity x) = Identity (f x)

-- | The Constant functor.
data Const a b = Const a

instance functorConst :: Functor (Const a) where
  map _ (Const x) = Const x

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

infixl 5 Cons as :

instance functorList :: Functor List where
  map _ Nil = Nil
  map f (x : xs) = f x : map f xs

-- | The length of a list as a natural transformation from `List` to `Const
-- | Nat`.
length :: List ↝ Const Nat
length Nil = Const Zero
length (_ : xs) =
  case length xs of
    Const n → Const (n + one)
