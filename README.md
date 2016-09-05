# CTPrelude

An educational Prelude for PureScript with names from category theory.

The idea of this project is to use mathematical names from category theory
for the types and functions that are typically defined in a Prelude or one
of the basic libraries. For example, `Tuple` is called `Product` (with
infix alias ⊗) and `Either` is called `Coproduct` (with infix alias ⊕).
`Unit`, a type with a single inhabitant, is called `One` whereas
`Boolean` is called `Two`. The following table shows a few well-known
PureScript types, and their (isomorphic) CTPrelude equivalent:

``` purs
         Void  ≅  Zero
         Unit  ≅  One
      Boolean  ≅  Two
     Ordering  ≅  Three

   Either a b  ≅  a ⊕ b
    Tuple a b  ≅  a ⊗ b
    These a b  ≅  a ⊕ b ⊕ (a ⊗ b)

Coproduct f g  ≅  f ⊞ g
  Product f g  ≅  f ⊠ g

        Maybe  ≅  Const One ⊞ Identity
   NonEmpty f  ≅  Identity ⊠ f
```

This module is mainly intended for educational purposes. For some
applications, see the file `test/Main.purs`. It demonstrates how to use
`CTPrelude` to prove isomorphisms between types, such as
`a ⊕ a ≅ Two ⊗ a` or a few of the above.

## The category of PureScript types

In the following, we will deal with the category of PureScript types,
called **Purs**. Objects in **Purs** are PureScript types like `Int`,
`String`, `Two`, `List Two`, `Int ⊗ String`, or `Int → String`
(function types).

Morphisms in **Purs** are functions. The identity morphism is called `id`.
Composition of morphisms is simple function composition, defined by the
`∘` operator (which is associative).

*Note*: In the following, we ignore any problems arising from bottom
values.

# Source code
``` purescript
module CTPrelude where

```
## Simple objects (types)

### Zero
A type with no inhabitant (the empty set). `Zero` is the *initial element*
in **Purs** (see below). This type is also known as `Void`.
``` purescript
data Zero

```
### One
A type with a single inhabitant (a singleton set). `One` is the *terminal
object* in **Purs** (see below). This type is also known as `Unit`.
``` purescript
data One = One

```
### Two
A type with two inhabitants (a set with two elements). Typically known as
`Boolean`. `Two` is isomorphic to `One ⊕ One`, i.e. `Two ≅ One ⊕ One`.
``` purescript
data Two = TwoA | TwoB

```
### Three
A type with three inhabitants. We have `Three ≅ One ⊕ Two ≅ Two ⊕ One`.
``` purescript
data Three = ThreeA | ThreeB | ThreeC

```
## Identity morphism and composition

The identity morphism (or function).
``` purescript
id ∷ ∀ a. a → a
id x = x

```
Morphism composition is given by function composition. Note that function
composition is associative and that `id` is the neutral element of `∘`.
``` purescript
compose ∷ ∀ a b c. (b → c) → (a → b) → (a → c)
compose f g x = f (g x)

infixr 10 compose as ∘

```
## Initial and terminal object

### Initial
In the category of PureScript types, `Zero` is the *initial object*.
``` purescript
type Initial = Zero

```
`fromInitial` (also known as `absurd`) is the unique morphism from the
*inital object* to any other object (type). In logic (using Curry-Howard
correspondence), this corresponds to "from falsehood, anything" or "ex
falso quodlibet".
``` purescript
foreign import
  fromInitial ∷ ∀ a. Initial → a
-- Note that `fromInitial` can never be called, because the type `Initial` has
-- no inhabitant. The function can not be implemented in PureScript, therefore
-- the foreign import.

```
### Terminal
In the category of PureScript types, `One` is the *terminal object*
(sometimes also called final object).
``` purescript
type Terminal = One

```
`toTerminal` (also known as `unit`) is the unique morphism from any object
(type) to the terminal object.
``` purescript
toTerminal ∷ ∀ a. a → Terminal
toTerminal _ = One

```
## Product and coproduct

The product of two types, typically known as `Tuple`. This corresponds to
the categorical product of two objects.
``` purescript
data Product a b = Product a b

infixr 6 type Product as ⊗
infixr 6 Product as ⊗

```
The coproduct (sum) of two types, typically known as `Either`. This
corresponds to the categorical coproduct of two objects.
``` purescript
data Coproduct a b = CoproductA a | CoproductB b

infixr 5 type Coproduct as ⊕

```
## Covariant functors

A typeclass for covariant endofunctors on **Purs** (i.e. functors from
**Purs** to **Purs**). The term `Functor` is used instead of `Endofunctor`
for convenience.

Laws:
- Identity: `map id = id`
- Composition: `map (f ∘ g) = map f ∘ map g`
``` purescript
class Functor f where
  map ∷ ∀ a b. (a → b) → (f a → f b)

infixl 4 map as <$>

```
## Contravariant functors

A typeclass for contravariant endofunctors on **Purs**.

Laws:
- Identity: `cmap id = id`
- Composition: `cmap (f ∘ g) = cmap g ∘ cmap f`
``` purescript
class Contravariant f where
  cmap ∷ ∀ a b. (a → b) → (f b → f a)

infixl 4 cmap as >$<

```
## Morphisms

A newtype for morphisms on **Purs** (functions).
``` purescript
newtype Morphism a b = Morphism (a → b)

instance functorFunction ∷ Functor ((→) a) where
  map = (∘)

instance functorMorphism ∷ Functor (Morphism a) where
  map g (Morphism f) = Morphism (g ∘ f)

```
A newtype for morphisms with the type arguments reversed
``` purescript
newtype Reversed b a = Reversed (a → b)

instance contravariantReversed ∷ Contravariant (Reversed b) where
  cmap g (Reversed f) = Reversed (f ∘ g)

```
## Natural transformations

A natural transformation is a mapping between two functors.
``` purescript
type NaturalTransformation f g = ∀ a. f a → g a

infixr 4 type NaturalTransformation as ↝

```
## Isomorphisms

An isomorphism between two types `a` and `b` is established by two
morphisms, `forwards ∷ a → b` and `backwards ∷ b → a`, satisfying
the following laws:
- `forwards ∘ backwards = id`
- `backwards ∘ forwards = id`
``` purescript
data Iso a b = Iso (a → b) (b → a)

infix 1 type Iso as ≅

```
### forwards
Get a function `a → b` from the isomorphism `a ≅ b`.
``` purescript
forwards ∷ ∀ a b. a ≅ b → a → b
forwards (Iso fwd _) = fwd

```
### backwards
Get a function `b → a` from the isomorphism `a ≅ b`.
``` purescript
backwards ∷ ∀ a b. a ≅ b → b → a
backwards (Iso _ bwd) = bwd

```
### reverse
Reverse an isomorphism.
``` purescript
reverse ∷ ∀ a b. a ≅ b → b ≅ a
reverse (Iso fwd bwd) = Iso bwd fwd

```
### Natural isomorphisms
A natural isomorphism between two types `f` and `g` of kind `* → *`
is given by two natural transformations, `forwardsN ∷ f ↝ g` and
`backwardsN ∷ g ↝ f`, satisfying the following laws:
- `forwardsN ∘ backwardsN = id`
- `backwardsN ∘ forwardsN = id`
``` purescript
data NaturalIso f g = NaturalIso (f ↝ g) (g ↝ f)

infix 1 type NaturalIso as ≊

```
### forwardsN
Get the natural transformation `f ↝ g` from the isomorphism `f ≊ g`.
``` purescript
forwardsN ∷ ∀ f g. f ≊ g → f ↝ g
forwardsN (NaturalIso fwd _) = fwd

```
### backwardsN
Get the natural transformation `g ↝ f` from the isomorphism `f ≊ g`.
``` purescript
backwardsN ∷ ∀ f g. f ≊ g → g ↝ f
backwardsN (NaturalIso _ bwd) = bwd

```
### reverseN
Reverse an isomorphism.
``` purescript
reverseN ∷ ∀ f g. f ≊ g → g ≊ f
reverseN (NaturalIso fwd bwd) = NaturalIso bwd fwd

```
## Products and coproducts of functors

The product of two functors.
``` purescript
data ProductF f g a = ProductF (f a) (g a)

infixl 4 type ProductF as ⊠
infixl 4 ProductF as ⊠

instance functorProductF ∷ (Functor f, Functor g) ⇒ Functor (ProductF f g) where
  map f (fa ⊠ ga) = (f <$> fa) ⊠ (f <$> ga)

```
The coproduct of two functors.
``` purescript
data CoproductF f g a = CoproductFA (f a) | CoproductFB (g a)

infixl 3 type CoproductF as ⊞

instance functorFCoproduct ∷ (Functor f, Functor g) ⇒ Functor (CoproductF f g) where
  map f (CoproductFA fa) = CoproductFA (f <$> fa)
  map f (CoproductFB fb) = CoproductFB (f <$> fb)

```
## Composition of functors

Right-to-left composition of functors. The composition of two functors is
always a functor.
``` purescript
newtype Compose f g a = Compose (f (g a))

infixl 5 type Compose as ⊚

instance functorCompose ∷ (Functor f, Functor g) ⇒ Functor (Compose f g) where
  map h (Compose fga) = Compose (map (map h) fga)

```
## Bifunctors

A `Bifunctor` is a `Functor` from the product category
**Purs** × **Purs** to **Purs**. A type constructor with two arguments is
a `Bifunctor` if it is covariant in each argument.

Laws:
- Identity: `bimap id id = id`
- Composition: `bimap f1 g1 ∘ bimap f2 g2 = bimap (f1 ∘ f2) (g1 ∘ g2)`
``` purescript
class Bifunctor f where
  bimap ∷ ∀ a b c d. (a → c) → (b → d) → f a b → f c d

instance bifunctorProduct ∷ Bifunctor Product where
  bimap f g (x ⊗ y) = f x ⊗ g y

instance bifunctorSum ∷ Bifunctor Coproduct where
  bimap f _ (CoproductA x) = CoproductA (f x)
  bimap _ g (CoproductB y) = CoproductB (g y)

```
## Profunctors.

A `Profunctor` is a `Functor` from the product category
**Purs**<sup>op</sup> × **Purs** to **Purs**, where the superscript *op*
indicates the dual (opposite) category. It can also be thought of as a
`Bifunctor` which is contravariant in its first argument and covariant in
its second argument.

Laws:
- Identity: `dimap id id = id`
- Composition: `dimap f1 g1 ∘ dimap f2 g2 = dimap (f2 ∘ f1) (g1 ∘ g2)`
``` purescript
class Profunctor p where
  dimap ∷ ∀ a b c d. (c → a) → (b → d) → p a b → p c d

instance profunctorFunction ∷ Profunctor (→) where
  dimap f h g = h ∘ g ∘ f

```
### Star
`Star` lifts functors to profunctors (forwards).
``` purescript
newtype Star f a b = Star (a → f b)

runStar ∷ ∀ f a b. Star f a b → (a → f b)
runStar (Star fn) = fn

instance profunctorStar ∷ Functor f ⇒ Profunctor (Star f) where
  dimap f g (Star fn) = Star (map g ∘ fn ∘ f)

```
### Costar
`Costar` lifts functors to profunctors (backwards).
``` purescript
newtype Costar f a b = Costar (f a → b)

runCostar ∷ ∀ f a b. Costar f a b → (f a → b)
runCostar (Costar fn) = fn

instance profunctorCostar ∷ Functor f ⇒ Profunctor (Costar f) where
  dimap f g (Costar fn) = Costar (g ∘ fn ∘ map f)

```
### Strong
The `Strong` class extends `Profunctor` with combinators for working with
products.
``` purescript
class Profunctor p ⇐ Strong p where
  first  ∷ ∀ a b c. p a b → p (a ⊗ c) (b ⊗ c)
  second ∷ ∀ a b c. p b c → p (a ⊗ b) (a ⊗ c)

instance strongFunction ∷ Strong (→) where
  first  fn (a ⊗ c) = fn a ⊗ c
  second fn (a ⊗ c) = a ⊗ fn c

```
### Choice
The `Choice` class extends `Profunctor` with combinators for working with
coproducts.
``` purescript
class Profunctor p ⇐ Choice p where
  left  ∷ ∀ a b c. p a b → p (a ⊕ c) (b ⊕ c)
  right ∷ ∀ a b c. p b c → p (a ⊕ b) (a ⊕ c)

instance choiceFunction ∷ Choice (→) where
  left  fn (CoproductA x) = CoproductA (fn x)
  left  fn (CoproductB x) = CoproductB x
  right fn (CoproductA x) = CoproductA x
  right fn (CoproductB x) = CoproductB (fn x)

```
### Closed
The `Closed` class extends `Profunctor` with a combinator to work with
functions.
``` purescript
class Profunctor p ⇐ Closed p where
  closed ∷ ∀ a b x. p a b → p (x → a) (x → b)

instance closedFunction ∷ Closed Function where
  closed = (∘)

```
## Identity and Const

### Identity
The Identity functor.
``` purescript
data Identity a = Identity a

instance functorIdentity ∷ Functor Identity where
  map f (Identity x) = Identity (f x)

```
### runIdentity
Extract the value from the `Identity` functor.
``` purescript
runIdentity ∷ ∀ a. Identity a → a
runIdentity (Identity x) = x

```
### Const
The Constant functor.
``` purescript
data Const a b = Const a

instance functorConst ∷ Functor (Const a) where
  map _ (Const x) = Const x

instance contravariantConst ∷ Contravariant (Const a) where
  cmap _ (Const x) = Const x

instance bifunctorConst ∷ Bifunctor Const where
  bimap f _ (Const x) = Const (f x)

```
### runConst
Extract the value from a `Const` functor.
``` purescript
runConst ∷ ∀ a b. Const a b → a
runConst (Const x) = x

```
## Monad

A monad is an endofunctor on **Purs**, equipped with two natural
transformations `pure` (often *η*) and `join` (often *µ*).

Laws:
- Right identity: `pure ↣ f = f`
- Left identity:  `f ↣ pure = f`
- Associativity:  `(f ↣ g) ↣ h = f ↣ (g ↣ h)`
``` purescript
class Functor m ⇐ Monad m where
  pure ∷ Identity ↝ m
  join ∷ m ⊚ m ↝ m

```
Compose two functions with monadic return values.
``` purescript
composeKleisli ∷ ∀ m a b c. Monad m ⇒ (a → m b) → (b → m c) → a → m c
composeKleisli f g a = join (Compose (g <$> f a))

infixr 1 composeKleisli as ↣
```
