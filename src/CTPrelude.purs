-- | An educational Prelude for PureScript with names from category theory.
-- |
-- | In the following, we will deal with the category of PureScript types,
-- | called 'Purs'.
-- |
-- | Objects in Purs are PureScript types like `Int`, `String`, `Two`, `Nat`,
-- | `List Two`, `Int ⊗ String`, or `Int → String` (function types).
-- |
-- | Morphisms in Purs are functions. The identity morphism is called `id`.
-- | Composition of morphisms is simple function composition, defined by the
-- | `∘` operator (which is associative).
-- |
-- | We will ignore any problems arising from bottom values.
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

-- | Helper-function to define `$`.
apply :: ∀ a b. (a → b) → a → b
apply f x = f x

infixr 0 apply as $

-------------------------------------------------------------------------------
-- Initial and final object.
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

infixr 6 type Product as ⊗
infixr 6 Product as ⊗

-- | The coproduct (sum) of two types, also known as `Either`.
data Coproduct a b = CoproductA a | CoproductB b

infixr 5 type Coproduct as ⊕

-------------------------------------------------------------------------------
-- Covariant functors.
-------------------------------------------------------------------------------

-- | A typeclass for covariant endofunctors on Purs (i.e. functors from
-- | Purs to Purs). The term `Functor` is used instead of `Endofunctor` for
-- | convenience.
-- |
-- | Laws:
-- | - Identity: `map id = id`
-- | - Composition: `map (f ∘ g) = map f ∘ map g`
class Functor f where
  map :: ∀ a b. (a → b) → (f a → f b)

infixl 4 map as <$>

-------------------------------------------------------------------------------
-- Contravariant functors.
-------------------------------------------------------------------------------

-- | A typeclass for contravariant endofunctors on Purs.
-- |
-- | Laws:
-- | - Identity: `cmap id = id`
-- | - Composition: `cmap (f ∘ g) = cmap g ∘ cmap f`
class Contravariant f where
  cmap :: ∀ a b. (a → b) → (f b → f a)

infixl 4 cmap as >$<

-------------------------------------------------------------------------------
-- Morphisms
-------------------------------------------------------------------------------

-- | A newtype for morphisms on Purs (functions).
newtype Morphism a b = Morphism (a → b)

instance functorFunction :: Functor ((→) a) where
  map = (∘)

instance functorMorphism :: Functor (Morphism a) where
  map g (Morphism f) = Morphism (g ∘ f)

-- | A newtype for morphisms with the type arguments reversed
newtype Reversed b a = Reversed (a → b)

instance contravariantReversed :: Contravariant (Reversed b) where
  cmap g (Reversed f) = Reversed (f ∘ g)

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
-- | - `fwd1 ∘ bwd1 = id`
-- | - `bwd1 ∘ fwd1 = id`
class Isomorphism1 a b where
  fwd1 :: a → b
  bwd1 :: b → a

-- | A type class for isomorphisms between two types of kind `* → *`.
-- |
-- | Laws:
-- | - `fwd2 ∘ bwd2 = id`
-- | - `bwd2 ∘ fwd2 = id`
class Isomorphism2 f g where
  fwd2 :: f ↝ g
  bwd2 :: g ↝ f

-------------------------------------------------------------------------------
-- Products and coproducts of functors.
-------------------------------------------------------------------------------

-- | The product of two functors.
data ProductF f g a = ProductF (f a) (g a)

infixl 4 type ProductF as ⊠
infixl 4 ProductF as ⊠

instance functorProductF :: (Functor f, Functor g) ⇒ Functor (ProductF f g) where
  map f (fa ⊠ ga) = (f <$> fa) ⊠ (f <$> ga)

-- | The coproduct of two functors.
data CoproductF f g a = CoproductFA (f a) | CoproductFB (g a)

infixl 3 type CoproductF as ⊞

instance functorFCoproduct :: (Functor f, Functor g) ⇒ Functor (CoproductF f g) where
  map f (CoproductFA fa) = CoproductFA (f <$> fa)
  map f (CoproductFB fb) = CoproductFB (f <$> fb)

-------------------------------------------------------------------------------
-- Identity and Const functor.
-------------------------------------------------------------------------------

-- | The Identity functor.
data Identity a = Identity a

runIdentity :: ∀ a. Identity a → a
runIdentity (Identity x) = x

instance functorIdentity :: Functor Identity where
  map f (Identity x) = Identity (f x)

-- | The Constant functor.
data Const a b = Const a

-- | Extract the value from a Constant functor.
runConst :: ∀ a b. Const a b → a
runConst (Const x) = x

instance functorConst :: Functor (Const a) where
  map _ (Const x) = Const x

instance contravariantConst :: Contravariant (Const a) where
  cmap _ (Const x) = Const x

-------------------------------------------------------------------------------
-- Bifunctors.
-------------------------------------------------------------------------------

-- | A `Bifunctor` is a `Functor` from the product category Purs × Purs to
-- | Purs. A type constructor with two arguments is a `Bifunctor` if it is
-- | functorial in each argument.
-- |
-- | Laws:
-- | - Identity: `bimap id id == id`
-- | - Composition: `bimap f1 g1 ∘ bimap f2 g2 == bimap (f1 ∘ f2) (g1 ∘ g2)`
class Bifunctor f where
  bimap :: ∀ a b c d. (a → c) → (b → d) → f a b → f c d

instance bifunctorProduct :: Bifunctor Product where
  bimap f g (x ⊗ y) = f x ⊗ g y

instance bifunctorSum :: Bifunctor Coproduct where
  bimap f _ (CoproductA x) = CoproductA (f x)
  bimap _ g (CoproductB y) = CoproductB (g y)

instance bifunctorConst :: Bifunctor Const where
  bimap f _ (Const x) = Const (f x)

-------------------------------------------------------------------------------
-- Composition of Functors.
-------------------------------------------------------------------------------

newtype Compose f g a = Compose (f (g a))

infixl 3 type Compose as ⊚

instance functorCompose :: (Functor f, Functor g) ⇒ Functor (Compose f g) where
  map h (Compose fga) = Compose (map (map h) fga)

-------------------------------------------------------------------------------
-- Profunctors.
-------------------------------------------------------------------------------

-- | A `Profunctor` is a `Functor` from the pair category `(Type^op, Type)`
-- | to `Type`.
-- |
-- | In other words, a `Profunctor` is a type constructor which is
-- | contravariant in its first argument and covariant in its second argument.
-- |
-- | Laws:
-- | - Identity: `dimap id id = id`
-- | - Composition: `dimap f1 g1 ∘ dimap f2 g2 = dimap (f2 ∘ f1) (g1 ∘ g2)`
class Profunctor p where
  dimap :: ∀ a b c d. (c → a) → (b → d) → p a b → p c d

instance profunctorFunction :: Profunctor (→) where
  dimap f h g = h ∘ g ∘ f

-- | `Star` lifts functors to profunctors (forwards).
newtype Star f a b = Star (a → f b)

runStar :: ∀ f a b. Star f a b → (a → f b)
runStar (Star fn) = fn

instance profunctorStar :: Functor f ⇒ Profunctor (Star f) where
  dimap f g (Star fn) = Star $ map g ∘ fn ∘ f

-- | `Costar` lifts functors to profunctors (backwards).
newtype Costar f a b = Costar (f a → b)

runCostar :: ∀ f a b. Costar f a b → (f a → b)
runCostar (Costar fn) = fn

instance profunctorCostar :: Functor f ⇒ Profunctor (Costar f) where
  dimap f g (Costar fn) = Costar $ g ∘ fn ∘ map f

-- | The `Strong` class extends `Profunctor` with combinators for working with
-- | products.
class Profunctor p ⇐ Strong p where
  first  :: ∀ a b c. p a b → p (a ⊗ c) (b ⊗ c)
  second :: ∀ a b c. p b c → p (a ⊗ b) (a ⊗ c)

instance strongFunction :: Strong (→) where
  first  fn (a ⊗ c) = fn a ⊗ c
  second fn (a ⊗ c) = a ⊗ fn c

-- | The `Choice` class extends `Profunctor` with combinators for working with
-- | coproducts.
class Profunctor p ⇐ Choice p where
  left  :: ∀ a b c. p a b → p (a ⊕ c) (b ⊕ c)
  right :: ∀ a b c. p b c → p (a ⊕ b) (a ⊕ c)

instance choiceFunction :: Choice (→) where
  left  fn (CoproductA x) = CoproductA (fn x)
  left  fn (CoproductB x) = CoproductB x
  right fn (CoproductA x) = CoproductA x
  right fn (CoproductB x) = CoproductB (fn x)

-- | The `Closed` class extends `Profunctor` with a combinator to work with
-- | functions.
class Profunctor p ⇐ Closed p where
  closed :: ∀ a b x. p a b → p (x → a) (x → b)

instance closedFunction :: Closed Function where
  closed = (∘)
