module CTPrelude.Internal
  ( class Eq
  , eq
  , (==)
  , notEq
  , (/=)
  , unsafeTrace
  , unsafePrint
  ) where

-------------------------------------------------------------------------------
-- The Eq typeclass
-------------------------------------------------------------------------------

class Eq a where
  eq ∷ a → a → Boolean

infix 4 eq as ==

instance eqBoolean ∷ Eq Boolean where
  eq true true   = true
  eq false false = true
  eq _ _         = false

notEq ∷ ∀ a. Eq a ⇒ a → a → Boolean
notEq x y = (x == y) == false

infix 4 notEq as /=

-------------------------------------------------------------------------------
-- Debugging functions
-------------------------------------------------------------------------------

-- | A function which can be used for debugging output. Prints the first value
-- | to the console and returns the second argument.
foreign import
  unsafeTrace ∷ ∀ a b. a → b → b

-- | A function which can be used for debugging output. Print a value to the
-- | console and return `true`.
unsafePrint ∷ ∀ a. a → Boolean
unsafePrint x = unsafeTrace x true
