# purescript-ctprelude

An educational Prelude for PureScript with names from category theory.

Go ahead, and **[read the source](src/CTPrelude.purs)**.

The idea of this project is to use mathematical names from category theory for
the types and functions that are typically defined in a Prelude or one of
the basic libraries. For example, `Tuple` is called `Product` (with infix alias
⊗) and `Either` is called `Coproduct` (with infix alias ⊕). `Unit`, a type
with a single inhabitant, is called `One` whereas `Bool` is called `Two`.

This module is mainly intended for educational purposes. As an example, the
file `test/Main.purs` shows how to prove that `a ⊕ a` is isomorphic to
`Two ⊗ a`.

Also, the test file demonstrates a few of the following isomorphisms:
``` purs
Either a b  ≅  a ⊕ b
Tuple a b   ≅  a ⊗ b
These a b   ≅  a ⊕ b ⊕ (a ⊗ b)

Coproduct f g  ≅  f ⊞ g
Product f g    ≅  f ⊠ g

NonEmpty f  ≅  Identity ⊠ f
Maybe       ≅  Const One ⊞ Identity
List        ≅  Const One ⊞ (Identity ⊠ List)
```
