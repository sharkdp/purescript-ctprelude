# purescript-ctprelude

An educational Prelude for PureScript with names from category theory.

The idea of this project is to use mathematical names from category theory for
the types and functions that are typically defined in a `Prelude`, or some of
the basic libraries. For example, `Tuple` is called `Product` (with infix alias
⊗) and `Either` is called `Coproduct` (with infix alias ⊕). `Unit`, a type
with a single inhabitant, is called `One` whereas `Bool` is called `Two`.

This module is mainly intended for educational purposes. As an example, the
file `test/Main.purs` shows how to prove that `a ⊕ a` is isomorphic to `Two
⊗ a`.
Also, there is a proof that `Maybe` is isomorphic to `Const One ⊞ Identity`,
where ⊞ is the coproduct of two functors and `One` is a type with a single
inhabitant (`Unit`).
