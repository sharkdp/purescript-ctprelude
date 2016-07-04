# purescript-ctprelude

(WORK IN PROGRESS)

A Prelude with names from category theory. 

This module provides an alternative Prelude for PureScript.
The idea is to use mathematical names from category theory for the types and functions.
For example, `Tuple` is called `Product` (with infix alias ⊗) and `Either` is called `Coproduct` (with infix alias ⊕).

This module is mainly intended for educational purposes.
As an example, the `test/Main.purs` shows how to prove (on a type-level) that `Maybe` is isomorphic to `Const One ⊞ Identity`, where ⊞ is the coproduct of two functors and `One` is a type with a single inhabitant (`Unit`).
