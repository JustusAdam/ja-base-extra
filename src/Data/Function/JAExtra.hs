{-|
Module      : $HEADER$
Description : Extra functions for working with functions.
Copyright   : (c) Justus Adam, 2015
License     : BDS3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX, Windows
== Naming conventions for stuffing- and constant functions
If we follow trough with the naming conventions of stuffing- and constant functions
the following rules emerge:

@
  stuffWith1 == const0 == id
  stuffWith0 == const1
@
-}
{-# LANGUAGE UnicodeSyntax #-}
module Data.Function.JAExtra
  (
  -- * Stuffing functions

  -- | Functions from the stuffing family are used when a function takes several
  -- consecutive arguments of the same type.
  --
  -- Sometimes it is desirable to call some of these arguments with the same value
  -- and those functions here allow you to do that in the
  -- \"do-not-repeat-yourself\" way.
  --
  -- The canonical way to call this function (if all arguments are present) would
  -- be in the infix form:
  --
  -- @
  --  function \`stuffWith2\` value
  -- @
  --
  -- The naming convention is stuffWith__N__ takes a function accepting at least
  -- __N__ consecutive arguments of the same type and a value of that type calling
  -- the function with the provided value in all __N__ places.
  --
  -- @
  --  stuff3 f a == f a a a
  -- @
  --
  -- Multiple argument stuffing can also be achieved by chaining 'stuffWith'.
  --
  -- @
  --  stuffWith4 f == (stuffWith . stuffWith . stuffWith) f
  -- @

    stuffWith, stuffWith2, stuffWith3, stuffWith4, stuffWith5

  -- * Constant functions

  -- | Functions from the const family behave very much like 'const' function
  -- from prelude. But for more arguments.
  --
  -- The const__N__ function takes a value and __N__ arguments of
  -- arbitrary type returning the value unchanged after all arguments have been
  -- supplied.
  --
  -- The same effect can be reached by chaining 'const' but does not look as good.
  --
  -- @
  --  const3 v == (const . const . const) v
  -- @

  , const1, const2, const3, const4, const5

  ) where


-- | Alias for 'stuffWith2'
stuffWith ∷ (α → α → β) → α → β
stuffWith f a = f a a


stuffWith2 ∷ (α → α → β) → α → β
stuffWith2 = stuffWith


stuffWith3 ∷ (α → α → α → β) → α → β
stuffWith3 = stuffWith2 . stuffWith2


stuffWith4 ∷ (α → α → α → α → β) → α → β
stuffWith4 = stuffWith3 . stuffWith2


stuffWith5 ∷ (α → α → α → α → α → β) → α → β
stuffWith5 = stuffWith4 . stuffWith2


-- | Alias for the 'const' function from prelude in the const function family
-- naming scheme.
const1 ∷ α → β → α
const1 = const


const2 ∷ γ → α → β → γ
const2 = const1 . const1


const3 ∷ δ → α → β → γ → δ
const3 = const2 . const1


const4 ∷ ε → α → β → γ → δ → ε
const4 = const3 . const1


const5 ∷ ζ → α → β → γ → δ → ε → ζ
const5 = const4 . const1
