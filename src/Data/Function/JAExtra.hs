{-|
Module      : $HEADER$
Description : Extra functions for working with functions.
Copyright   : (c) Justus Adam, 2015
License     : BDS3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX, Windows
-}
{-# LANGUAGE UnicodeSyntax #-}
module Data.Function.JAExtra
  (
  -- * Stuffing functions

  -- | Functions from the stuffing family are used when a function takes several
  -- consecutive parameters of the same type.
  --
  -- Sometimes it is desirable to call some of these arguments with the same value
  -- and those functions here allow you to do that in the
  -- \"do-not-repeat-yourself\" way.

    stuff2, stuff3, stuff4, stuff5

  -- * Constant functions

  -- | Functions from the const family behave very much like 'const' function
  -- from prelude. But for more arguments.
  --
  -- The const__N__ function takes a value and __N__ arguments of
  -- arbitrary type returning the value after all arguments have been supplied.
  --
  -- The same effect can be reached by chaining 'const' but does not look as good.
  --
  -- @
  --  const3 v == (const . const . const) v
  -- @

  , const1, const2, const3, const4, const5

  ) where


stuff2 ∷ (α → α → β) → α → β
stuff2 f a = f a a


stuff3 ∷ (α → α → α → β) → α → β
stuff3 f a = f a a a


stuff4 ∷ (α → α → α → α → β) → α → β
stuff4 f a = f a a a a


stuff5 ∷ (α → α → α → α → α → β) → α → β
stuff5 f a = f a a a a a


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
