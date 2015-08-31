{-# LANGUAGE UnicodeSyntax #-}
module Data.Function.JAExtra
  (
    -- * Stuffing functions
    stuff2, stuff3, stuff4, stuff5
  ) where


{-|
  The following functions from the stuffing family are used when a function
  takes several consecutive parameters of the same type.

  Sometimes it is desirable to call some of these arguments with the same value
  and those functions here allow you to do that in the "do-not-repeat-yourself" way.
-}
stuff2 ∷ (α → α → β) → α → β
stuff2 f a = f a a


stuff3 ∷ (α → α → α → β) → α → β
stuff3 f a = f a a a


stuff4 ∷ (α → α → α → α → β) → α → β
stuff4 f a = f a a a a


stuff5 ∷ (α → α → α → α → α → β) → α → β
stuff5 f a = f a a a a a
