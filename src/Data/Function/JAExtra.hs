{-# LANGUAGE UnicodeSyntax #-}
module Data.Function.JAExtra
  ( stuff2, stuff3, stuff4, stuff5
  ) where


stuff2 ∷ (α → α → β) → α → β
stuff2 f a = f a a


stuff3 ∷ (α → α → α → β) → α → β
stuff3 f a = f a a a


stuff4 ∷ (α → α → α → α → β) → α → β
stuff4 f a = f a a a a


stuff5 ∷ (α → α → α → α → α → β) → α → β
stuff5 f a = f a a a a a
