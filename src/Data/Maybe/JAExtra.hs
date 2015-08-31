{-# LANGUAGE UnicodeSyntax #-}
module Data.Maybe.JAExtra
  ( justIf
  ) where


{-|
  Contruct a value based on a boolean guard.
-}
justIf ∷ α → Bool → Maybe α
justIf a True = Just a
justIf _ False = Nothing
