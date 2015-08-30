{-# LANGUAGE UnicodeSyntax #-}
module Data.Maybe.JAExtra
  ( justIf
  ) where


justIf ∷ α → Bool → Maybe α
justIf a True = Just a
justIf _ False = Nothing
