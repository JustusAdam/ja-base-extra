{-# LANGUAGE UnicodeSyntax #-}
module Data.Maybe.JAExtra
  ( onlyIf
  ) where


{-|
  Contruct a value based on a boolean guard.

  >>> "blue" `onlyIf` (len [1] == 1)
  Just "blue"
-}
onlyIf ∷ α → Bool → Maybe α
onlyIf a True = Just a
onlyIf _ False = Nothing
