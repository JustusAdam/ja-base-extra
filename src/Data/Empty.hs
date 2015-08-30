{-# LANGUAGE UnicodeSyntax #-}
module Data.Empty
  (
  ) where


class Empty η where
  ε ∷ η
  isEmpty ∷ η → Bool


instance Empty [α] where
  ε = []
  isEmpty = null


instance Empty (Maybe α) where
  ε = Nothing

  isEmpty Nothing = True
  isEmpty _ = False


instance Empty () where
  ε = ()
  isEmpty = const True
