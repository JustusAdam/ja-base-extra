{-# LANGUAGE UnicodeSyntax #-}
module Data.List.JAExtra
  ( to1Value, to2Tuple, to3Tuple, to4Tuple, to5Tuple
  , to6Tuple, to7Tuple, to8Tuple, to9Tuple, to10Tuple

  , get
  , slice
  ) where


to1Value ∷ [α] -> Maybe α
to1Value [a] = return a
to1Value _ = Nothing
{-# INLINE to1Value #-}


to2Tuple ∷ [α] → Maybe (α, α)
to2Tuple [a1, a2] = return (a1, a2)
to2Tuple _ = Nothing
{-# INLINE to2Tuple #-}


to3Tuple ∷ [α] → Maybe (α, α, α)
to3Tuple [a1, a2, a3] = return (a1, a2, a3)
to3Tuple _ = Nothing
{-# INLINE to3Tuple #-}


to4Tuple ∷ [α] → Maybe (α, α, α, α)
to4Tuple [a1, a2, a3, a4] = return (a1, a2, a3, a4)
to4Tuple _ = Nothing
{-# INLINE to4Tuple #-}


to5Tuple ∷ [α] → Maybe (α, α, α, α, α)
to5Tuple [a1, a2, a3, a4, a5] = return (a1, a2, a3, a4, a5)
to5Tuple _ = Nothing
{-# INLINE to5Tuple #-}


to6Tuple ∷ [α] → Maybe (α, α, α, α, α, α)
to6Tuple [a1, a2, a3, a4, a5, a6] = return (a1, a2, a3, a4, a5, a6)
to6Tuple _ = Nothing
{-# INLINE to6Tuple #-}


to7Tuple ∷ [α] → Maybe (α, α, α, α, α, α, α)
to7Tuple [a1, a2, a3, a4, a5, a6, a7] = return (a1, a2, a3, a4, a5, a6, a7)
to7Tuple _ = Nothing
{-# INLINE to7Tuple #-}


to8Tuple ∷ [α] → Maybe (α, α, α, α, α, α, α, α)
to8Tuple [a1, a2, a3, a4, a5, a6, a7, a8] = return (a1, a2, a3, a4, a5, a6, a7, a8)
to8Tuple _ = Nothing
{-# INLINE to8Tuple #-}


to9Tuple ∷ [α] → Maybe (α, α, α, α, α, α, α, α, α)
to9Tuple [a1, a2, a3, a4, a5, a6, a7, a8, a9] = return (a1, a2, a3, a4, a5, a6, a7, a8, a9)
to9Tuple _ = Nothing
{-# INLINE to9Tuple #-}


to10Tuple ∷ [α] → Maybe (α, α, α, α, α, α, α, α, α, α)
to10Tuple [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10] = return (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
to10Tuple _ = Nothing
{-# INLINE to10Tuple #-}


get ∷ Int → [α] → Maybe α
get i =
  if i >= 0
    then get' i
    else get' (-i) . reverse
  where
    get' 0 (x:_)    = return x
    get' _ []       = Nothing
    get' i l@(_:xs) = (i-1) `get'` xs


slice ∷ Int → Int → [α] → [α]
slice i j l = take (j' - i') . drop i' $ l
  where
    turn i = if i < 0 then length l + i else i
    j'= turn j
    i'= turn i
