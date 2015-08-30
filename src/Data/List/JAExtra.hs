{-# LANGUAGE UnicodeSyntax #-}
module Data.List.JAExtra
  ( to1Value, to2Tuple, to3Tuple, to4Tuple, to5Tuple
  , to6Tuple, to7Tuple, to8Tuple, to9Tuple, to10Tuple

  , get
  , slice

  , fillZip, fillZip2, fillZip3, fillZip4, fillZip5
  ) where


import Data.List
import Data.Tuple.JAExtra


to1Value ∷ [α] → Maybe α
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


fillZip2 ∷ [α] → [β] → [(Maybe α, Maybe β)]
fillZip2 [] ys = zip (repeat Nothing) (map Just ys)
fillZip2 xs [] = zip (map Just xs) (repeat Nothing)
fillZip2 (x:xs) (y:ys) = (return x, return y) : fillZip2 xs ys


fillZip3 ∷ [α] → [β] → [γ] → [(Maybe α, Maybe β, Maybe γ)]
fillZip3 [] ys zs = uncurry (zip3 (repeat Nothing)) $ unzip $ fillZip2 ys zs
fillZip3 xs [] zs = zip3 xs' (repeat Nothing) zs'
  where (xs', zs') = unzip $ fillZip2 xs zs
fillZip3 xs ys [] = uncurry zip3 (unzip $ fillZip2 xs ys) (repeat Nothing)
fillZip3 (x:xs) (y:ys) (z:zs) = (return x, return y, return z) : fillZip3 xs ys zs


fillZip4 ∷ [α] → [β] → [γ] → [δ] → [(Maybe α, Maybe β, Maybe γ, Maybe δ)]
fillZip4 [] bs cs ds = uncurry3 (zip4 (repeat Nothing)) $ unzip3 $ fillZip3 bs cs ds
fillZip4 as [] cs ds = zip4 as' (repeat Nothing) cs' ds'
  where (as', cs', ds') = unzip3 $ fillZip3 as cs ds
fillZip4 as bs [] ds = zip4 as' bs' (repeat Nothing) ds'
  where (as', bs', ds') = unzip3 $ fillZip3 as bs ds
fillZip4 as bs cs [] = uncurry3 zip4 (unzip3 $ fillZip3 as bs cs) (repeat Nothing)


fillZip5 ∷ [α] → [β] → [γ] → [δ] → [ε] → [(Maybe α, Maybe β, Maybe γ, Maybe δ, Maybe ε)]
fillZip5 [] bs cs ds es = uncurry4 (zip5 (repeat Nothing)) $ unzip4 $ fillZip4 bs cs ds es
fillZip5 as [] cs ds es = zip5 as' (repeat Nothing) cs' ds' es'
  where (as', cs', ds', es') = unzip4 $ fillZip4 as cs ds es
fillZip5 as bs [] ds es = zip5 as' bs' (repeat Nothing) ds' es'
  where (as', bs', ds', es') = unzip4 $ fillZip4 as bs ds es
fillZip5 as bs cs [] es = zip5 as' bs' cs' (repeat Nothing) es'
  where (as', bs', cs', es') = unzip4 $ fillZip4 as bs cs es
fillZip5 as bs cs ds [] = uncurry4 zip5 (unzip4 $ fillZip4 as bs cs ds) (repeat Nothing)


fillZip ∷ [α] → [β] → [(Maybe α, Maybe β)]
fillZip = fillZip2
