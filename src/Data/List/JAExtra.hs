{-# LANGUAGE UnicodeSyntax #-}
module Data.List.JAExtra
  ( to1Value, to2Tuple, to3Tuple, to4Tuple, to5Tuple
  , to6Tuple, to7Tuple, to8Tuple, to9Tuple, to10Tuple

  , get
  , slice

  , monoidFillZip, monoidFillZip2, monoidFillZip3, monoidFillZip4, monoidFillZip5

  , fillZip, fillZip2, fillZip3, fillZip4, fillZip5
  ) where


import Data.List
import Data.Tuple.JAExtra
import Control.Monad


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
get i
  | i >= 0 = get' i
  | otherwise = get' (-i) . reverse
  where
    get' 0 (x:_)    = return x
    get' _ []       = Nothing
    get' i l@(_:xs) = (i-1) `get'` xs


slice ∷ Int → Int → [α] → [α]
slice i j l = take (j' - i') . drop i' $ l
  where
    turn i
      | i < 0     = length l + i
      | otherwise = i
    j'= turn j
    i'= turn i


monoidFillZip2 ∷ (Monoid α, Monoid β) ⇒ [α] → [β] → [(α, β)]
monoidFillZip2 [] ys = zip (repeat mempty) ys
monoidFillZip2 xs [] = zip xs (repeat mempty)
monoidFillZip2 (x:xs) (y:ys) = (x, y) : monoidFillZip2 xs ys


monoidFillZip3 ∷ (Monoid α, Monoid β, Monoid γ) ⇒ [α] → [β] → [γ] → [(α, β, γ)]
monoidFillZip3 [] ys zs = uncurry (zip3 (repeat mempty)) $ unzip $ monoidFillZip2 ys zs
monoidFillZip3 xs [] zs = zip3 xs' (repeat mempty) zs'
  where (xs', zs') = unzip $ monoidFillZip2 xs zs
monoidFillZip3 xs ys [] = uncurry zip3 (unzip $ monoidFillZip2 xs ys) (repeat mempty)
monoidFillZip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : monoidFillZip3 xs ys zs


monoidFillZip4 ∷ (Monoid α, Monoid β, Monoid γ, Monoid δ) ⇒ [α] → [β] → [γ] → [δ] → [(α, β, γ, δ)]
monoidFillZip4 [] bs cs ds = uncurry3 (zip4 (repeat mempty)) $ unzip3 $ monoidFillZip3 bs cs ds
monoidFillZip4 as [] cs ds = zip4 as' (repeat mempty) cs' ds'
  where (as', cs', ds') = unzip3 $ monoidFillZip3 as cs ds
monoidFillZip4 as bs [] ds = zip4 as' bs' (repeat mempty) ds'
  where (as', bs', ds') = unzip3 $ monoidFillZip3 as bs ds
monoidFillZip4 as bs cs [] = uncurry3 zip4 (unzip3 $ monoidFillZip3 as bs cs) (repeat mempty)
monoidFillZip4 (a:as) (b:bs) (c:cs) (d:ds) = (a, b, c, d) : monoidFillZip4 as bs cs ds


monoidFillZip5 ∷ (Monoid α, Monoid β, Monoid γ, Monoid δ, Monoid ζ) ⇒ [α] → [β] → [γ] → [δ] → [ζ] → [(α, β, γ, δ, ζ)]
monoidFillZip5 [] bs cs ds es = uncurry4 (zip5 (repeat mempty)) $ unzip4 $ monoidFillZip4 bs cs ds es
monoidFillZip5 as [] cs ds es = zip5 as' (repeat mempty) cs' ds' es'
  where (as', cs', ds', es') = unzip4 $ monoidFillZip4 as cs ds es
monoidFillZip5 as bs [] ds es = zip5 as' bs' (repeat mempty) ds' es'
  where (as', bs', ds', es') = unzip4 $ monoidFillZip4 as bs ds es
monoidFillZip5 as bs cs [] es = zip5 as' bs' cs' (repeat mempty) es'
  where (as', bs', cs', es') = unzip4 $ monoidFillZip4 as bs cs es
monoidFillZip5 as bs cs ds [] = uncurry4 zip5 (unzip4 $ monoidFillZip4 as bs cs ds) (repeat mempty)
monoidFillZip5 (a:as) (b:bs) (c:cs) (d:ds) (e:es) = (a, b, c, d, e) : monoidFillZip5 as bs cs ds es


monoidFillZip ∷ (Monoid α, Monoid β) ⇒ [α] → [β] → [(α, β)]
monoidFillZip = monoidFillZip2


fillZip2 ∷ [α] → [β] → [(Maybe α, Maybe β)]
fillZip2 [] ys = zip (repeat mzero) (map return ys)
fillZip2 xs [] = zip (map return xs) (repeat mzero)
fillZip2 (x:xs) (y:ys) = (return x, return y) : fillZip2 xs ys


fillZip3 ∷ [α] → [β] → [γ] → [(Maybe α, Maybe β, Maybe γ)]
fillZip3 [] ys zs = uncurry (zip3 (repeat mzero)) $ unzip $ fillZip2 ys zs
fillZip3 xs [] zs = zip3 xs' (repeat mzero) zs'
  where (xs', zs') = unzip $ fillZip2 xs zs
fillZip3 xs ys [] = uncurry zip3 (unzip $ fillZip2 xs ys) (repeat mzero)
fillZip3 (x:xs) (y:ys) (z:zs) = (return x, return y, return z) : fillZip3 xs ys zs


fillZip4 ∷ [α] → [β] → [γ] → [δ] → [(Maybe α, Maybe β, Maybe γ, Maybe δ)]
fillZip4 [] bs cs ds = uncurry3 (zip4 (repeat mzero)) $ unzip3 $ fillZip3 bs cs ds
fillZip4 as [] cs ds = zip4 as' (repeat mzero) cs' ds'
  where (as', cs', ds') = unzip3 $ fillZip3 as cs ds
fillZip4 as bs [] ds = zip4 as' bs' (repeat mzero) ds'
  where (as', bs', ds') = unzip3 $ fillZip3 as bs ds
fillZip4 as bs cs [] = uncurry3 zip4 (unzip3 $ fillZip3 as bs cs) (repeat mzero)
fillZip4 (a:as) (b:bs) (c:cs) (d:ds) = (return a, return b, return c, return d) : fillZip4 as bs cs ds


fillZip5 ∷ [α] → [β] → [γ] → [δ] → [ζ] → [(Maybe α, Maybe β, Maybe γ, Maybe δ, Maybe ζ)]
fillZip5 [] bs cs ds es = uncurry4 (zip5 (repeat mzero)) $ unzip4 $ fillZip4 bs cs ds es
fillZip5 as [] cs ds es = zip5 as' (repeat mzero) cs' ds' es'
  where (as', cs', ds', es') = unzip4 $ fillZip4 as cs ds es
fillZip5 as bs [] ds es = zip5 as' bs' (repeat mzero) ds' es'
  where (as', bs', ds', es') = unzip4 $ fillZip4 as bs ds es
fillZip5 as bs cs [] es = zip5 as' bs' cs' (repeat mzero) es'
  where (as', bs', cs', es') = unzip4 $ fillZip4 as bs cs es
fillZip5 as bs cs ds [] = uncurry4 zip5 (unzip4 $ fillZip4 as bs cs ds) (repeat mzero)
fillZip5 (a:as) (b:bs) (c:cs) (d:ds) (e:es) = (return a, return b, return c, return d, return e) : fillZip5 as bs cs ds es


fillZip ∷ [α] → [β] → [(Maybe α, Maybe β)]
fillZip = fillZip2
