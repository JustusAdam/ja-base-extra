{-# LANGUAGE UnicodeSyntax #-}
module Data.Tuple.JAExtra
  ( uncurry2, uncurry3, uncurry4, uncurry5, uncurry6
  , uncurry7, uncurry8, uncurry9, uncurry10

  , curry2, curry3, curry4, curry5, curry6
  , curry7, curry8, curry9, curry10
  ) where


uncurry2 ∷ (a → b → c) → (a, b) → c
uncurry2 = uncurry
{-# INLINE uncurry2 #-}


uncurry3 ∷ (α → β → γ → δ) → (α, β, γ) → δ
uncurry3 f (a, b, c) = f a b c
{-# INLINE uncurry3 #-}


uncurry4 ∷ (α → β → γ → δ → ε) → (α, β, γ, δ) → ε
uncurry4 f (a, b, c, d) = f a b c d
{-# INLINE uncurry4 #-}


uncurry5 ∷ (α → β → γ → δ → ε → ζ) → (α, β, γ, δ, ε) → ζ
uncurry5 f (a, b, c, d, e) = f a b c d e
{-# INLINE uncurry5 #-}


uncurry6 ∷ (α → β → γ → δ → ε → ζ → η) → (α, β, γ, δ, ε, ζ) → η
uncurry6 f (a, b, c, d, e, f') = f a b c d e f'
{-# INLINE uncurry6 #-}


uncurry7 ∷ (α → β → γ → δ → ε → ζ → η → θ) → (α, β, γ, δ, ε, ζ, η) → θ
uncurry7 f (a, b, c, d, e, f', g) = f a b c d e f' g
{-# INLINE uncurry7 #-}


uncurry8 ∷ (α → β → γ → δ → ε → ζ → η → θ → ι) → (α, β, γ, δ, ε, ζ, η, θ) → ι
uncurry8 f (a, b, c, d, e, f', g, h) = f a b c d e f' g h
{-# INLINE uncurry8 #-}


uncurry9 ∷ (α → β → γ → δ → ε → ζ → η → θ → ι → κ) → (α, β, γ, δ, ε, ζ, η, θ, ι) → κ
uncurry9 f (a, b, c, d, e, f', g, h, i) = f a b c d e f' g h i
{-# INLINE uncurry9 #-}


uncurry10 ∷ (α → β → γ → δ → ε → ζ → η → θ → ι → κ → λ) → (α, β, γ, δ, ε, ζ, η, θ, ι, κ) → λ
uncurry10 f (a, b, c, d, e, f', g, h, i, j) = f a b c d e f' g h i j
{-# INLINE uncurry10 #-}


curry2 ∷ ((α, β) → γ) → α → β → γ
curry2 = curry
{-# INLINE curry2 #-}


curry3 ∷ ((α, β, γ) → δ) → α → β → γ → δ
curry3 f a b c = f (a, b, c)
{-# INLINE curry3 #-}


curry4 ∷ ((α, β, γ, δ) → ε) → α → β → γ → δ → ε
curry4 f a b c d = f (a, b, c, d)
{-# INLINE curry4 #-}


curry5 ∷ ((α, β, γ, δ, ε) → ζ) → α → β → γ → δ → ε → ζ
curry5 f a b c d e = f (a, b, c, d, e)
{-# INLINE curry5 #-}


curry6 ∷ ((α, β, γ, δ, ε, ζ) → η) → α → β → γ → δ → ε → ζ → η
curry6 f a b c d e f' = f (a, b, c, d, e, f')
{-# INLINE curry6 #-}


curry7 ∷ ((α, β, γ, δ, ε, ζ, η) → θ) → α → β → γ → δ → ε → ζ → η → θ
curry7 f a b c d e f' g = f (a, b, c, d, e, f', g)
{-# INLINE curry7 #-}


curry8 ∷ ((α, β, γ, δ, ε, ζ, η, θ) → ι) → α → β → γ → δ → ε → ζ → η → θ → ι
curry8 f a b c d e f' g h = f (a, b, c, d, e, f', g, h)
{-# INLINE curry8 #-}


curry9 ∷ ((α, β, γ, δ, ε, ζ, η, θ, ι) → κ) → α → β → γ → δ → ε → ζ → η → θ → ι → κ
curry9 f a b c d e f' g h i = f (a, b, c, d, e, f', g, h, i)
{-# INLINE curry9 #-}

curry10 ∷ ((α, β, γ, δ, ε, ζ, η, θ, ι, κ) → λ) → α → β → γ → δ → ε → ζ → η → θ → ι → κ → λ
curry10 f a b c d e f' g h i j = f (a, b, c, d, e, f', g, h, i, j)
{-# INLINE curry10 #-}
