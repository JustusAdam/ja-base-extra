{-# LANGUAGE UnicodeSyntax #-}
module Data.Tuple.JAExtra
  ( uncurry2, uncurry3, uncurry4, uncurry5, uncurry6
  , uncurry7, uncurry8, uncurry9, uncurry10
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
