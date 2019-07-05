>{-# LANGUAGE UnicodeSyntax #-}
>module Math.Number.Unicode where
>import Math.Number.Interface

>(∂) :: (DifferentiallyClosed r) => (r -> r) -> r -> r
>(∂) = derivate

>(∫) :: (DifferentiallyClosed r) => (r,r) -> (r -> r) -> r
>(∫) = integral
