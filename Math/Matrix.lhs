>{-# LANGUAGE GADTs, QuasiQuotes #-}
>module Math.Matrix (
>-- | Matrix interface type classes
>   module Math.Matrix.Interface,
>-- | Matrix syntactic sugar
>   module Math.Matrix.QuasiQuoter,
>-- | Matrix data structure
>   module Math.Matrix.Matrix,
>-- | one dimensional vector
>   module Math.Matrix.Vector1,
>-- | two dimensional vector
>   module Math.Matrix.Vector2,
>-- | three dimensional vector
>   module Math.Matrix.Vector3,
>-- | four dimensional vector
>   module Math.Matrix.Vector4,
>-- | arbitrary dimensional vector
>   module Math.Matrix.Simple,
>-- | vectors optimized for SIMD
>   module Math.Matrix.SIMD,
>-- | Dual vector
>   module Math.Matrix.Covector, 
>-- | data structure for linear arrows
>   module Math.Matrix.Linear,
>-- | operations on Indexable class
>   module Math.Matrix.Indexable,
>-- | alternative impl of "Math.Matrix.Covector"
>   module Math.Matrix.Bracket,
>-- | unicode syntax
>   module Math.Matrix.Unicode,
>-- | commutator and characteristic polynomials
>   module Math.Matrix.Instances) where
>import Math.Matrix.Interface hiding (unit)
>import Math.Matrix.Matrix
>import Math.Matrix.Covector hiding (kernel)
>import Math.Matrix.Vector1
>import Math.Matrix.Vector2
>import Math.Matrix.Vector3
>import Math.Matrix.Vector4
>import Math.Matrix.Simple
>import Math.Matrix.Instances
>import qualified Math.Matrix.Bracket
>import Math.Matrix.Transpose
>import Math.Matrix.Linear
>import qualified Math.Matrix.Indexable
>import Math.Matrix.QuasiQuoter
>import Math.Matrix.Unicode
>import Math.Matrix.SIMD
