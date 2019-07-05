>{-# LANGUAGE Safe, GADTs, QuasiQuotes #-}
>{-# OPTIONS_HADDOCK not-home, ignore-exports #-}
>-- |
>-- Module: Math.Matrix
>-- Description: Matrix computations
>-- Copyright: (c) Esa Pulkkinen, 2000..2018
>-- License: LGPL
>-- Maintainer: esa.pulkkinen@iki.fi
>-- Stability: experimental
>module Math.Matrix (
>-- * Matrix interface type classes
>   module Math.Matrix.Interface,
>-- * Matrix syntactic sugar
>   module Math.Matrix.QuasiQuoter,
>-- ** Matrix data structure
>   module Math.Matrix.Matrix,
>-- * one dimensional vector
>   module Math.Matrix.Vector1,
>-- * two dimensional vector
>   module Math.Matrix.Vector2,
>-- * three dimensional vector
>   module Math.Matrix.Vector3,
>-- * four dimensional vector
>   module Math.Matrix.Vector4,
>-- * arbitrary dimensional vector
>   module Math.Matrix.Simple,
>-- ** vectors optimized for SIMD
>   module Math.Matrix.SIMD,
>-- * Dual vector
>   module Math.Matrix.Covector, 
>-- ** data structure for linear arrows
>   module Math.Matrix.Linear,
>-- ** unicode syntax
>   module Math.Matrix.Unicode,
>-- ** commutator and characteristic polynomials
>   module Math.Matrix.Instances,
>-- ** concurrent evaluation of matrices
>   module Math.Matrix.Concurrent
>  ) where
>import Math.Matrix.Interface 
>import Math.Matrix.Matrix
>import Math.Matrix.Covector 
>import Math.Matrix.Vector1
>import Math.Matrix.Vector2
>import Math.Matrix.Vector3
>import Math.Matrix.Vector4
>import Math.Matrix.Simple
>import Math.Matrix.Instances
>import Math.Matrix.Transpose
>import Math.Matrix.Linear
>import Math.Matrix.QuasiQuoter
>import Math.Matrix.Unicode
>import Math.Matrix.SIMD
>import Math.Matrix.Concurrent
