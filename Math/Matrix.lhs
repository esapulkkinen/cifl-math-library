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
>  ) where
>import safe Math.Matrix.Interface 
>import safe Math.Matrix.Matrix
>import safe Math.Matrix.Covector 
>import safe Math.Matrix.Vector1
>import safe Math.Matrix.Vector2
>import safe Math.Matrix.Vector3
>import safe Math.Matrix.Vector4
>import safe Math.Matrix.Simple
>import safe Math.Matrix.Instances
>import safe Math.Matrix.Transpose
>import safe Math.Matrix.Linear
>import safe Math.Matrix.QuasiQuoter
>import safe Math.Matrix.Unicode
>import safe Math.Matrix.SIMD
