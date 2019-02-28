>{-# LANGUAGE Safe, GADTs, QuasiQuotes #-}
>{-# OPTIONS_HADDOCK not-home #-}
>module Math.Number (
>-- * interface definitions for numbers
>   module Math.Number.Interface,
>-- * real numbers
>   module Math.Number.Real,
>-- * alternative implementation of reals
>   module Math.Number.R,
>-- * infinite dimensional vector
>   module Math.Number.Stream,
>-- ** infinite dimensional vector to two directions
>   module Math.Number.BiStream,
>-- * dimensional analysis
>   module Math.Number.DimensionalAnalysis,
>-- ** type level rational numbers
>   module Math.Number.TypeRational,
>-- ** type level dimensional analysis
>   module Math.Number.TypeUnits,
>-- * unit-specific newtypes for performance optimization of dimensional analysis
>   module Math.Number.Units,
>-- ** US customary units
>   module Math.Number.USCustomaryUnits,
>-- * complex number tools
>   module Math.Number.Complex,
>-- ** symbolic expressions
>   module Math.Number.NumericExpression
> ) where
>import qualified Math.Number.Real
>import qualified Math.Number.R
>import Math.Number.Stream
>import Math.Number.BiStream
>import Math.Number.DimensionalAnalysis
>import Math.Number.Units
>import Math.Number.USCustomaryUnits
>import Math.Number.NumericExpression
>import Math.Number.Complex
>import Math.Number.TypeRational
>import Math.Number.TypeUnits
>import Math.Number.Interface
