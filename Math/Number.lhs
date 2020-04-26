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
>-- ** two dimensional transformation
>   module Math.Number.Transform2D,
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
>import safe qualified Math.Number.Real
>import safe qualified Math.Number.R
>import safe Math.Number.Stream
>import safe Math.Number.BiStream
>import safe Math.Number.DimensionalAnalysis
>import safe Math.Number.Units
>import safe Math.Number.USCustomaryUnits
>import safe Math.Number.NumericExpression
>import safe Math.Number.Complex
>import safe Math.Number.TypeRational
>import safe Math.Number.TypeUnits
>import safe Math.Number.Interface
>import safe Math.Number.Transform2D
