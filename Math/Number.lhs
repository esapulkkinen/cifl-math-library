>{-# LANGUAGE GADTs, QuasiQuotes #-}
>module Math.Number (
>-- | real numbers
>   module Math.Number.Real,
>-- | infinite dimensional vector
>   module Math.Number.Stream,
>-- | infinite dimensional vector to two directions
>   module Math.Number.BiStream,
>-- | dimensional analysis
>   module Math.Number.DimensionalAnalysis,
>-- | type level rational numbers
>   module Math.Number.TypeRational,
>-- | type level dimensional analysis
>   module Math.Number.TypeUnits,
>-- | unit-specific newtypes for performance optimization of dimensional analysis
>   module Math.Number.Units,
>-- | US customary units
>   module Math.Number.USCustomaryUnits,
>-- | complex number tools
>   module Math.Number.Complex,
>-- | symbolic expressions
>   module Math.Number.NumericExpression
> ) where
>import Math.Number.Real
>import Math.Number.Stream
>import Math.Number.BiStream
>import Math.Number.DimensionalAnalysis
>import Math.Number.Units
>import qualified Math.Number.USCustomaryUnits
>import Math.Number.NumericExpression
>import Math.Number.Complex
>import Math.Number.TypeRational
>import Math.Number.TypeUnits
