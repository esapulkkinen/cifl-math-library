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
>-- | symbolic expressions
>   module Math.Number.NumericExpression
> ) where
>import Math.Number.Real
>import Math.Number.Stream
>import Math.Number.BiStream
>import Math.Number.DimensionalAnalysis
>import Math.Number.NumericExpression
