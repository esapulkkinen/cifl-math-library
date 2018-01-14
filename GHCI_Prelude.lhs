>module GHCI_Prelude (
>   module Math.Tools,
>   module Math.Matrix,
>   module Math.Number
>  ) where
>import Math.Tools hiding (sequence,interleave,square_root)
>import Math.Matrix
>import Math.Number
