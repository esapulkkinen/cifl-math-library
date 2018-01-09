>module GHCI_Prelude (
>   module Math.Tools,
>   module Math.Matrix,
>  ) where
>import Math.Tools hiding (sequence,interleave,square_root)
>import Math.Matrix

import Model hiding (imaginary_unit, unyoneda)
