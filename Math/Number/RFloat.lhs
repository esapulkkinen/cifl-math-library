>-- | real numbers with underlying floating point representation.
>module Math.Number.RFloat where
>import qualified Math.Number.R as Real
>import Data.Monoid
>import Data.Ratio
> 
>data R = RLimit { approximate_endo :: Endo Rational }
>       | RDouble { double_approximation :: !Double,
>                   accurate_representation :: R }

>liftReal :: (Double -> Double) -> (Real.R -> Real.R) -> R -> R
>liftReal fd f (RLimit x) = RLimit $ Real.approximate_endo $ f (Real.Limit x)
>liftReal fd f (RDouble d r) = let res = fd d in RDouble res (liftReal fd f r)

>liftReal2 :: (Double -> Double -> Double)
>          -> (Real.R -> Real.R -> Real.R)
>          -> R -> R -> R
>liftReal2 fd f (RLimit x) (RLimit y) =
>     RLimit $ Real.approximate_endo $ f (Real.Limit x) (Real.Limit y)
>liftReal2 fd f (RDouble d r) (RDouble d' r') = RDouble (fd d d') (liftReal2 fd f r r')
>liftReal2 fd f x'@(RLimit _) z@(RDouble _ _) = liftReal2 fd f (expand_precision x') z
>liftReal2 fd f z@(RDouble _ _) x'@(RLimit _) = liftReal2 fd f z (expand_precision x')

>expand_precision :: R -> R
>expand_precision r = RDouble apprx (r - apprx)
>   where apprx = real_to_double r

>instance Fractional R where
>    (/) = liftReal2 (/) (/)

>real :: (Rational -> Rational) -> R
>real = \f -> RLimit (Endo f)

>real_to_double :: R -> Double
>real_to_double (RLimit e) = Real.real_to_double (Real.Limit e)
>real_to_double (RDouble d _) = d

>instance DedekindCut Double R where
>  r %< (RLimit e) = r %< Real.Limit e
>  r %< (RDouble d r') = r %< d || (r == d && 0 %< r')
>  (RLimit e) <% r = Real.Limit e <% r
>  (RDouble d r') <% r = d <% r || (r == d && 0 <% r')

>approximate :: R -> Rational -> Rational
>approximate (Limit x) r = x `appEndo` r
>approximate (RDouble d rest) r = let
>     k = floatRadix d ^ floatDigits d
>     res | denominator r < k = toRational d
>         | otherwise = toRational d
>                     + approximate rest (r * toRational k) / toRational k
>   in res

>floor_r :: (Integral b) => R -> b
>floor_r r = floor (r `approximate` 1)

>truncate_r :: (Integral b) => R -> b
>truncate_r r = truncate (r `approximate` 1)

>round_r :: (Integral b) => R -> b
>round_r r = round (r `approximate` (1%2))

>ceiling_r :: (Integral b) => R -> b
>ceiling_r r = ceiling (r `approximate` (1%2))

>properFraction_r :: (Integral b) => R -> (b, R)
>properFraction_r r = let res = floor_r r in (res, r - fromIntegral res)
