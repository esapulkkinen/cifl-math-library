>{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
>module Math.Number.Test.TrigonometricTest where
>
>import Test.HUnit hiding (assert)
>import Test.QuickCheck
>import Test.QuickCheck.Monadic
>import Math.Number.Trigonometric
>import Data.Complex
>import Data.Ratio
>import Data.Maybe

>randomFractionOfCircle :: Gen Rational
>randomFractionOfCircle = do
>   a <- choose (0,120)
>   return $ (3*a) % 360

>prop_exact_trigonometric_values_have_on_3_degree_intervals = monadicIO $ do
>   q <- pick randomFractionOfCircle
>   v <- run (evalComplexTrigonometric <$> trigCis q)
>   assert (magnitude (v - (cos (2*pi*fromRational q) :+ sin(2*pi*fromRational q))) < (0.001 :: Double))

>prop_exact_trigonometric_values_have_on_17_denominator = monadicIO $ do
>   q' <- pick (choose (0,34))
>   let q = q' % 17
>   v <- run (evalComplexTrigonometric <$> trigCis q)
>   assert (magnitude (v - (cos (2*pi*fromRational q) :+ sin(2*pi*fromRational q))) < (0.001 :: Double))

>$(return [])
>trigTests = $quickCheckAll

>tests :: Test
>tests = "Math.Number.Trigonometric" ~: test [
>   "properties" ~: trigTests
>   ]
