>{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
>module Math.Number.Test.RealTest where
>import Prelude hiding (take)
>import Test.QuickCheck
>import Test.HUnit
>import Math.Tools.Median
>import Math.Number.Interface
>import Math.Number.Real
>import Math.Number.Stream
>import Math.Test.Common
>import Math.Tools.Test.MedianTest (median_algebra)

>randomReal :: Gen R
>randomReal = do
>   (i :: Integer) <- choose (0,50)
>   (f :: Integer) <- choose (0,10000)
>   return $ fromInteger i + fromInteger f / 10000 

>realInRange :: R -> R -> Gen R
>realInRange a b = do
>   (f :: Integer) <- choose (0,10000)
>   return $ a + (b - a)*fromInteger f / 10000.0


>instance Arbitrary R where
>   arbitrary = randomReal


>prop_equality_reflexive = (forAll randomReal $ \r -> r `approximately_equal` r)
>prop_equality_transitive = (forAll randomReal $ \r ->
>                      forAll randomReal $ \r' ->
>                      forAll randomReal $ \r'' ->
>                      if r `approximately_equal` r' && r' `approximately_equal` r''
>                       then r `approximately_equal` r'' else True)

prop_median_test = med (pi :: R) (3.141 :: R) (3.242 :: R) `approximately_equal` pi

>prop_average_test = average (pi :: R) 0 `approximately_equal` (pi / 2)

>derivatetest1 :: Test
>derivatetest1 = -- True ~? "derivatetest1"
>  (derivate (\(x :: R) -> x*x) 4.0) `approximately_equal` 8.0 ~? "derivatetest1"

>$(return [])
>qcTests = $quickCheckAll

>tests :: Test
>tests = "Math.Number.Real" ~: test [
>   -- median_algebra (const () :: R -> ()),
>   derivatetest1,
>   "properties" ~: qcTests
>   ]
