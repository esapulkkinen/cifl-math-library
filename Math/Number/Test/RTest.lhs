>{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
>module Math.Number.Test.RTest where
>import Math.Number.R
>import Test.QuickCheck
>import Test.HUnit

>approximately_equal :: R -> R -> Bool
>approximately_equal = approximately_equal_to_r 0.0001

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

>prop_average_test = average (pi :: R) 0.0 `approximately_equal` (pi / 2)

>derivatetest1 :: Test
>derivatetest1 = -- True ~? "derivatetest1"
>  ((derivate_r (\(x :: R) -> x*x) 4.0) `approximately_equal` 8.0) ~? "derivatetest1"

>$(return [])
>qcTests = $quickCheckAll

>tests :: Test
>tests = "Math.Number.R" ~: test [
>   -- median_algebra (const () :: R -> ()),
>   derivatetest1,
>   "properties" ~: qcTests
>   ]
