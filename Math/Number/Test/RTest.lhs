>{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
>module Math.Number.Test.RTest where
>import Math.Number.R
>import Test.QuickCheck
>import Test.HUnit

>infix 4 `approximatelyEqual`
>infix 4 ====

>approximatelyEqual :: R -> R -> Bool
>approximatelyEqual = approximatelyEqualToR 0.0001

>(====) = approximatelyEqual

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

>prop_equality_reflexive = (forAll randomReal $ \r -> r ==== r)
>prop_equality_transitive = (forAll randomReal $ \r ->
>                      forAll randomReal $ \r' ->
>                      forAll randomReal $ \r'' ->
>                      if r ==== r' && r' ==== r''
>                       then r ==== r'' else True)

>prop_average_test = average (pi :: R) 0.0 ==== (pi / 2)

>-- | <https://en.wikipedia.org/wiki/Construction_of_the_real_numbers>
>prop_addition_associative = forAll randomReal $ \r ->
>                            forAll randomReal $ \r' ->
>                            forAll randomReal $ \r'' ->
>                            (r + (r' + r'')) ==== ((r + r') + r'')

>prop_addition_commutative = forAll randomReal $ \a ->
>    forAll randomReal $ \b ->
>    (a + b) ==== (b + a)

>prop_multiplication_commutative = forAll randomReal $ \a ->
>    forAll randomReal $ \b ->
>    (a * b) ==== (b * a)

>prop_multiplication_associative = forAll randomReal $ \r ->
>                            forAll randomReal $ \r' ->
>                            forAll randomReal $ \r'' ->
>                            (r * (r' * r'')) ==== ((r * r') * r'')

>prop_distributativity = forAll randomReal $ \r ->
>                            forAll randomReal $ \r' ->
>                            forAll randomReal $ \r'' ->
>   r * (r' + r'') ==== (r*r') + (r*r'')

>prop_additive_identity = forAll randomReal $ \x -> x + 0 ==== x
>prop_multiplicative_identity = forAll randomReal $ \x -> x * 1 ==== x
>prop_additive_inverse = forAll randomReal $ \x -> x + (negate x) ==== 0
>prop_multiplicative_inverse = forAll randomReal $ \x ->
>   x ==== 0 || x * (1 / x) ==== 1

prop_refl_order = forAll randomReal $ \x -> x <= x

>derivatetest1 :: Test
>derivatetest1 = -- True ~? "derivatetest1"
>  ((derivateR (\(x :: R) -> x*x) 4.0) ==== 8.0) ~? "derivatetest1"

>$(return [])
>qcTests = $quickCheckAll

>tests :: Test
>tests = "Math.Number.R" ~: test [
>   -- median_algebra (const () :: R -> ()),
>   derivatetest1,
>   "properties" ~: qcTests
>   ]
