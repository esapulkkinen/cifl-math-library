>{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
>module Math.Tools.Test.MedianTest where
>import Math.Test.Common
>import Test.QuickCheck
>import Test.HUnit
>import Math.Tools.Median

>median_majority (f :: a -> ()) = forAll arbitrary $ \ (x :: a,y :: a) -> med x x y == x
>median_commutative1 (f :: a -> ()) = forAll arbitrary $ \ (x :: a, y :: a, z :: a) -> med x y z == med x z y
>median_commutative2 (f :: a -> ()) = forAll arbitrary $ \ (x :: a,y :: a,z :: a) -> med x y z == med y z x
>median_commutative3 (f :: a -> ()) = forAll arbitrary $ \ (x :: a,y :: a,z :: a) -> med x y z == med z x y
>median_commutative4 (f :: a -> ()) = forAll arbitrary $ \ (x :: a,y :: a,z :: a) -> med x y z == med z y x
>median_associative (f :: a -> ()) = forAll arbitrary $ \ (x :: a,w :: a,y :: a,z :: a) -> med x w (med y w z) == med (med x w y) w z
>median_distributive (f :: a -> ()) = forAll arbitrary $ \ (x :: a,y :: a,z :: a,u :: a,v :: a) -> med (med x y z) u v == med x (med y u v) (med z u v)

>median_algebra f = "median_algebra" ~: test [
>   median_majority f,median_commutative1 f,
>   median_commutative2 f,median_commutative3 f,
>   median_commutative4 f,median_associative f,
>   median_distributive f]

>$(return [])
>qcTests = $quickCheckAll

>tests :: Test
>tests = "Math.Tools.Median" ~: test [
>   median_algebra (const () :: [Integer] -> ()),
>   median_algebra (const () :: Bool -> ()),
>   median_algebra (const () :: Integer -> ()),
>   median_algebra (const () :: Float -> ()),
>   median_algebra (const () :: Double -> ()),
>   median_algebra (const () :: Int -> ()),
>   median_algebra (const () :: Char -> ()),
>   median_algebra (const () :: Ordering -> ()),
>   median_algebra (const () :: Rational -> ()),
>   median_algebra (const () :: (Integer,Integer) -> ()),
>   median_algebra (const () :: (Integer,Integer,Integer) -> ()),
>   median_algebra (const () :: (Integer,Integer,Integer,Integer) -> ()),
>   median_algebra (const () :: (Integer,Integer,Integer,Integer,Integer) -> ()),
>   "properties" ~: qcTests
>  ]
