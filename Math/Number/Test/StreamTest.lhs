>{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
>module Math.Number.Test.StreamTest where
>import Test.QuickCheck
>import Test.HUnit
>import Math.Test.Common
>import qualified Math.Number.Stream as Stream
>import Math.Number.Stream
>import Math.Number.StreamInterface

>prop_sqrt_squared_naturals :: Property
>prop_sqrt_squared_naturals = property $ (sqrtStream s * sqrtStream s - s) < 0.001
>   where s :: Stream Double
>         s = fmap fromIntegral naturals

>prop_primes1 :: Property
>prop_primes1 = property $ Stream.take 10 primes == [2,3,5,7,11,13,17,19,23,29]

>$(return [])
>qcTests = $quickCheckAll

>tests :: Test
>tests = "Math.Number.Stream" ~: test [
>    "properties" ~: qcTests
>  ]
