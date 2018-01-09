>{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
>module Math.Matrix.Test.StreamTest where
>import Test.QuickCheck
>import Test.HUnit
>import Math.Test.Common
>import Math.Number.Stream

>prop_sqrt_squared_naturals :: Property
>prop_sqrt_squared_naturals = property $ (sqrt_stream s * sqrt_stream s - s) < 0.001
>   where s = fmap fromIntegral naturals

>$(return [])
>qcTests = $quickCheckAll

>tests :: Test
>tests = "Math.Number.Stream" ~: test [
>    "properties" ~: qcTests
>  ]
