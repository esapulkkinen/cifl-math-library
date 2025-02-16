>{-# LANGUAGE TemplateHaskell #-}
>module Math.Number.Test.DimensionalAnalysisTest where
>import Test.QuickCheck
>import Test.HUnit
>import Math.Number.DimensionalAnalysis
>import Math.Matrix.Interface

>propReadMeter :: Property
>propReadMeter = property $ (read "3.3 m" :: Quantity Double) == 3.3 %* meter
>
>propReadMillimeterDimension = property $ valueDimension (read "3.6mm" :: Quantity Double) == meterDimension
>propReadMillimeter_amount = property $ valueAmount (read "3.6mm" :: Quantity Double) - 3.6 < 0.0001
>
>propReadParenthesis = property $
> valueDimension (read "2.4 kgm/(s*s*rad)" :: Quantity Double)
> == kilogramDimension + meterDimension - (2*secondDimension) - radianDimension
>propReadComplex :: Property
>propReadComplex = property $
> valueDimension (read "2.8 kgm/s/s/s" :: Quantity Double)
> == kilogramDimension + meterDimension - 3*secondDimension
>
>$(return [])
>qcTests = $quickCheckAll
>
>tests :: Test
>tests = "Math.Number.DimensionalAnalysis" ~: test [
>     "properties" ~: qcTests
>   ]
