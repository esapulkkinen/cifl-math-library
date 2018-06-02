>{-# LANGUAGE TemplateHaskell #-}
>module Math.Number.Test.DimensionalAnalysisTest where
>import Test.QuickCheck
>import Test.HUnit
>import Math.Number.DimensionalAnalysis
>import Math.Matrix.Interface

>prop_read_meter :: Property
>prop_read_meter = property $ (read "3.3 m" :: Quantity Double) == 3.3 %* meter
>
>prop_read_millimeter_dimension = property $ value_dimension (read "3.6mm" :: Quantity Double) == meter_dimension
>prop_read_millimeter_amount = property $ value_amount (read "3.6mm" :: Quantity Double) - 3.6 < 0.0001
>
>prop_read_parenthesis = property $
> value_dimension (read "2.4 kgm/(s*s*rad)" :: Quantity Double)
> == kilogram_dimension + meter_dimension - (2*second_dimension) - radian_dimension
>prop_read_complex :: Property
>prop_read_complex = property $
> value_dimension (read "2.8 kgm/s/s/s" :: Quantity Double)
> == kilogram_dimension + meter_dimension - 3*second_dimension
>
>$(return [])
>qcTests = $quickCheckAll
>
>tests :: Test
>tests = "Math.Number.DimensionalAnalysis" ~: test [
>     "properties" ~: qcTests
>   ]
