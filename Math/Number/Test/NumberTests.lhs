>module Math.Number.Test.NumberTests where
>import Test.HUnit
>import qualified Math.Number.Test.RealTest
>import qualified Math.Number.Test.StreamTest
>import qualified Math.Number.Test.DimensionalAnalysisTest
> 
>tests = test [ Math.Number.Test.RealTest.tests,
>               Math.Number.Test.StreamTest.tests,
>               Math.Number.Test.DimensionalAnalysisTest.tests
>             ]
