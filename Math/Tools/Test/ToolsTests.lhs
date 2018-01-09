>module Math.Tools.Test.ToolsTests where
>import Test.HUnit
>import qualified Math.Tools.Test.MedianTest
>import qualified Math.Tools.Test.AdjunctionTest

>tests = test [ Math.Tools.Test.MedianTest.tests,
>               Math.Tools.Test.AdjunctionTest.tests
>             ]
