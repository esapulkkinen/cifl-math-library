>module Math.Matrix.Test.MatrixTests where
>import Test.HUnit
>import qualified Math.Matrix.Test.Vector3Test
>import qualified Math.Matrix.Test.InterfaceTest
>import qualified Math.Matrix.Test.StreamTest

>tests = test [
>   Math.Matrix.Test.Vector3Test.tests,
>   Math.Matrix.Test.StreamTest.tests,
>   Math.Matrix.Test.InterfaceTest.tests
> ]
